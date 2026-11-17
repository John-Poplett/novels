# Operations: collecting the download series

How the 30-day download snapshots are collected, copied and kept. The
*what* and *why* of the series live in `SCHEMA.md` and `PREREGISTRATION.md`
§11; this file is the *how*, machine by machine. Written 2026-11-17.

The series is pre-registered with no backfill: a snapshot that is missed,
lost, or silently wrong cannot be recovered. Everything below exists to
make each of those three failures either impossible or visible.

## Shape

| Machine | Role | Runs at (America/Chicago) |
|---|---|---|
| Mac (laptop) | primary collector | 22:00 (04:00 UTC CST / 03:00 UTC CDT) |
| `ts-appliance` (Pi 5, always on) | fallback collector, off-site uploader | 01:00 (07:00 / 06:00 UTC) |
| `gpu-box` (workstation) | local copy, git record | receives only |
| Backblaze B2 | off-site copy, append-only | receives only |

Each night:

```
Mac  22:00  pull new snapshots from the Pi → collect if due → copy to gpu-box + Pi
Pi   01:00  collect if due → copy to gpu-box → upload to B2
```

Both runs fall on the same UTC date. The Pi's run sees the Mac's snapshot
(copied three hours earlier), so `snapshotDue()` returns `FALSE` and it
exits before downloading. It collects only when the Mac did not. The Mac's
next run first pulls whatever the Pi took, so the two never diverge.

Every copy is append-only: `rsync --ignore-existing` without `--delete`,
`rclone copy --immutable`, and a B2 key that cannot delete. Nothing is ever
overwritten or removed downstream of the machine that collected it.

Git is the published record, not the backup: snapshot directories are
committed on `gpu-box` deliberately (monthly or at milestones), never by
the collectors. Collectors never commit (AGENTS.md).

## Status

| Step | What | Who | State |
|---|---|---|---|
| A1 | `collector:` line in `provenance.txt`; refuse a payload that is not a plausible bzip2 archive | Claude | in progress |
| A2 | `data-raw/collect-snapshot.sh`: pull, collect, copy, upload, log | Claude | planned |
| A3 | keep the raw payload on snapshot dates (F4) | Claude | planned |
| B4 | apt install on the Pi (needs sudo) | John | **open — blocks B5–B8** |
| B5–B8 | CRAN packages, clone, smoke test, rehearsal on the Pi | Claude | blocked on B4 |
| C9 | Pi → gpu-box ssh key | Claude | planned |
| C10 | Mac → Pi ssh key and config | John | open |
| D11 | Mac: reset to `origin/dev`, new crontab line | John | after A2 |
| D12 | Pi crontab | Claude | after A2, B8 |
| F1 | B2 bucket and bucket-scoped key | John | open |
| F2–F3 | rclone on the Pi, upload wiring | Claude | after F1, A2 |

Deadline: all of it in place, and failover tested, before **2026-12-07**,
the first real grid date.

## A. Code (on `feat/` branches, through `dev`)

Both collectors run the same code, so code lands first.

1. `provenance.txt` gains `collector: <hostname>`, so a snapshot says which
   machine took it. The script checks the download is a bzip2 archive of
   plausible size before unpacking. (An outage is already caught earlier:
   the feed answers `503` and `download.file()` stops.)
2. `data-raw/collect-snapshot.sh` replaces the crontab one-liner. Hosts are
   arguments, so the Mac and the Pi share it. Every step that can fail
   writes a dated line to the log, and a failed `cd` stops the script
   instead of running the rest in the wrong directory.
3. Raw payload (F4, below).

## B. Install on the Pi

Measured 2026-11-17: aarch64, Debian 13 (trixie), 4 GB RAM, 215 GB free on
NVMe, cron active, no unattended upgrades. **`/tmp` is a 2 GB tmpfs (RAM)**
and the extracted feed is 2.1 GB, so `TMPDIR` must point at the NVMe.

4. **[John]** System packages and Debian's R packages:

   ```bash
   sudo apt install git rsync rclone r-base-core r-base-dev gfortran \
     libarchive-dev libcurl4-openssl-dev libssl-dev libxml2-dev \
     r-cran-{pkgload,digest,readr,dplyr,glmnet,magrittr,proc,purrr,stringi,stringr,tibble,tidyr,tm,testthat,data.table,rcpp,matrix,r6}
   ```

5. **[Claude]** The four packages Debian lacks, from CRAN source (expect tens
   of minutes; `text2vec` is the slow one). `load_all()` needs every
   package in `Imports`, not only the ones the snapshot uses.

   ```bash
   mkdir -p "$(Rscript -e 'cat(Sys.getenv("R_LIBS_USER"))')"
   Rscript -e 'install.packages(c("archive","gutenbergr","targets","text2vec"),
                                repos = "https://cloud.r-project.org", Ncpus = 4)'
   ```

6. **[Claude]** Clone (public repo; the Pi never pushes):

   ```bash
   git clone -b dev https://github.com/John-Poplett/novels ~/MachineLearning/novels
   mkdir -p ~/.cache/novels-tmp ~/.local/state
   ```

7. **[Claude]** Smoke test — expect "No snapshot due":

   ```bash
   cd ~/MachineLearning/novels
   TZ=UTC Rscript data-raw/snapshot-downloads.R
   Rscript -e 'pkgload::load_all(); testthat::test_file("tests/testthat/test-downloadSnapshots.R")'
   ```

8. **[Claude]** Rehearsal: a full download and parse in a throwaway clone
   with an empty series, so it writes a "first" snapshot that never touches
   the real one. Record run time, peak `TMPDIR` and peak memory, then
   delete the clone.

   ```bash
   git clone -q ~/MachineLearning/novels ~/rehearsal && rm -rf ~/rehearsal/data-raw/snapshots/*
   cd ~/rehearsal && TMPDIR=~/.cache/novels-tmp TZ=UTC /usr/bin/time -v Rscript data-raw/snapshot-downloads.R
   rm -rf ~/rehearsal
   ```

## C. SSH keys

Cron has no agent or keychain, so each key is passphrase-less and used for
nothing else.

9. **[Claude]** On the Pi: `ssh-keygen -t ed25519 -N '' -f ~/.ssh/snapshot_ed25519`,
   public half into `gpu-box:~/.ssh/authorized_keys`, a `Host gpu-box`
   block using it. Test: `ssh -o BatchMode=yes gpu-box true`.
10. **[John]** On the Mac: the existing cron key (the one used for gpu-box)
    into the Pi's `~/.ssh/authorized_keys`, and a `Host ts-appliance` block
    (Tailscale name `appliance`, or LAN `192.168.2.10`). Test:
    `ssh -o BatchMode=yes ts-appliance true`.

## D. Cron

11. **[John]** Mac: `git fetch --prune && git reset --hard origin/dev`, then

    ```
    0 22 * * * /Users/john/MachineLearning/novels/data-raw/collect-snapshot.sh --pull-from ts-appliance --copy-to "gpu-box ts-appliance" >> /Users/john/Library/Logs/novels-snapshot.log 2>&1
    ```

12. **[Claude]** Pi:

    ```
    0 1 * * * TMPDIR=/home/john/.cache/novels-tmp /home/john/MachineLearning/novels/data-raw/collect-snapshot.sh --copy-to gpu-box --offsite b2:<bucket> >> /home/john/.local/state/novels-snapshot.log 2>&1
    ```

The script sets `TZ=UTC` for R, as the current crontab does. Exact flags
are fixed when A2 lands; this section is updated with it.

## F. Off-site: Backblaze B2

A bucket of its own, not the restic repository: restic needs delete
permission for its lock files, stores data opaquely, and would put the
laptop-backup credentials on the Pi. Plain files can be fetched and checked
against `provenance.txt` without any tool.

1. **[John]** Create a private bucket. Enable Object Lock at creation with
   a bucket-wide default retention (e.g., three years: the wave ends
   2028-12-31 and the analysis follows), so no per-file setting is needed. Create an application key restricted to that bucket
   with `listBuckets`, `listFiles`, `readFiles`, `writeFiles` — **no
   `deleteFiles`**. Optionally set a spending cap under Caps & Alerts.
   Hand the key ID and key to Claude out of band (not in the repo).
2. **[Claude]** `rclone config` on the Pi with that key; a dry run, then one
   real upload of `2026-11-07`, and hash comparison against gpu-box.
3. **[Claude]** Wire `--offsite` into `collect-snapshot.sh`: `rclone copy
   --immutable`, a log line on failure.
4. **[Claude]** Raw payload (A3): keep `rdf-files.tar.bz2` on snapshot dates
   only — ~12 a year, never nightly, so Project Gutenberg serves nothing
   extra. The collector stores it beside the snapshot, checks its SHA-256
   against `provenance.txt`, and the payload travels like the snapshot:
   Mac → Pi with `rsync --remove-source-files` (the Mac's copy goes once the
   Pi has it), Pi → B2 with `rclone copy --immutable` (B2 verifies the
   upload's SHA-1), then the Pi deletes its local copy. The payload is
   catalogue metadata only — about 80,000 RDF records, no book texts — and
   lets every count be re-derived after a parser bug, and catalogue drift
   (re-shelved or re-typed books) be dated.

Cost, checked 2026-11-17: B2 storage is about $6.95/TB-month, uploads and
API calls are free at this scale, downloads free up to 3× storage. The
series reaches ~4.5 GB by 2029 (≈1.5 GB/year of payloads, ~7 MB/year of
snapshots): about three cents a month on an account already past the free
10 GB.

## E. Upkeep

- **Code updates on a collector:** `git fetch --prune && git reset --hard
  origin/dev`, never `git pull`. Rebase-merges rewrite SHAs, and `pull`
  refuses when copied-in snapshot directories sit where git later tracks
  them; `reset --hard` overwrites them with identical bytes.
- **Kernel reboots on the Pi:** not between 00:30 and 02:00 Central on a
  grid date. A missed Pi night means the Mac covered it, or the next night
  takes a late snapshot.
- **Before 2026-12-07:** pause the Mac's cron for one non-grid night and
  confirm the Pi logs "No snapshot due"; confirm both logs have a line
  every night.
- **2026-12-08:** `sha256sum data-raw/snapshots/2026-12-07/*` matches on all
  three machines, and `collector:` names the Mac.

### Known gap

If the Mac collects but its copy to the Pi fails while the Pi is up, both
take a snapshot of the same date, three hours apart, and each keeps its
own. Rare; the `collector:` line and the hash comparison make it visible. A
nightly comparison can be added if it ever happens.
