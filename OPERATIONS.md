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
| A1 | `collector:` line in `provenance.txt`; refuse a payload that is not a plausible bzip2 archive | Claude | done 2026-11-17 |
| A2 | `data-raw/collect-snapshot.sh`: pull, collect, copy, log (upload with F3) | Claude | done 2026-11-19; real run on the Pi green |
| A3 | keep the raw payload on snapshot dates (F4) | Claude | done 2026-11-19; both crontabs carry it |
| A4 | refuse a feed that is down or more than 48 h old (`Last-Modified`) | Claude | done 2026-11-19 |
| B4 | apt install on the Pi (needs sudo) | John | done 2026-11-18 |
| B5–B8 | clone, packages from `renv.lock`, smoke test, rehearsal on the Pi | Claude | done 2026-11-19: 88 packages match `renv.lock`; smoke test green; rehearsal green (below) |
| C9 | Pi → gpu-box ssh key, write-only via `rrsync` | Claude | done 2026-11-18 |
| C10 | Mac → Pi ssh key and config | John | done 2026-11-19: unattended (`BatchMode`, no agent) ssh to the Pi and gpu-box both ok |
| D11 | Mac: reset to `origin/dev`, new crontab line | John | done 2026-11-19 (Mac at `49c14e0`; crontab with `RSYNC=` and `--move-payloads-to`) |
| D12 | Pi crontab | Claude | live 2026-11-19 (01:00 CT): copy to gpu-box, upload snapshots and payloads to B2 |
| F1 | B2 bucket and bucket-scoped key | John | done 2026-11-19: bucket `gutenberg-download-counts`, Object Lock on; key `writeFiles,listFiles,readFiles` (no `deleteFiles`); the first key was pasted in chat and replaced |
| F2 | rclone on the Pi, probe, first upload | Claude | done 2026-11-19 (below) |
| F3 | `--offsite` upload wiring | Claude | done 2026-11-19; Pi crontab uploads nightly |

Deadline: all of it in place, and failover tested, before **2026-12-07**,
the first real grid date.

## A. Code (on `feat/` branches, through `dev`)

Both collectors run the same code, so code lands first.

1. `provenance.txt` gains `collector: <hostname>`, so a snapshot says which
   machine took it. The script checks the download is a bzip2 archive of
   plausible size before unpacking.
2. `data-raw/collect-snapshot.sh` replaces the crontab one-liner. Hosts are
   arguments, so the Mac and the Pi share it. Every step that can fail
   writes a dated line to the log, and a failed `cd` stops the script
   instead of running the rest in the wrong directory.
3. Raw payload (F4, below).
4. Before downloading, a HEAD request reads the feed's `Last-Modified`.
   The script refuses a feed that answers anything but `200`, sends no
   usable `Last-Modified`, or was regenerated more than **48 hours** ago,
   and records `feed_last_modified_utc:` in `provenance.txt`. A stale feed
   would record an earlier 30-day window under today's date, a silently
   wrong observation. A refused night is only a late one: the next nightly
   run retries, and the snapshot keeps its own date.

## B. Install on the Pi

Measured 2026-11-17: aarch64, Debian 13 (trixie), 4 GB RAM, 215 GB free on
NVMe, cron active, no unattended upgrades. **`/tmp` is a 2 GB tmpfs (RAM)**
and the extracted feed is 2.1 GB, so `TMPDIR` must point at the NVMe.

4. **[John]** System packages and Debian's R packages:

   ```bash
   sudo apt install git rsync rclone r-base-core r-base-dev gfortran \
     libarchive-dev libcurl4-openssl-dev libssl-dev libxml2-dev libuv1-dev cmake \
     r-cran-{pkgload,digest,readr,dplyr,glmnet,magrittr,proc,purrr,stringi,stringr,tibble,tidyr,tm,testthat,data.table,rcpp,matrix,r6}
   ```

5. **[Claude]** Packages, at the versions in `renv.lock` (the ones gpu-box
   last checked green), not whatever CRAN serves on the day. Only the
   runtime set and the test runner: a collector needs neither the
   vignette packages nor the toolchain, whose system libraries the Pi lacks.
   `load_all()` needs every package in `Imports`. renv's up-front list of
   missing system packages covers the whole lockfile; for this subset only
   `libuv1-dev` and `cmake` (for `fs`) matter, and step 4 installs them. Expect tens of minutes on
   ARM (everything builds from source; `text2vec` is the slow one).

   **Build serially.** On 2026-11-18 a parallel build (4 packages at once,
   `make -j4` each, during RcppArmadillo/RcppEigen) rebooted the Pi: 4 GB
   of RAM, 2 GB of zram, other services running. renv's install concurrency
   is `RENV_CONFIG_INSTALL_JOBS`, not `Ncpus`. Run detached, so a dropped
   ssh session does not stop it:

   ```bash
   export RENV_CONFIG_INSTALL_JOBS=1 MAKEFLAGS=-j1   # one package, one compiler
   export RENV_CONFIG_INSTALL_TRANSACTIONAL=FALSE    # keep what built if one fails
   ulimit -v 2500000                                 # ~2.4 GB per process
   mkdir -p "$(Rscript -e 'cat(Sys.getenv("R_LIBS_USER"))')"
   Rscript -e 'install.packages("renv", repos = "https://cloud.r-project.org")'
   cd ~/MachineLearning/novels
   Rscript -e 'd <- read.dcf("DESCRIPTION")[1, "Imports"]
     imports <- trimws(sub("[(].*$", "", strsplit(d, ",")[[1]]))
     renv::restore(lockfile = "renv.lock", packages = c(imports, "pkgload", "testthat"),
                   library = Sys.getenv("R_LIBS_USER"), prompt = FALSE)'
   ```

   Re-run after every lockfile change that lands on `dev`, timed so
   collectors upgrade just after a grid date, never just before.

6. **[Claude]** Clone (public repo; the Pi never pushes). Do this before step 5,
   which reads `renv.lock` from the checkout:

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

**Rehearsal, 2026-11-19** (from `gutenberg.pglaf.org`: www.gutenberg.org
was answering 503, and the PGLAF host serves a copy frozen at 2026-11-15):
79,828 records, 29.5 min end to end — almost all of it the download at
~95 KB/s — peak `TMPDIR` **2,602 MB** (it would have overflowed the 2 GB
tmpfs `/tmp`), peak R RSS 419 MB, at least 1,721 MB of RAM left free. The
first attempt failed after 60 s: R's default `download.file()` timeout
covers the whole transfer. Fixed in `downloadRdfPayload()` (one hour; since renamed `downloadWithTimeout()` and used for the corpus archive too).

## C. SSH keys

Cron has no agent or keychain, so each key is passphrase-less and used for
nothing else.

9. **[Claude]** On the Pi: `ssh-keygen -t ed25519 -N '' -f ~/.ssh/snapshot_ed25519`,
   public half into `gpu-box:~/.ssh/authorized_keys` **restricted to
   write-only rsync into the snapshot directory**:

   ```
   restrict,command="/usr/bin/rrsync -wo /home/john/MachineLearning/novels/data-raw/snapshots" ssh-ed25519 AAAA… snapshot-collector@appliance
   ```

   so a passphrase-less key on an always-on box can add snapshots to
   gpu-box and do nothing else: no shell, no reads, no other path. The
   remote path is relative to that directory (`gpu-box:`). gpu-box's host
   key is put in the Pi's `known_hosts` from `/etc/ssh/ssh_host_ed25519_key.pub`,
   not accepted on first use. A `Host gpu-box` block (MagicDNS name,
   `IdentitiesOnly yes`) uses the key. Test — a dry-run copy succeeds, a
   shell is refused:

   ```bash
   rsync -a --ignore-existing --dry-run -v data-raw/snapshots/ gpu-box:
   ssh -o BatchMode=yes gpu-box true   # "rrsync error: SSH_ORIGINAL_COMMAND does not run rsync"
   ```
10. **[John]** On the Mac: the existing cron key (the one used for gpu-box)
    into the Pi's `~/.ssh/authorized_keys`, and a `Host ts-appliance` block
    (Tailscale name `appliance`, or LAN `192.168.2.10`). Test:
    `ssh -o BatchMode=yes ts-appliance true`.

## D. Cron

11. **[John]** Mac: `git fetch --prune && git reset --hard origin/dev`, then

    ```
    0 22 * * * RSYNC=/opt/homebrew/bin/rsync /Users/john/MachineLearning/novels/data-raw/collect-snapshot.sh --pull-from ts-appliance:MachineLearning/novels/data-raw/snapshots/ --copy-to gpu-box:MachineLearning/novels/data-raw/snapshots/ --copy-to ts-appliance:MachineLearning/novels/data-raw/snapshots/ --move-payloads-to ts-appliance:MachineLearning/novels/data-raw/payloads/ >> /Users/john/Library/Logs/novels-snapshot.log 2>&1
    ```

12. **[Claude]** Pi:

    ```
    0 1 * * * TMPDIR=/home/john/.cache/novels-tmp RCLONE=/home/john/.local/bin/rclone /home/john/MachineLearning/novels/data-raw/collect-snapshot.sh --copy-to gpu-box: --offsite b2snap:gutenberg-download-counts/snapshots/ --offsite-payloads b2snap:gutenberg-download-counts/raw/ >> /home/john/.local/state/novels-snapshot.log 2>&1
    ```

Each location is a complete rsync path, because the keys differ: the Pi's
key for gpu-box is rrsync-restricted to the snapshot directory (`gpu-box:`),
the Mac's is not. A pull source must end in `/` (or be `host:` for an
rrsync key), or rsync nests a second `snapshots/`. The script sets
`TZ=UTC` for R, appends `/usr/local/bin` and `/opt/homebrew/bin` to cron's
`PATH` (appended, so on the Mac cron would still find Apple's
`/usr/bin/rsync` first: hence `RSYNC=` naming Homebrew's 3.5.2, tested
2026-11-19 for `--remove-source-files` and `--timeout`), runs every step
even when an earlier one failed, and writes a dated
`FAILED:` line for each failure. Only the Pi uploads off-site
(`--offsite`, last, `rclone copy --immutable`): it holds the B2 key, and it
has every snapshot, the Mac's included, by 01:00. `RCLONE` names the
v1.76.0 binary, since Debian's is too old for B2 (F2).

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
   Then, on the Pi, `rclone config create b2snap b2 account <keyID> key
   <applicationKey>` and `chmod 600 ~/.config/rclone/rclone.conf`. **The
   key goes nowhere else** — not to Claude, not to chat, not to the repo.
2. **[Claude]** On the Pi: a probe upload and a refused delete, a dry run,
   then one real upload of `2026-11-07`, and hash comparison against
   gpu-box.

   Done 2026-11-19. Debian's `rclone` 1.60.1 cannot log in (B2 refuses its
   v1 `authorize_account`: "not currently supported on API version number
   1"), so the Pi runs the official **v1.76.0** binary at
   `~/.local/bin/rclone`, checksum-verified against both rclone.org's and
   GitHub's `SHA256SUMS`; call it by full path, since cron's `PATH` lacks
   `~/.local/bin`. Probe `_probe/probe-2026-11-19.txt` uploaded, and
   `deletefile --b2-hard-delete` was refused (`401 unauthorized`); it
   stays. `snapshots/2026-11-07/` uploaded with `copy --immutable`:
   `rclone check` 0 differences, and a downloaded copy's SHA-256s match
   gpu-box's for all three files. Bucket layout: `snapshots/<date>/`,
   later `raw/<date>/` for F4.
3. **[Claude]** Wire `--offsite` into `collect-snapshot.sh`: `rclone copy
   --immutable`, a log line on failure. Done 2026-11-19 (crontab in D12).
4. **[Claude]** Raw payload (A3): keep `rdf-files.tar.bz2` on snapshot dates
   only — ~12 a year, never nightly, so Project Gutenberg serves nothing
   extra. The collector stores it in `data-raw/payloads/<date>/` (git-ignored,
   and not copied to gpu-box: 127 MB a date has no business in either),
   checks its SHA-256 against `provenance.txt`, and the payload travels:
   Mac → Pi with `rsync --remove-source-files` (the Mac's copy goes once the
   Pi has it; `--move-payloads-to`), and Pi → B2 `raw/<date>/` with
   `rclone moveto --immutable` after the Pi re-checks the SHA-256 against
   `provenance.txt` (`--offsite-payloads`; rclone verifies the upload's
   SHA-1 and deletes the local copy only then). A payload that fails a
   check stays where it is, with a `FAILED:` line, and is retried nightly.
   B2 is the payload's one long-term copy, under Object Lock. The payload
   is catalogue metadata only — about 80,000 RDF records, no book texts —
   and lets every count be re-derived after a parser bug, and catalogue
   drift (re-shelved or re-typed books) be dated.

   Done 2026-11-19. End to end on the Pi with the real rsync and rclone (a
   3 MB stand-in, dated 2099-01-01, to `_probe/raw/`): the rsync source was
   removed, the hash check passed, the B2 copy's SHA-256 matched, and no
   local copy remained. The first real payload will be 2026-12-07's.

Cost, checked 2026-11-17: B2 storage is about $6.95/TB-month, uploads and
API calls are free at this scale, downloads free up to 3× storage. The
series reaches ~4.5 GB by 2029 (≈1.5 GB/year of payloads, ~7 MB/year of
snapshots): about three cents a month on an account already past the free
10 GB.

## G. Catalogue freeze (2026-12-31)

Eligibility is fixed once from Project Gutenberg's `pg_catalog.csv`
(PREREGISTRATION.md section 3). The 2026-11-20 amendment (section 11) allows
an outage: the catalogue counts if Project Gutenberg regenerated it on or
after 2026-12-31 (its `Last-Modified`) and it is retrieved by **2027-01-31**.
No mirror, ever. `data-raw/freeze-catalogue.R` enforces all of it.

1. **[Claude]** Rehearsal, any day before: `Rscript data-raw/freeze-catalogue.R
   --dry-run`, also with `--source https://gutenberg.pglaf.org/cache/epub/feeds/pg_catalog.csv.gz`
   to rehearse the parsing while the main site is down. Writes nothing.
   Done 2026-11-20: the mirror's copy (regenerated 2026-11-08) parsed
   cleanly, 79,769 rows, 61,977 English texts, and its timing was refused;
   the official URL and a real run with `--source` were refused too.
2. **[Claude]** On 2026-12-31 or the first day after it when the site is up
   (on gpu-box or the Mac; a few seconds, 5.6 MB):
   `Rscript data-raw/freeze-catalogue.R`. It refuses a catalogue
   regenerated before 2026-12-31 (if it is the morning of the 31st, run it
   again later that day), and refuses to run twice.
3. **[Claude]** Commit `data-raw/catalogue-freeze/` on a `feat/` branch;
   record its `payload_sha256` in PREREGISTRATION.md section 13; copy it to
   B2: from the Pi, `~/.local/bin/rclone copy --immutable
   data-raw/catalogue-freeze/ b2snap:gutenberg-download-counts/catalogue-freeze/`
   after the commit reaches the Pi.
4. If it cannot be done by 2027-01-31: stop. The wave's start is revisited
   and amended before any window data are used.

## E. Upkeep

- **Code updates on a collector:** `git fetch --prune && git reset --hard
  origin/dev`, never `git pull`. Rebase-merges rewrite SHAs, and `pull`
  refuses when copied-in snapshot directories sit where git later tracks
  them; `reset --hard` overwrites them with identical bytes.
- **Feed down or stale** (the log shows `HTTP 503` or `hours ago (limit
  48)`): nothing to do for a few nights; each night retries. If it lasts
  past a grid date, the late snapshot is still valid, and its interval is
  recorded. Do **not** point the script at a mirror (`gutenberg.pglaf.org`
  served a copy frozen at 2026-11-15 throughout the November outage): a
  different source is a pre-registration amendment, decided and written
  down first.
- **gpu-box switched off** (it draws the most power, so this is allowed
  at any time, grid dates included): nothing depends on it at collection
  time. Each collector logs `FAILED: copy to gpu-box:…` after a 30 s
  connect timeout (rsync also gives up on a transfer stalled for 5 min) and
  carries on, so the Pi's B2 upload still runs. That line is expected on
  off nights, not an alarm. The first night gpu-box is back, the
  `--ignore-existing` copy of the whole directory delivers everything it
  missed. Committing a new snapshot to git can be done from the Mac's
  checkout instead.
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
