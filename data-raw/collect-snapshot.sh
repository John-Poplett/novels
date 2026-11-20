#!/bin/sh
# Nightly collector for the download-snapshot series (OPERATIONS.md).
#
#   collect-snapshot.sh [--pull-from SRC]... [--copy-to DEST]... [--offsite REMOTE]...
#                       [--move-payloads-to DEST] [--offsite-payloads REMOTE]
#
# 1. pull: copy snapshots another collector took into this checkout
#    (so this machine's snapshotDue() sees them);
# 2. collect: run data-raw/snapshot-downloads.R, which exits before
#    downloading anything unless a snapshot is due;
# 3. copy: send this checkout's snapshots to each destination;
# 4. offsite: upload them to each rclone REMOTE (e.g.
#    "b2snap:gutenberg-download-counts/snapshots/") with --immutable, so an
#    upload can add files but never replace one. The rclone binary is $RCLONE
#    (default: rclone on PATH) -- the Pi's Debian rclone is too old for B2;
# 5. payloads (F4): snapshot-downloads.R keeps the raw RDF payload of each
#    snapshot date in data-raw/payloads/<date>/, outside the git-tracked
#    snapshots. --move-payloads-to (the Mac) sends them to DEST with rsync
#    and deletes the local copy once sent. --offsite-payloads (the Pi)
#    checks each payload's SHA-256 against its snapshot's provenance.txt,
#    then moves it to REMOTE<date>/ with rclone moveto --immutable: B2 is
#    its one long-term home. A payload that fails the check stays put and is
#    logged. Each of the two options may be given once, because a move
#    leaves nothing for a second destination.
#
# SRC and DEST are complete rsync locations, because each peer's key differs:
# the Pi's key for gpu-box is rrsync-restricted to the snapshot directory
# (DEST is "gpu-box:"), the Mac's is not (DEST is
# "gpu-box:MachineLearning/novels/data-raw/snapshots/"). A SRC must name the
# directory's contents -- a trailing "/", or "host:" for an rrsync-restricted
# key -- or rsync nests a second snapshots/ inside this one.
#
# Every copy is append-only: --ignore-existing or --immutable, never --delete. Every step
# runs even when an earlier one failed -- a failed pull must not stop the
# collection, and a failed collection must not stop the copies -- and every
# failure writes one dated line, so the log (one line a night minimum) is the
# heartbeat. The exit status is 0 only when every step succeeded.
#
# Cron has PATH=/usr/bin:/bin, so the locations Rscript lives in on the Mac
# (/usr/local/bin, /opt/homebrew/bin) are appended -- appended, so a caller's
# PATH still wins (the tests put stand-ins for rsync and Rscript first).

PATH=$PATH:/usr/local/bin:/opt/homebrew/bin
export PATH

# A peer may be switched off (gpu-box often is, to save power): give up on
# connecting after 30 s, on a stalled transfer after 5 min, and never wait for
# a password, so one absent host cannot hang the night or block later steps.
# The nightly --ignore-existing copy of the whole directory catches it up
# when it is back.
copy() {
  rsync -a --ignore-existing --timeout=300 \
    -e "ssh -o BatchMode=yes -o ConnectTimeout=30 -o ServerAliveInterval=30 -o ServerAliveCountMax=4" \
    "$@"
}

sha256() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1"; else shasum -a 256 "$1"; fi |
    cut -d' ' -f1
}

stamp() { date -u +%Y-%m-%dT%H:%M:%SZ; }
log() { echo "$(stamp) collect-snapshot: $*"; }

usage() {
  echo "usage: collect-snapshot.sh [--pull-from SRC]... [--copy-to DEST]... [--offsite REMOTE]... [--move-payloads-to DEST] [--offsite-payloads REMOTE]" >&2
  exit 2
}

pulls=""
copies=""
offsites=""
payloads_to=""
payloads_offsite=""
while [ $# -gt 0 ]; do
  case "$1" in
    --pull-from) [ $# -ge 2 ] && [ -n "$2" ] || usage; pulls="$pulls
$2"; shift 2 ;;
    --copy-to) [ $# -ge 2 ] && [ -n "$2" ] || usage; copies="$copies
$2"; shift 2 ;;
    --offsite) [ $# -ge 2 ] && [ -n "$2" ] || usage; offsites="$offsites
$2"; shift 2 ;;
    --move-payloads-to) [ $# -ge 2 ] && [ -n "$2" ] && [ -z "$payloads_to" ] || usage
      payloads_to=$2; shift 2 ;;
    --offsite-payloads) [ $# -ge 2 ] && [ -n "$2" ] && [ -z "$payloads_offsite" ] || usage
      payloads_offsite=$2; shift 2 ;;
    *) log "unknown argument '$1'"; usage ;;
  esac
done

# The checkout is the directory above this script, wherever it was called from.
root=$(cd "$(dirname "$0")/.." 2>/dev/null && pwd)
if [ -z "$root" ] || [ ! -f "$root/data-raw/snapshot-downloads.R" ]; then
  log "FAILED: cannot find the checkout above $0; nothing was run"
  exit 1
fi
cd "$root" || exit 1
snapshots="data-raw/snapshots/"
payloads="data-raw/payloads/"
mkdir -p "$snapshots" "$payloads"

status=0
log "start on $(hostname)"

# Newlines separate locations, so a location may not contain one; spaces are fine.
old_ifs=$IFS
IFS='
'
for src in $pulls; do
  if copy "$src" "$snapshots"; then
    log "pulled from $src"
  else
    log "FAILED: pull from $src"; status=1
  fi
done
IFS=$old_ifs

if TZ=UTC Rscript data-raw/snapshot-downloads.R; then
  log "collect ok"
else
  log "FAILED: data-raw/snapshot-downloads.R exited $?"; status=1
fi

IFS='
'
for dest in $copies; do
  if copy "$snapshots" "$dest"; then
    log "copied to $dest"
  else
    log "FAILED: copy to $dest"; status=1
  fi
done
for remote in $offsites; do
  if "${RCLONE:-rclone}" copy --immutable "$snapshots" "$remote"; then
    log "uploaded to $remote"
  else
    log "FAILED: upload to $remote"; status=1
  fi
done
IFS=$old_ifs

if [ -n "$payloads_to" ]; then
  if copy --remove-source-files --exclude '*.partial' "$payloads" "$payloads_to"; then
    log "payloads moved to $payloads_to"
  else
    log "FAILED: payload move to $payloads_to (the local copies stay; next night retries)"; status=1
  fi
fi

if [ -n "$payloads_offsite" ]; then
  for file in "$payloads"*/rdf-files.tar.bz2; do
    [ -f "$file" ] || continue
    date=$(basename "$(dirname "$file")")
    expected=$(sed -n 's/^payload_sha256: //p' "$snapshots$date/provenance.txt" 2>/dev/null)
    actual=$(sha256 "$file")
    if [ -z "$expected" ]; then
      log "FAILED: payload $date has no payload_sha256 in $snapshots$date/provenance.txt; kept, not uploaded"; status=1
    elif [ "$actual" != "$expected" ]; then
      log "FAILED: payload $date hashes to $actual, provenance says $expected; kept, not uploaded"; status=1
    elif "${RCLONE:-rclone}" moveto --immutable "$file" "$payloads_offsite$date/rdf-files.tar.bz2"; then
      rmdir "$payloads$date" 2>/dev/null
      log "payload $date moved to $payloads_offsite$date/"
    else
      log "FAILED: payload $date upload to $payloads_offsite (kept; next night retries)"; status=1
    fi
  done
fi

log "done, status $status"
exit $status
