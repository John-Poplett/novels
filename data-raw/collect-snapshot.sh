#!/bin/sh
# Nightly collector for the download-snapshot series (OPERATIONS.md).
#
#   collect-snapshot.sh [--pull-from SRC]... [--copy-to DEST]...
#
# 1. pull: copy snapshots another collector took into this checkout
#    (so this machine's snapshotDue() sees them);
# 2. collect: run data-raw/snapshot-downloads.R, which exits before
#    downloading anything unless a snapshot is due;
# 3. copy: send this checkout's snapshots to each destination.
#
# SRC and DEST are complete rsync locations, because each peer's key differs:
# the Pi's key for gpu-box is rrsync-restricted to the snapshot directory
# (DEST is "gpu-box:"), the Mac's is not (DEST is
# "gpu-box:MachineLearning/novels/data-raw/snapshots/"). A SRC must name the
# directory's contents -- a trailing "/", or "host:" for an rrsync-restricted
# key -- or rsync nests a second snapshots/ inside this one.
#
# Every copy is append-only: --ignore-existing, never --delete. Every step
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

stamp() { date -u +%Y-%m-%dT%H:%M:%SZ; }
log() { echo "$(stamp) collect-snapshot: $*"; }

usage() {
  echo "usage: collect-snapshot.sh [--pull-from SRC]... [--copy-to DEST]..." >&2
  exit 2
}

pulls=""
copies=""
while [ $# -gt 0 ]; do
  case "$1" in
    --pull-from) [ $# -ge 2 ] && [ -n "$2" ] || usage; pulls="$pulls
$2"; shift 2 ;;
    --copy-to) [ $# -ge 2 ] && [ -n "$2" ] || usage; copies="$copies
$2"; shift 2 ;;
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
mkdir -p "$snapshots"

status=0
log "start on $(hostname)"

# Newlines separate locations, so a location may not contain one; spaces are fine.
old_ifs=$IFS
IFS='
'
for src in $pulls; do
  if rsync -a --ignore-existing "$src" "$snapshots"; then
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
  if rsync -a --ignore-existing "$snapshots" "$dest"; then
    log "copied to $dest"
  else
    log "FAILED: copy to $dest"; status=1
  fi
done
IFS=$old_ifs

log "done, status $status"
exit $status
