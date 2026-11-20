#!/bin/sh
# Tests for session/guest-start with a stub sbcl and a stub river-init.
# Each case runs a copy of guest-start beside a river-init that only
# records it ran, under a scratch HOME. Run: sh tests/guest-start.sh
set -u

top=$(cd "$(dirname "$0")/.." && pwd)
scratch=$(mktemp -d)
trap 'rm -rf "$scratch"' EXIT
pass=0
fail=0

ok() {
  if [ "$2" = "$3" ]; then
    pass=$((pass + 1)); printf 'PASS: %s\n' "$1"
  else
    fail=$((fail + 1)); printf 'FAIL: %s (got %s, wanted %s)\n' "$1" "$2" "$3"
  fi
}

# setup NAME SBCL-OUTPUT [nolisp]: a tree under $scratch/NAME with
# session/guest-start, a stub river-init, a HOME, and a stub sbcl that
# prints SBCL-OUTPUT ("absent" leaves sbcl off PATH)
setup() {
  d="$scratch/$1"
  mkdir -p "$d/tree/session" "$d/home" "$d/bin"
  cp "$top/session/guest-start" "$d/tree/session/"
  printf '#!/bin/sh\necho RIVER-INIT-RAN > "%s/ran"\n' "$d" \
    > "$d/tree/session/river-init"
  chmod +x "$d/tree/session/river-init"
  if [ "${3:-}" != nolisp ]; then
    mkdir -p "$d/home/quicklisp"
    : > "$d/home/quicklisp/setup.lisp"
    : > "$d/home/.sbclrc"
  fi
  if [ "$2" != absent ]; then
    printf '#!/bin/sh\n%s\n' "$2" > "$d/bin/sbcl"
    chmod +x "$d/bin/sbcl"
  fi
}

# run NAME: run the case's guest-start; sets rc and ran
run() {
  d="$scratch/$1"
  env -i HOME="$d/home" PATH="$d/bin:/usr/bin:/bin" \
    "$d/tree/session/guest-start" 2>/dev/null
  rc=$?
  if [ -e "$d/ran" ]; then ran=yes; else ran=no; fi
  log="$d/home/.cache/rhizome/guest-start.log"
}

has() { grep -qF -- "$2" "$1" && echo yes || echo no; }

setup nolisp absent nolisp
# /usr/bin may carry a real sbcl; this case checks the Lisp files only
run nolisp
ok "no quicklisp: exits 1" "$rc" 1
ok "no quicklisp: river-init not run" "$ran" no
ok "no quicklisp: names setup.lisp" "$(has "$log" "missing: $scratch/nolisp/home/quicklisp/setup.lisp")" yes
ok "no .sbclrc: names it" "$(has "$log" "missing: $scratch/nolisp/home/.sbclrc")" yes
ok "gap: says river has no WM" "$(has "$log" "river is running with no window manager")" yes

setup noreg 'echo "RHIZOME-SOURCE none"'
run noreg
ok "no registry entry: exits 1" "$rc" 1
ok "no registry entry: river-init not run" "$ran" no
ok "no registry entry: names the share tree" "$(has "$log" "(:tree \"$scratch/noreg/\")")" yes

setup nostart 'exit 1'
run nostart
ok "sbcl fails to start: exits 1" "$rc" 1
ok "sbcl fails to start: says so" "$(has "$log" "sbcl does not start")" yes

setup good "echo \"RHIZOME-SOURCE $scratch/good/tree/\""
run good
ok "all present: river-init runs" "$ran" yes
ok "all present: logs the source" "$(has "$log" "rhizome source: $scratch/good/tree/")" yes
ok "all present: notes the missing target" "$(has "$log" "rhizome-session.target is not installed")" yes

setup other 'echo "RHIZOME-SOURCE /elsewhere/rhizome/"'
run other
ok "other tree: river-init runs" "$ran" yes
ok "other tree: notes which tree runs" "$(has "$log" "resolves rhizome to /elsewhere/rhizome/")" yes

printf '%d passed, %d failed\n' "$pass" "$fail"
[ "$fail" = 0 ]
