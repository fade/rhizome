#!/bin/sh
# Regenerate the converted themes under themes/ from their Emacs sources.
#
# A developer tool: rhizome never runs it, and its output is committed.
# Each variable names a checkout of the Emacs theme source:
#
#   MODUS_THEMES      https://github.com/protesilaos/modus-themes (5.2.0)
#   EF_THEMES         https://github.com/protesilaos/ef-themes (2.2.1)
#   KUSANAGI_THEME    https://github.com/LionyxML/kusanagi-theme (0.1.1)
#   TOKYO_NIGHT_EMACS https://github.com/bbatsov/tokyo-night-emacs (v1.1.0)
#   MOONUNIT_THEME    https://github.com/fade/deepsky-themes-moonunit
#
# A theme whose variable is unset is skipped. ef-summer, kusanagi, and
# deepsky-moonunit build on the Modus themes, so they are also skipped
# when MODUS_THEMES is unset. Reports go to TC_REPORT
# (default: $TMPDIR or /tmp). Usage, from anywhere:
#   MODUS_THEMES=~/src/modus-themes ... tools/theme-convert/convert-all.sh
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
out=${TC_OUT:-$root/themes}
reports=${TC_REPORT:-${TMPDIR:-/tmp}}
emacs=${EMACS:-emacs}

convert() {
    recipe=$1
    shift
    echo "theme-convert: $recipe"
    TC_OUT=$out TC_REPORT=$reports TC_RHIZOME=$root/ "$emacs" -Q --batch \
        -l "$here/theme-convert.el" "$here/recipes/$recipe.el" "$@"
}

if [ -n "${MODUS_THEMES:-}" ]; then
    convert modus-operandi "$MODUS_THEMES"
    convert modus-vivendi "$MODUS_THEMES"
    convert modus-operandi-deuteranopia "$MODUS_THEMES"
    convert modus-vivendi-deuteranopia "$MODUS_THEMES"
    convert modus-operandi-tritanopia "$MODUS_THEMES"
    convert modus-vivendi-tritanopia "$MODUS_THEMES"
    if [ -n "${EF_THEMES:-}" ]; then
        convert ef-summer "$EF_THEMES" "$MODUS_THEMES"
    fi
    if [ -n "${KUSANAGI_THEME:-}" ]; then
        convert kusanagi "$KUSANAGI_THEME" "$MODUS_THEMES"
    fi
    if [ -n "${MOONUNIT_THEME:-}" ]; then
        convert deepsky-moonunit "$MOONUNIT_THEME" "$MODUS_THEMES"
    fi
fi
if [ -n "${TOKYO_NIGHT_EMACS:-}" ]; then
    convert tokyo-night-emacs "$TOKYO_NIGHT_EMACS"
fi
