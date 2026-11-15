;;; Recipe: deepsky-moonunit, as it renders with its author's theme settings.
(:name "deepsky-moonunit"
 :emacs-theme deepsky-moonunit
 :url "https://github.com/fade/deepsky-themes-moonunit"
 :header ("Origin: the Emacs theme deepsky-moonunit, as it renders with its"
          "  author's theme settings, converted to rhizome roles."
          "Authors: Brian O'Reilly (deepsky-moonunit), built on the Modus"
          "  themes by Protesilaos Stavrou."
          "  Copyright (C) 2020-2026 Brian O'Reilly"
          "  Copyright (C) 2019-2026 Free Software Foundation, Inc."
          "Licence: GPL-3.0-or-later, the licence of the Emacs theme it derives"
          "  from. rhizome is AGPL-3.0-or-later; section 13 of the GPLv3 and"
          "  section 13 of the AGPLv3 permit combining the two.")
 ;; The theme settings from the author's Emacs init, and nothing else.
 :settings ((setq modus-themes-mixed-fonts t))
 :after-load ((set-face-attribute 'mode-line nil :box nil)
              (set-face-attribute 'cursor nil :background "yellow"))
 :settings-note ("Author's settings applied: modus-themes-mixed-fonts t, mode-line"
                 "  :box nil, cursor colour \"yellow\".")
 :corrections
 ((:text "the stray (set-face-foreground 'cursor \"navy\") in the palette is ignored"
   :check (not (equal (tc-hex (face-attribute 'cursor :foreground nil 'default))
                      "#000080")))
  (:text "dblue, written \" #1874cd\", is read as #1874cd"
   :colour (" #1874cd" . "#1874cd"))
  (:text "magit-diff-added-highlight :forground is read as :foreground"
   :face-attribute (magit-diff-added-highlight :forground :foreground)
   :check (equal (tc-hex (face-attribute 'magit-diff-added-highlight :foreground))
                 "#333333"))
  (:text "the second, dead isearch spec stays ignored"
   :check (equal (tc-hex (face-attribute 'isearch :background nil 'default))
                 "#ffff00"))))
