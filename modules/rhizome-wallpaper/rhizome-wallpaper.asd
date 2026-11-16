(asdf:defsystem #:rhizome-wallpaper
  :description "The rhizome wallpaper module: a background image per
output, painted by swaybg and picked again on the wallpaper command"
  :author "parenworks"
  :license "AGPL-3.0"
  :depends-on (#:rhizome)
  :components ((:file "wallpaper")))
