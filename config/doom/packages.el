;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;;replaces headline markers with unicode bullets
(package! org-bullets)

;;mixes pitches for text that is obviously table or code vs text that is not meant to be that
(package! mixed-pitch)

;;obsidian & emacs side-by-side 
(package! obsidian)

;;org rainbow randomly-generated tags :)
;;disabling because I don't like how it overrides my file variables for TODO colors
(package! org-rainbow-tags)

;;mermaid support (requires mermaid cli installed for graph generation)
(package! mermaid-mode)

;;an upgraded agenda view
;;TODO: understand this
(package! org-super-agenda)

;;provides a query language for org files for better searches
;;TODO: understand this
(package! org-ql)

;;show live key feedback
(package! showkey)

;;fancy priorities
(package! org-fancy-priorities)

;;misc helper commands
(package! misc-cmds)

;;helper package that helps get variables from your zshrc/bashrc
(package! exec-path-from-shell)

;;centers the cursor properly
;;please use this instead of scroll margin 999. that one doesnt work like vim's and just sucks.
;;TODO: I believe in this plugin. if you do end up working on smooth scrolling again for emacs, PLEASE just keep this plugin here
(package! centered-cursor-mode)

;;enables drag-and-drop images for org mode (either from the browser, file system, remote image, or screenshots)
(package! org-download)
;;adds a screenshot-taker command to add screenshots
(package! org-ros)

;;org-tidy: adds a new minor mode to hide property drawers
(package! org-tidy)

;;org-roam-ui: runs a web-server on localhost that you can access. it graphically displays your org-roam notes as a graph
;;"org-roam-ui tries to keep up with the latest features of org-roam, which conflicts with Doom Emacs's desire for stabilitiy. to Make sure nothing breaks, use the latest version of org-roam by unpinning it"
(unpin! org-roam)
(package! org-roam-ui)

;;enable zooming the text of the entire emacs frame, rather than a single buffer
(package! zoom-frm)

;;md-roam (it's not on MELPA)
;;https://github.com/nobiot/md-roam
(package! md-roam
          :recipe (:host github
                     :repo "nobiot/md-roam"
                     :files ("*.el")
                   )
          )

;;ahk-mode (adds an AutoHotKey major mode to emacs)
(package! ahk-mode)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
