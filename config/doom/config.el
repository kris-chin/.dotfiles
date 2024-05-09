;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;inspo taken from https://zzamboni.org/post/beautifying-org-mode-in-emacs/

;;Get our environment variables from our .zshrc/bashrc
;;(use-package! exec-path-from-shell
;;              :config
;;              (progn 
;;                ;;get our environment variables from our shell
;;                (dolist (env-var '(
;;                                   "JIRA_API_TOKEN"
;;                                   "JIRA_AUTH_TYPE"
;;                                   "JIRA_BASE_URL"
;;                                   "USER_EMAIL"
;;                                   "USERNAME"
;;                                   "MYSELF"
;;                                   "USER_EMAIL"
;;                                   "RELEVANT_PROJECTS"
;;                                   "PRODUCT_MANAGERS"
;;                                   "QA_ENGINEERS"
;;                                   "RELEVANT_COMPONENTS"
;;                                   "RELEVANT_LABELS"
;;                                   "BASE_URL"
;;                                   "JIRA_BASE_URL"
;;                                   "DUMMY_ISSUE"
;;                                   "GITHUB_BASE_NAME"
;;                                   "GITHUB_API_TOKEN"
;;                                   "JENKINS_BASE_URL"
;;                                   "JENKINS_USERNAME"
;;                                   "JENKINS_API_TOKEN"
;;                               ))
;;                  (add-to-list 'exec-path-from-shell-variables env-var))
;;                ;;initialize exec-path-from-shell for macos
;;                (when (memq window-system '(mac ns x))
;;                  (exec-path-from-shell-initialize))
;;                )
;;              )

;;my own custom major mode to run external integration scripts
(load! "~/.config/doom/flow-script.el")

;;load mappings
(load! "~/.config/doom/mappings.el")


;;hide emphasis markers in the markup
(setq org-hide-emphasis-markers t)

;;set up a font-lock substitution in org mode that looks for '-' characters and replaces them with a unicode dot
;;for future reference, compose-region is the function that replaces a match with a replacement text
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;replace headline markers with different unicode bullets
(use-package! org-bullets
              :config
              (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;set up preportional fonts of different sizes for headlines
;;for future reference, this is setting some local variables, one being @variable-tuple, which changes based on if a font is installed
;;this removes unique heading colors :( dont know how to get it back yet
;;TODO: figure out how to toggle this for note-taking vs a TODO list
;;(let* ((variable-tuple
;;        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;       (base-font-color     (face-foreground 'default nil 'default)) ;;this becomes black for some reason and it sucks
;;       (headline           `(:inherit default :weight bold ))) ;;so I removed :foreground from here
;;
;;  (custom-theme-set-faces
;;   'user
;;   `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
;;   ;;custom set variable AND fixed pitch font faces
;;   ;;`(variable-pitch ((t (:family ,@variable-tuple :height 18 :weight thin))))
;;   ;;`(fixed-pitch ((t (:family "Fragment Mono" :height 16 ))))
;;  )
;;)

;;automatically enable variable-pitch mode for org mode buffers
;;for future reference, i'm assuming what this is doing is calling the variable-pitch-mode command when emacs sees its in org mode
(add-hook 'org-mode-hook 'variable-pitch-mode)

;;enable mixed pitch mode 
(use-package! mixed-pitch
             :config
             (add-hook 'text-mode-hook #'mixed-pitch-mode)
             )

;;enable line numbers when we enter treemacs
(add-hook 'treemacs-mode-hook 'display-line-numbers-mode)
;;enable follow mode so the tree follows the current file
(add-hook 'treemacs-mode-hook 'treemacs-follow-mode)

;;remap the cmd key in MacOs to the meta key (makes life easier)
;;TODO: maybe some sort of way to detect this only macOs? (dont know if this doesnt matter in non mac-os
(setq mac-command-modifier 'meta)

;;sets the org roam directory
(setq org-roam-directory (file-truename "~/org/roam"))
;;set up org-roam to autosync on file changes to maintain cache consistency
(org-roam-db-autosync-mode)

;;configure obsidian for emacs
(use-package! obsidian
              :ensure t
              :demand t
              :config
              (obsidian-specify-path "~/Obsidian")
              (global-obsidian-mode t)
              )

;;auto fold blocks in org mode
(setq org-hide-block-startup t)

;;set up org default notes for org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;useful function for modifying header line in org-capture
(defun org-capture-modify-header-line ()
  (setq header-line-format "`C-c C-q` tags | `C-c C-j` link JIRA | `C-c C-g` link branch | `C-c C-p` new roam page")
)
(add-hook 'org-capture-mode-hook #'org-capture-modify-header-line)

(defun focus-template-function ()
  "Just a test function for testing out how to write in elisp"
  (concat ;;progn runs multiple args at a time
     "* NOT_STARTED [#3] %?"
     "\n:PROPERTIES:"
     "\n:Created: %T"
     "\n:Jira:"
     "\n:GitBranch:"
     "\n:Page:"
     "\n:END:"
     "\n** Breakdown:"
     "\n1."
  )
)

;;templates for my org-capture
(setq org-capture-templates
      '(
        ("f" "Focus" entry (file+headline "~/org/focus.org" "Tasks") (function focus-template-function) )
       )
)

;;set embark's prompter to always use completing-read (faster)
(setq embark-prompter 'embark-completing-read-prompter)

;;custom TODO workflow states
;;TODO: Add BLOCKED and other stuff
(setq org-todo-keywords '((sequence "TODO" "NOT_STARTED" "QUEUED" "ANALYSIS" "DESIGN" "CODE" "DONE_ON_LOCAL" "PR_FEEDBACK" "TESTING"
                                    "|" "FOR_BACKEND" "FOR_THIS_RELEASE" "DONE")))

;;org priorities 0-9 
;;TODO: change this and also maybe make it cooler (easier to understand?) with org-fancy-priorities
;;The lowest priority NEEDS to be a higher ascii number than the highest, but what if I want an inverse number system?
(setq org-highest-priority 1) 
(setq org-lowest-priority 10) ;;This is weird
(setq org-default-priority 2) ;;Start with 2nd Highest priority and go from there

;;Configure org-fancy-priorities
;;for some reason, ?1 is needed rather than 41.. why is that?
(use-package! org-fancy-priorities
              :ensure t
              :hook
              (org-mode . org-fancy-priorities-mode)
              :config
              (setq org-fancy-priorities-list
                    '(
                      (?1 . "P0")
                      (?2 . "P1")
                      (?3 . "P2")
                      (?4 . "P3")
                      (?5 . "P4")
                      (?6 . "P5")
                      (?7 . "P6")
                      (?8 . "P7")
                      (?9 . "P8")
                      (?: . "P9") ;;This kinda messes up the [#10] display for some reason, so you should look into this
                      )
              )
)

;;custom Priority colors
;;For some reason, ?1
(setq org-priority-faces '(
                           (?1 . "#FF0000") ;;red
                           (?2 . "#FFA500") ;;orange
                           (?3 . "#FFFF00") ;;yellow
                           (?4 . "#ADFF2F") ;;yellow-green
                           (?5 . "#008000") ;;green
                           (?6 . "#00FFFF") ;;blue-green
                           (?7 . "#0000FF") ;;blue
                           (?8 . "#4B0082") ;;indigo
                           (?9 . "#9400D3") ;;violet
                           (?: . "#FF00FF") ;;red-violet
                           )
      )

;;custom TODO keyword colors
(setq org-todo-keyword-faces '(
                               ("NOT_STARTED" . "snow4")
                               ("QUEUED" . "snow4")
                               ("ANALYSIS" . "RoyalBlue4")
                               ("DESIGN" . "RoyalBlue4")
                               ("CODE" . "RoyalBlue4")
                               ("DONE_ON_LOCAL" . "plum1")
                               ("PR_FEEDBACK" . "purple3")
                               ("TESTING" . "purple3")
                               ("FOR_BACKEND" . "maroon2")
                               ("FOR_THIS_RELEASE" . "goldenrod1")
                               ("DONE" . "chartreuse3")
                               )
      )

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;;(setq user-full-name "XXX"
;;      user-mail-address "XXX")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;;(setq doom-font "Fragment Mono:pixelsize=12:weight=regular:slant=normal:width=normal:spacing=100:scalable=true")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-miramare)
;;cool themes: doom-rogue, doom-gruvbox, doom-miramre, doom-henna

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq fancy-splash-image "~/assets/emacs-splash-500x500.png")

;;set up org-roam-ui
(use-package! websocket :after org-roam)
(use-package! org-roam-ui
              :after org-roam
              :config (setq org-roam-ui-sync-theme t
                            org-roam-ui-follow t
                            org-roam-ui-update-on-save t
                            org-roam-ui-open-on-start nil)) ;;disable opening in a new browser on start
;;ALWAYS open the server on start (if it isnt open yet)
(unless org-roam-ui-mode
  (org-roam-ui-mode)
  )

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
