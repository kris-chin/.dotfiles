;;org-mode related configuration

(setq org-directory "~/org/")

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

;;sets the org roam directory
(setq org-roam-directory (file-truename "~/org"))

;;associate md files with org roam (md-roam)
(setq org-roam-file-extensions '("org" "md"))
(md-roam-mode 1)
(setq md-roam-file-extension "md")

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


;;start all org documents folded
(setq org-startup-folded t)

(defun inbox-template-function ()
  "Template for new inbox entries"
  (concat ;;progn runs multiple args at a time
     "* TODO %^{Insert Title}  %^G"
     "\n:PROPERTIES:"
     "\n:CREATED: %T"
     "\n:END:"
     "\n -  %?"
     "\n** Log"
  )
)

;;Same as the above function but it uses the captured text as the title of the function
(defun inbox-template-selection-capture-function ()
  "Template for new inbox entries"
  (concat ;;progn runs multiple args at a time
     "* TODO %i  %^G"
     "\n:PROPERTIES:"
     "\n:CREATED: %T"
     "\n:END:"
     "\n -  %?"
     "\n** Log"
  )
)

;;templates for my org-capture
(setq org-capture-templates
      '(
        ("c" "Inbox - Personal" entry (file+headline "~/org/gtd/personal.org" "Inbox") (function inbox-template-function) )
        ("v" "Inbox - Personal (selected text)" entry (file+headline "~/org/gtd/personal.org" "Inbox") (function inbox-template-selection-capture-function) )
        ("d" "Inbox - Tinkering" entry (file+headline "~/org/gtd/tinkering.org" "Inbox") (function inbox-template-function) )
        ("f" "Inbox - Tinkering (selected text)" entry (file+headline "~/org/gtd/tinkering.org" "Inbox") (function inbox-template-selection-capture-function) )
       )
)

;;custom TODO workflow states
;; the "!" flag adds to a logbook of state changes, @ asks for a note with timestamp
(setq org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@)"
                                   "|" "DONE(!)")))

(setq org-log-done 'time)
;;Put loging into a "LOGBOOK" drawer
(setq org-log-into-drawer "LOGBOOK")

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
  ;;Sometimes this still attempts to open a websocket even though it is already open from a previous instance. (this probably has to do with the fact that I'm running emacs as a daemon)
  ;;I'm just gonna ignore the error, hopefully that doesn't bite me in the ass
  (ignore-errors
    (org-roam-ui-mode)
  )
  )
