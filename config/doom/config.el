;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
;;Agenda Configs
(load! "~/.config/doom/agenda.el")
;;load mappings
(load! "~/.config/doom/mappings.el")
;;load org-mode config
(load! "~/.config/doom/org.el")
;;API for creating new entries easily
(load! "~/.config/doom/create-org-entry.el")

;;enable line numbers when we enter treemacs
(add-hook 'treemacs-mode-hook 'display-line-numbers-mode)
;;enable follow mode so the tree follows the current file
(add-hook 'treemacs-mode-hook 'treemacs-follow-mode)

;;remap the cmd key in MacOs to the meta key (makes life easier)
;;TODO: maybe some sort of way to detect this only macOs? (dont know if this doesnt matter in non mac-os
(setq mac-command-modifier 'meta)

;;set embark's prompter to always use completing-read (faster)
(setq embark-prompter 'embark-completing-read-prompter)

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
;;(setq doom-font "Fragment Mono:pixelsize=12:weight=regular:slant=normal:width=normal:spacing=100:scalable=true")

;;cool themes: doom-rogue, doom-gruvbox, doom-miramre, doom-henna
(setq doom-theme 'doom-miramare)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)

(setq fancy-splash-image "~/assets/emacs-splash-500x500.png")

;;set a minimal delay on the refresh speed of the vterm buffer (should make it as fast as possible)
(setq vterm-timer-delay 0.01)

;;enable and disable evil depending on if vterm is enabled on current window
(add-hook 'window-selection-change-functions (lambda (window) (if (string-equal major-mode "vterm-mode")
                                               (evil-local-mode -1)
                                             (evil-local-mode 1)
                                             )))

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
