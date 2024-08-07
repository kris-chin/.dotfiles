;; These mappings were created from a tangled config file in ~/org/doom-config/mappings.org

(map! :map evil-normal-state-map "C-h" #'evil-window-left)
(map! :map evil-normal-state-map "C-j" #'evil-window-down)
(map! :map evil-normal-state-map "C-k" #'evil-window-up)
(map! :map evil-normal-state-map "C-l" #'evil-window-right)

;;window resize maps
(map! "C-a C-h" #'shrink-window-horizontally)
(map! "C-a C-l" #'enlarge-window-horizontally)
(map! "C-a C-j" #'shrink-window)
(map! "C-a C-k" #'enlarge-window)

;;map \ + b for all buffers
(map! :map evil-normal-state-map "\\ b" #'ibuffer)

;;map "SPC + m\n" to easymotion
(map! :leader "m" #'evilem-motion-find-char)
(map! :leader "n" #'evilem-motion-find-char-backward)

;;unmap evil-emacs state cuz why the hell would I use this
(map! :map evil-motion-state-map "\\" nil)

  ;;Helper function to call universal-argument with echo feedback
(defun wrap-universal-argument ()
  (interactive)
  "Wraps universal0argument with echo feedback"
  (progn
    (universal-argument)
    (message "Universal Argument Pressed")
    )
  )
  ;;Map the universal argument to M-u, and also provide visual feedback that it was pressed
  (map! :map evil-normal-state-map "M-u" #'wrap-universal-argument)
  (map! :map evil-insert-state-map "M-u" #'wrap-universal-argument)
  (map! :map evil-visual-state-map "M-u" #'wrap-universal-argument)

;;map \ + t to treemacs
(map! :map evil-normal-state-map "\\ t" #'treemacs)
(map! :map evil-normal-state-map "t" nil) ;;unmap treemacs with just t
;;map this for treemacs
(map! :after treemacs :map treemacs-mode-map "\\ t" #'treemacs)
;;Do this for override-mode (used with org-agenda)
(map! :map 'override :nvm "\\ t" #'treemacs)

;;useful file-manipulation treemacs keybindings :)
(map! :after treemacs :map treemacs-mode-map "a" #'treemacs-create-file)
(map! :after treemacs :map treemacs-mode-map "r" #'treemacs-rename-file)
(map! :after treemacs :map treemacs-mode-map "d" #'treemacs-delete-file)
(map! :after treemacs :map treemacs-mode-map "c" #'treemacs-copy-file)
(map! :after treemacs :map treemacs-mode-map "m" #'treemacs-move-file)

;;useful treemacs motion
(map! :after treemacs :map treemacs-mode-map "C-j" #'treemacs-next-neighbour)
(map! :after treemacs :map treemacs-mode-map "C-k" #'treemacs-previous-neighbour)
;;I really wanted to bind C-o to move up a parent. but it was being really annoying. either because of a seperate plugin or because o is treated as some sort of localleader)
;;the key to move up a parent is "u"

(defhydra hydra-flow-jira ()
    "jira scripts hydra"
    ("c" #'flow-jira-create-ticket "create JIRA ticket")
    ("s" #'flow-jira-update-ticket-status "update JIRA status")
    ("t" #'flow-test-script "test script")
  )

(defhydra hydra-flow-github ()
  "github scripts hydra"
  ("c" #'flow-github-create-pr "create pull request")
)

(defhydra hydra-flow-git-branch ()
  "git branch hyrda"
    ("c" #'flow-git-create-branch "create git branch")
    ("s" #'flow-git-switch-to-branch "switch to git branch")
    ("p" #'flow-git-push-to-remote "push to remote")
  )

(defhydra hydra-flow-git ()
    "git script hydra"
    ("h" #'hydra-flow-github/body "GitHub" :exit t)
    ("b" #'hydra-flow-git-branch/body "Branch" :exit t)
)

(defhydra hydra-flow-tmux ()
  "github scripts tmux"
  ("c" #'flow-tmux-change-all-to-repo "cd all to repo")
)

;;map \ + r to org-roam-buffer toggle
(map! :map evil-normal-state-map "\\ r" #'org-roam-buffer-toggle)
(map! :map org-roam-mode-map "\\ r" #'org-roam-buffer-toggle) ;;do this for the org roam mode

(defhydra hydra-org-roam ()
  "org-roam"
  ;; Right-hand side: IDs and aliases
  ("i" #'org-id-get-create "create id")
  ("j" #'org-roam-alias-add "add alias")
  ;; Left-hand side: Adding links / creating new nodes
  ("r" #'org-roam-node-insert "insert link")
  ;;These two are ALMOST the same thing, but imma just keep them
  ("e" #'org-roam-capture "capture")
  ("f" #'org-roam-node-find "find node")
  )

  ;;Change some of the org-super-agenda bindings so they are compatible with evil
  (map! :after org-super-agenda :map org-super-agenda-header-map "j" 'evil-next-line)
  (map! :after org-super-agenda :map org-super-agenda-header-map "k" 'evil-previous-line)

(map! :map org-agenda-mode-map "r" #'org-agenda-refile)
(map! :map org-agenda-mode-map "t" #'org-agenda-todo)
(map! :map org-agenda-mode-map "=" #'org-agenda-set-tags)
;;These two bindings are actually prefix keys to a fuck-ton of keysequences, so I'm just overriding ALL of them with in general-override-mode
;;This means that I will just enable and disable general-override-mode for org agenda
(map! :map 'override :nvm "s" #'org-agenda-schedule :desc "Overrides all 's' bindings in override-mode")

;;Function that calls both rebuild AND save
(defun rebuild-and-save ()
  (interactive)
  (progn
    (org-agenda-redo)
    (org-save-all-org-buffers)
    )
  )
(map! :map 'override :nvm "g" #'rebuild-and-save :desc "Overrides all 'g' bindings in override-mode")

;;toggle todo states with "shift - h" and "shift - l"
(map! :map evil-normal-state-map "H" "S-<left>")
(map! :map evil-normal-state-map "L" "S-<right>")
;;toggle priorities with "shift - j" and "shift - k"
(map! :map evil-normal-state-map "J" #'org-priority-up)
(map! :map evil-normal-state-map "K" #'org-priority-down)

(defhydra hydra-org-mode-motion ()
  ("j" #'org-next-visible-heading "next visible")
  ("k" #'org-previous-visible-heading "previous visible")
  ("C-j" #'org-forward-heading-same-level "forward same level")
  ("C-k" #'org-backward-heading-same-level "backward same level")
  ("9" #'outline-up-heading "back up")
  )

;;this mapping is completely seperate from my normal org-mode mapping. this is purely for faster keybindings
(map! :map evil-normal-state-map "C-9" #'hydra-org-mode-motion/body)

(defhydra hydra-org-view-toggles ()
  ("n" #'org-narrow-to-subtree "narrow to subtree")
  ("w" #'widen "widen")
  ("l" #'org-toggle-link-display "toggle link display")
  ("c" #'org-columns "column view")
  ("t" #'org-tidy-toggle "tidy toggle")
  )

;;hydra for all flow
(defhydra hydra-org-flow ()
  ("g" #'hydra-flow-git/body "Git" :exit t)
  ("j" #'hydra-flow-jira/body "JIRA" :exit t)
  ("t" #'hydra-flow-tmux/body "tmux" :exit t)
  )

  (defhydra hydra-agenda-views ()
    ("a" #'custom-agenda-next-actions "Next Actions (Today)")
    ("s" #'custom-agenda-get-ahead "Get Ahead")
    ("d" #'custom-agenda-inbox "Inbox")
    )

  ;; Bindings related to GTD
(defhydra hydra-gtd ()
  ;;Left-Hand Side: Agenda / Capture
  ("a" #'hydra-agenda-views/body "agenda" :exit t)
  ("c" #'org-capture "capture")
  ("d" #'org-refile "refile")
  ;;RIght-Hand Side: Individual Org entry Modification
  ("o" #'org-todo "toggle TODO state")
  ("i" #'org-set-tags-command "set tags")
  ("k" #'org-schedule "Schedule")
  )

  (defhydra hydra-org-timestamp ()
    ("i" #'org-time-stamp-inactive "inactive timestamp")
    ("o" #'org-time-stamp "active timestamp")
    )

(defhydra hydra-org-mode ()
  ("r" #'hydra-org-roam/body "roam" :exit t)
  ("f" #'hydra-org-flow/body "flow" :exit t)
  ("d" #'hydra-gtd/body "GTD" :exit t)
  ("o" #'hydra-org-view-toggles/body "view toggles" :exit t)
  ("i" #'hydra-org-timestamp/body "timestamp" :exit t)
  ("l" #'org-insert-link "insert link")
  ;;("s" #'org-sort "sort heading")
  ;;("8" #'org-toggle-heading "toggle heading")
  )
;;this is kinda a crazy mapping but im down wit it..
(map! :map evil-normal-state-map "C-o" #'hydra-org-mode/body)
(map! :map evil-visual-state-map "C-o" #'hydra-org-mode/body)
(map! :map evil-insert-state-map "C-o" #'hydra-org-mode/body)

;;Lets have M-z be a reliable key for embark
(map! :map evil-normal-state-map "M-z" #'embark-act)

;;oh yes, a hydra in a hydra
(defhydra hydra-consult-buffer ()
  "consult-buffer"
  ("o" #'consult-buffer "buffer search")
  ("p" #'kill-buffer "kill buffer")
  )

(defhydra hydra-consult-line ()
  "consult-line"
  (";" #'consult-line "single line")
  ("p" #'consult-line-multi "multi line")
  )

(defhydra hydra-consult ()
  "consult"
  ;;note: nested hydras need :exit t
  ("o" #'hydra-consult-buffer/body "buffer" :exit t)
  ("p" (lambda () (interactive) (consult-find "~/org")) "find")
  ("[" (lambda () (interactive) (consult-grep "~/org")) "grep")
  (";" #'hydra-consult-line/body "line" :exit t)
  )
;;TODO: add folder search
(map! :after evil :map evil-normal-state-map "C-p" #'hydra-consult/body)

(defhydra hydra-zoom ()
  "zoom"
  ("[" #'text-scale-decrease "zoom out (BUFFER)")
  ("]" #'text-scale-increase "zoom in (BUFFER)")
  ("-" #'zoom-frm-out "zoom in (FRAME)")
  ("=" #'zoom-frm-in "zoom in (FRAME)")
  )

;;Apply this keybinding to ALL evil states
(map! :map evil-normal-state-map "M--" #'hydra-zoom/body)
(map! :map evil-normal-state-map "M-=" #'hydra-zoom/body)
(map! :map evil-insert-state-map "M--" #'hydra-zoom/body)
(map! :map evil-insert-state-map "M-=" #'hydra-zoom/body)
(map! :map evil-visual-state-map "M--" #'hydra-zoom/body)
(map! :map evil-visual-state-map "M-=" #'hydra-zoom/body)
;;Apply this keybinding to all modes that DONT use evil (eg. treemacs, org-agenda)
(map! "M--" #'hydra-zoom/body)
(map! "M-=" #'hydra-zoom/body)

;;Bypassing certain keybindings so they are passed to vterm
(map! :after vterm :map vterm-mode-map "C-c" #'vterm-send-C-c)
(map! :after vterm :map vterm-mode-map "C-u" #'vterm-send-C-u)
(map! :after vterm :map vterm-mode-map "C-h" #'vterm-send-C-h)
(map! :after vterm :map vterm-mode-map "C-l" #'vterm-send-C-l)
(map! :after vterm :map vterm-mode-map "C-j" #'vterm-send-C-j)
(map! :after vterm :map vterm-mode-map "C-k" #'vterm-send-C-k)
(map! :after vterm :map vterm-mode-map "<escape>" #'vterm-send-escape)
