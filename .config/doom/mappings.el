;; These mappings were created from a tangled config file in ~/org/doom-config/mappings.org

(map! "C-h" #'evil-window-left)
(map! :map evil-normal-state-map "C-j" #'evil-window-down)
(map! :map evil-normal-state-map "C-k" #'evil-window-up)
(map! "C-l" #'evil-window-right)

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

;;map \ + t to treemacs
(map! :map evil-normal-state-map "\\ t" #'treemacs)
(map! :map evil-normal-state-map "t" nil) ;;unmap treemacs with just t
(map! :after treemacs :map treemacs-mode-map "\\ t" #'treemacs) ;;also do this for the treemacs buffer

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

(defhydra hydra-flow-git ()
    "git scripts hydra"
    ("c" #'flow-git-create-branch "create git branch")
    ("s" #'flow-git-switch-to-branch "switch to git branch")
    ("p" #'flow-git-push-to-remote "push to remote")
    ("h" #'hydra-flow-github/body "GitHub" :exit t)
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
  ("i" #'org-roam-node-insert "insert link")
  ("f" #'org-roam-node-find "find node")
  ("c" #'org-roam-capture "capture")
  ("s" #'org-id-get-create "id")
  )

;;keybinds for org capture / org agenda / org link handling
(map! "C-c l" #'org-store-link)
(map! "C-c a" #'org-agenda)
(map! "C-c c" #'org-capture)

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
  ("o" #'outline-up-heading "back up")
  )

(defhydra hydra-org-mode ()
  ("o" #'hydra-org-mode-motion/body "motion" :exit t)
  ("r" #'hydra-org-roam/body "roam" :exit t)
  ("g" #'hydra-flow-git/body "Git" :exit t)
  ("j" #'hydra-flow-jira/body "JIRA" :exit t)
  ("t" #'hydra-flow-tmux/body "tmux" :exit t)
  ("n" #'org-narrow-to-subtree "narrow to subtree")
  ("w" #'widen "widen")
  ("l" #'org-toggle-link-display "toggle link display")
  ("8" #'org-toggle-heading "toggle heading")
  ("s" #'org-sort "sort heading")
  ("T" #'org-todo "toggle TODO state")
  ("c" #'org-columns "column view")
  )
;;this is kinda a crazy mapping but im down wit it..
(map! :map evil-normal-state-map "C-o" #'hydra-org-mode/body)

;;(setq metadata-keywords
;;      '(("description" . "Description")
;;        )
;;)
;;(defun quick-keyword ()
;;  "Easy Creation of #+KEYWORD metadata"
;;  (interactive)
;;  (
;;   (let* (
;;          (choice (completing-read "Type in Metadata: " metadata-keywords nil t))
;;          (action (assoc choice metadata-keywords))
;;         )
;;   )
;;   (message "You chose %s" action)
;;  )
;;)
;;(quick-keyword)

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
  ("-" #'text-scale-decrease "zoom out")
  ("=" #'text-scale-increase "zoom in")
  )

(map! :map evil-normal-state-map "M--" #'hydra-zoom/body)
(map! :map evil-normal-state-map "M-=" #'hydra-zoom/body)
