;;Helper function that converts a time in seconds to an org-mode active timestamp
(defun time-to-org-active (seconds)
  (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (seconds-to-time seconds)) 
  )

;;Provide a simple API for us to create entries purely through the org-mode element api
;;We can call this from the emacs server via bash. neat!
(defun create-org-entry (title bucket category
                               &optional time-created description tags parent-title)
  "Creates an org entry"
  (save-window-excursion
    ;;1. Go to the respective category file
    ;;TODO: This is interactive.. we dont need this.
    (find-file-other-window (format "~/org/gtd/%s.org" category))
    ;;2. Move pointer to the respective bucket
    (let ((marker (org-find-exact-headline-in-buffer (if parent-title parent-title bucket))))
      (goto-char marker)
      ;;Create the subheading. This should also move the marker
      (org-insert-todo-subheading nil)
      ;;From here, we can add our properties
      ;;Unfortunately, we're implementing our template like this. If changes to the template are made, this will need to change as well..
      (let ((element (org-element-at-point)))
        (org-edit-headline title)
        (org-set-tags tags)
        ;;Insert the content
        (goto-char (line-end-position))
        (insert (format "\n%s" description ))
        ;;Set our properties (this needs to be placed here so it is above the content)
        (org-set-property "CREATED" (if time-created
                                        (time-to-org-active (string-to-number time-created))
                                      (time-to-org-active (time-to-seconds))
                                      ))
        ;;Insert the log
        (org-insert-subheading nil)
        (org-edit-headline "Log")
      )
    )
  )
)
