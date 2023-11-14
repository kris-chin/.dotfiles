;;; -*- lexical-binding: t -*-
;;lexical-binding needed to be turned on so we could pass an outside variable into a lambda

;;These scripts define a simple Comint Wrapper Mode that helps with dealing with external scripts
;;For now, the mode is called "Flow" mode but that's corny af.

;;helper function to get the content of the last line of the buffer
;;(this actually gets the second-to-last line, but that's because the final line is an empty string)
;;linecount: number of lines to get from end of buffer
(defun get-last-line-content (linecount)
  "Gets the content of the last line in the current buffer"
  (save-excursion
    (goto-char (point-max)) ;;go to the end of the buffer

    (let* ((line-end (line-end-position))
           (line-start (progn
                        (forward-line (- linecount) ) ;;go up linecount lines
                        (line-beginning-position)
                        )))
      (buffer-substring line-start line-end)
    )
  )
)

;;Helper function to determine if a window is a main window
(defun my-window-is-main-p (window)
  "Check if WINDOW is a main window."
  (with-selected-window window
    (not (window-parameter nil 'no-other-windows))))

;;Helper function to select the window running the flow buffer
(defun select-flow-window ()
  "Selects the window holding the flow buffer"
  (select-window
    (let ((window (get-buffer-window "*Flow*")))
      (when window
        window)
      )
    )
  )

;;Helper variable for our buffer name
(defvar flow-buffer-name "*Flow*"
  "Name of the buffer to use for comint instances for flow scripts")

;;our sentinel function for our comint process.
;;this is used to kill the buffer & window as well as get the last line from the process
(defun flow-script-sentinel (process event)
  ;;kill buff if the process dies
  (unless (process-live-p process)
    (let* ((buffer (process-buffer process))
          (last-lines (get-last-line-content flow-script-line-count))
          (on-close-form flow-script-end-form)
          (window (get-buffer-window buffer))
          (exit-status (process-exit-status process))
         )
      (condition-case nil
                      (if (eq 0 exit-status) ;;if no errors occurred
                            (progn
                                (sit-for 1) ;;sit for 1 second to read the output
                                (delete-window window) ;;kill window first
                                (kill-buffer buffer) ;;then kill buffer, leaving us back in the old buffer
                                (funcall on-close-form last-lines) ;;call the callback in the OLD buffer (this needs to be here)
                            )
                          (message "An error occurred: Exit Status: %s" exit-status)
                        )
                      ;;literally do nothing on error.
                      ;;this is to just mute the error message that shows up when this code attempts to delete a main window
                      ;;change this if necessary
                      (error ())
      )
    )
  )
)

;;This is the actual function that runs comint and enables flow-mode
(defun run-flow-script (filePath fileArgs on-close-function linecount)
  "Runs a flow script at filePath and takes it's last outputted line and passes it into on-close-form"
  (interactive)
  (let* ((buffer (get-buffer-create flow-buffer-name)) ;;creates a new buffer with our buffer name
         (proc-alive (comint-check-proc buffer)) ;;is the process alive or dead?
         (process (get-buffer-process buffer)) ;;get a pointer to the process associated with the buffer
         )
    (unless proc-alive
      (with-current-buffer buffer
         (apply 'make-comint-in-buffer "*Flow*" buffer "python3" nil (append (list filePath) fileArgs) ) ;;run Python3 with our script as the argument
         (flow-mode) ;;run our custom mode for this buffer only
         (setq flow-script-end-form on-close-function) ;;set the on-close form (sentinel will call this)
         (setq flow-script-line-count linecount) ;;set the number of lines we want to get from the end of the script
         ))
    ;;when
    (when buffer
      (pop-to-buffer buffer)
    )
  )
)

;;This advice is *carefully* added to delete-window
(defun flow-tmp-make-changes (&optional window)
  "Makes changes to a file and switches back to flow mode. Runs only in flow-tmp-mode"
    (let ((buffer (window-buffer window)))
      ;;ENSURE that we are attempting to close the Flow-tmp window and not any other window
      (when (string-equal (buffer-name buffer) "*Flow-Tmp*")
        ;;get all of the buffer content and write to the file
        (write-region (point-min) (point-max) flow-tmp-filename)
        ;;switch back to the flow buffer
        (switch-to-buffer (get-buffer "*Flow*"))
        (condition-case nil
            (progn
              (kill-buffer buffer)
              ;;remove advice so we can do normal delete-window functionality again
              (advice-remove 'delete-window #'flow-tmp-make-changes)
             )
            (error ()) ;;mute the error caused by the missing process (change if necessary)
          )
        )
      )
  )

(define-derived-mode flow-tmp-mode markdown-mode "Flow Script External Editor"
   "Major mode for any temporary editing done in Flow Script
   /
   "
   ;;Hooks:
   ;;On save, don't actually save to file (i think this is already done)
   ;;On window close or buffer exit, actually WRITE to the file and then switch back to flow mode
   ;;TODO: ensure only "delete-window" for this buffer is affected (it affects other buffers) (this doesn't work for some reason?)
   (advice-add 'delete-window :override #'flow-tmp-make-changes)
   )



;;TODO: implement this
(defun flow-open-tmp-buffer (filename)
   "Opens a tmpfile in a new buffer editor (starts in markdown mode) Exiting the file means saving the file"
   (let ((buffer (get-buffer-create "*Flow-Tmp*")))
       (select-flow-window) ;;ensure the flow window is selected
       (switch-to-buffer buffer)  ;;create a temp buffer that temporarily replaces the window
       (insert-file-contents filename)
       (flow-tmp-mode) ;;run the tmp-editor mode
       (setq flow-tmp-filename filename)
     )
  )

;;TODO: maybe move the reserved-prefixes variable out for performance reasons?

;;NOTE: for some reason, filter-output doesn't necessarily correspond with new outputs of the comint,
;;so we just get the last line of the buffer at the moment instead
(defun flow-read-prefixes (filter-output)
  "Comint Filter Function for Flow mode that looks for message prefixes and calls their associated functions"
  (let* ((reserved-prefixes '( ("[TMP FILE]:" . flow-open-tmp-buffer) ) )
         (last-line (with-current-buffer "*Flow*"
                        (string-trim-right (get-last-line-content 1)) ;;get last line content and trim string
                       )
                     )
        )
      (dolist (prefix-cell reserved-prefixes)
          (when (string-prefix-p (car prefix-cell) last-line) ;;if the output string is prefixed by the CAR
            (let ((suffix-string (substring last-line (length (car prefix-cell)))))
                (funcall (cdr prefix-cell) suffix-string) ;;call the associated function with the suffix as the argument
              )
            )
        )
      )
  )


;;The definition for our major mode that wraps our comint
(define-derived-mode flow-mode comint-mode "Flow Script"
  "Major mode for `run-flow-script`
  //
  "
  ;;Set up our sentinel function to watch the process so we can eventually get the final line
  (set-process-sentinel (get-buffer-process (current-buffer)) 'flow-script-sentinel)
  (setq comint-process-echoes t)
  (setq flow-script-end-form '(message "FORM_NOT_OVERRITTEN")) ;;form that is called once process exits. this is called by the sentinel and will be passed in a string as an argument 
  (setq flow-script-line-count 1) ;;number of lines to get from the end of the output when the process ends
  (add-hook (make-local-variable 'comint-output-filter-functions) 'flow-read-prefixes) ;;watch output from comint and run any commands based on that
  (remove-hook (make-local-variable 'comint-output-filter-functions) 'ansi-color-process-output);;disable ansi color function to hide an error (this is due to the mid-buffer switch for flow-tmp-mode
  )

;;Helper function to be added as an advice 
;;This function auto-kills the buffer of a window-to-be-closed only if this window's associated buffer is Flow itself
(defun flow-auto-kill-buffer (&optional window)
  "kills the buffer of a window only if it belongs to Flow"
    (let ((buffer (window-buffer window)))
      ;;ENSURE that we are attempting to close the Flow window and not any other window
      (when (string-equal (buffer-name buffer) "*Flow*")
        (kill-buffer buffer)
        )
      )
  )

;;be careful with this advice. "if deleting the window would leave no more windows in the window tree, an error is signaled."
;;IE. if your function errors out, emacs is quitting!! (this is due to how evil works)
(advice-add 'delete-window :before #'flow-auto-kill-buffer)

;;simple keymap for "Flow" mode (name subject to change)
;;not sure why I need this in this spot of the file specifically, (maybe comint-mode isn't defined yet in flow-mode-map?)
(defvar flow-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;;example definition for this keymap
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `flow'")


;;Runs a flow script and sets properties of the nearest heading based on propertyNames
;;PropertyNames is a list of properties to set based on the last lines of the script buffer.
;;    ie. If propertyNames is of length 2, then the last 2 lines of the script buffer will be set to those 2 properties respectively
(defun flow-set-property (propertyNames filePath fileArgs)
  "Runs a property-adding script"
  (interactive)
  ;;Run the script, take the output from the script and put into our property
  ;;In order for propertyName to be defined inside the lambda, we needed to enable lexical-binding in this file
  (run-flow-script filePath fileArgs (lambda (output) 
                              (let ((lines (split-string output "\n"))) ;;Split the output we got from the process into individual lines
                                (dolist  (current-line lines) ;;Iterate through each line
                                  (let ((property-name (car propertyNames))) ;;Get the next entry of the PropertyNames list (aka, the CAR)
                                    (org-entry-put (point) property-name current-line) ;;Set the corresponding property to the line we retrieved
                                    (setq propertyNames (cdr propertyNames)) ;;Pop the first entry of the PropertyNames list (aka, set to the CDR)
                                  )
                                )
                              )
                            )
                   (length propertyNames) ;;Get the last length(propertyNames) lines of the buffer
  )
)

;;FUNCTIONS FOR OUR SCRIPTS!
(defun flow-jira-create-ticket ()
  "Creates a ticket for the selected property"
  (interactive)
  (flow-set-property '("JIRA_Ticket" "JIRA_Status" "JIRA_Link") (concat (expand-file-name "~") "/.config/doom/scripts/jira_create_ticket.py") nil )
)

(defun flow-test-script ()
  "Runs the test script"
  (interactive)
  (run-flow-script (concat (expand-file-name "~") "/.config/doom/scripts/test_script.py") '() nil 0)
  )

(defun flow-jira-update-ticket-status ()
  "Updates the status of a JIRA Ticket"
  (interactive)
  ;;TODO: get when prIsMerged
  ( let ((current-status (org-entry-get (point) "JIRA_Ticket")))
    (if current-status ;;if current-status was found
        (flow-set-property '("JIRA_Status") (concat (expand-file-name "~") "/.config/doom/scripts/jira_update_ticket_status.py") (list current-status) )
      (message "No JIRA Status for this heading!")
    )
  )
)
(defun flow-git-create-branch ()
  "Creates a new branch"
  (interactive)
  ( let ((jira-ticket (org-entry-get (point) "JIRA_Ticket"))
         (local-repository (org-entry-get (point) "Local_Repository" 'inherit))
         )
        (if local-repository
          (flow-set-property '("Git_Branch") (concat (expand-file-name "~") "/.config/doom/scripts/git_create_branch.py") 
                             (append 
                                (list (concat "--dir=" local-repository) )
                                (if jira-ticket
                                    (list (concat "--jira_issue=" jira-ticket))
                                  '()
                                )
                             )
          )
          (message "No Local Repository found!")
       )
  )
)

(defun flow-git-switch-to-branch ()
  "Switches the active directory to branch"
  (interactive)
  ( let ((target-branch (org-entry-get (point) "Git_Branch"))
         (local-repository (org-entry-get (point) "Local_Repository" 'inherit))
         )
        (if local-repository
          (if target-branch
              (run-flow-script (concat (expand-file-name "~") "/.config/doom/scripts/git_switch_to_branch.py") (list target-branch (concat "--dir=" local-repository)) nil 1)
            (message "No Git Branch for this heading!")
          )
          (message "No Local Repository found!")
        )
  )
)

(defun flow-git-push-to-remote ()
  "Pushes changes to a git remote"
  (interactive)
  (let ((branch-name (org-entry-get (point) "Git_Branch"))
        (local-repository (org-entry-get (point) "Local_Repository" 'inherit))
        )
    (if local-repository
      (run-flow-script (concat (expand-file-name "~") "/.config/doom/scripts/git_push_to_remote.py") 
                       (append 
                        (list (concat "--dir=" local-repository) )
                        (if branch-name
                          (list (concat "--branch_name=" branch-name))
                          '()
                        )
                      )
                      nil 0)
      (message "No Local Repository found!")
      )
  )
)

