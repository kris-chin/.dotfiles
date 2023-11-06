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

;;Helper function
;;If the buffer does not have any windows, kill it.
;;TODO Get this to work
(defun flow-kill-buffer-if-no-windows (buffer)
  "Kills the buffer if no windows are detected"
  (let ((process (get-buffer-process buffer) ))
    (when 
      (and (bufferp buffer) ;;buffer exists
           (not (get-buffer-window buffer t)) ;;if attempting to get the window for the buffer fails (no windows exist)
      )
      (delete-process process)
      ;;(kill-buffer buffer) ;;first, kill the buffer (kills the process as well)
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
    (let ((buffer (process-buffer process))
          (last-lines (get-last-line-content flow-script-line-count))
          (on-close-form flow-script-end-form)
         )
      (delete-window (get-buffer-window buffer)) ;;kill window first
      ;;TODO: figure out if last-line was actual content or script-exit
      ;;TODO: detect python errors
      (kill-buffer buffer) ;;then kill buffer, leaving us back in the old buffer
      (funcall on-close-form last-lines) ;;call the callback in the OLD buffer (this needs to be here)
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
  )

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

