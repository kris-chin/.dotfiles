;; Angenda-related configuration

;;Enable super agenda mode
(org-super-agenda-mode)

(setq org-agenda-files (file-expand-wildcards "~/org/gtd"))

;;Toggle general-override-mode depending on if we enter org-agenda
;;This is because I have some keybindings that override major keysequences for that mode
(defun override-org-agenda-maps () 
  "Toggles general-override mode alongside org-agenda-mode"
  (if (and (eq major-mode #'org-agenda-mode) (not (active-minibuffer-window)))
      (progn
        (general-override-mode 1)
      )
    (progn
      (general-override-mode 0)
    )
  ))

;;TODO: I COULD make this generic. However, I'll only do it if I need to.
(defun throw-if-agenda-is-subtask (&rest r)
  (interactive)
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-agenda-error))))
    (org-with-point-at marker
      (org-back-to-heading t)
      ;;Go up to heading
      (org-up-heading-safe)
      (let ((todo-state (nth 2 (org-heading-components)))
            (heading (nth 4 (org-heading-components)))
            )
        ;;if todo-state is NOT nil, then we have a parent task.
        (when todo-state
          ;;TODO: Maybe I could prompt if I'm SURE I want to refile. I could see this getting annoying
          (error (concat "Cannot refile task since it is a subtask of \"" heading "\"") )
        )
      ))))
;;Block agenda-refiling if the task is a subtask
(advice-add 'org-agenda-refile :before 'throw-if-agenda-is-subtask)

;;Toggle general-override-mode only when we are in org-agenda
(add-hook 'org-agenda-mode-hook 'override-org-agenda-maps)
;;Disable general-override-mode when we exit the agenda
(advice-add 'org-agenda-quit :after 'override-org-agenda-maps)

;;Function advice to add metadata to item whenever it is scheduled
(defun add-schedule-metadata (&rest r)
  (interactive "P")
  (let ((marker (or (org-get-at-bol 'org-hd-marker)
                    (org-agenda-error))))
      (org-with-point-at marker
        (org-back-to-heading t)
        (let ((schedule_count (org-entry-get (point) "SCHEDULE_COUNT")))
          (if schedule_count 
            ;;If defined increment it by 1
            (org-entry-put (point) "SCHEDULE_COUNT" (number-to-string (+ (string-to-number schedule_count) 1)))
            ;;If undefined, add a schedule_count property as 1
            (org-entry-put (point) "SCHEDULE_COUNT" "1"))))))

;;Add advice to track schedule count after rescheduling 
(advice-add #'org-agenda-schedule :after-while #'add-schedule-metadata)

;;Helper function to convert org timestamp to formatted string
(defun format-org-timestamp (timestamp format-string)
  "Format an Org Mode TIMESTAMP according to FORMAT-STRING."
  (if timestamp
    (let ((time-value (org-time-string-to-time timestamp)))
      (format-time-string format-string time-value))
  "")
  )

;;Custom formatter for org-ql entries since org-ql STILL doesnt support it
;;Mostly not written by me. Just heavily modified
;;Taken from https://github.com/alphapapa/org-ql/issues/23
(defun zdo/org-ql-view--format-element (orig-fun &rest args)
   "This function will intercept the original function and
   add the category to the result.

   ARGS is `element' in `org-ql-view--format-element'"
    (if (not args)
        ""
      (let* ((element args)
             (properties (cadar element))
             (result (apply orig-fun element))
             (category (org-entry-get (plist-get properties :org-marker) "CATEGORY"))
             (created (org-entry-get (plist-get properties :org-marker) "CREATED"))
             (is-subtask (org-with-point-at (plist-get properties :org-hd-marker)
                        (org-back-to-heading t)
                        ;;Go up to heading
                        (org-up-heading-safe)
                        (let (
                              ;;I'm keeping this heading information here just in case I'll need it
                              (heading (nth 4 (org-heading-components)))
                              (todo-state (nth 2 (org-heading-components)))
                              ) 
                            ;;Return a "[s] " if a task is a subtask. Return emptry string if not.
                            (if todo-state "[s] " "")
                          )
                      )
                     ))
        (org-add-props
            ;;TODO: display org custom properties here
            (format "   %-10s %-12s %s%s"  category (format-org-timestamp created "%m.%d %H:%M") is-subtask (substring result 2 nil) )
            (text-properties-at 0 result)
            ;;TODO: apply text properties in certain areas of the string, and add more conditionals
            ;;ALSO: these will override the above properties. we should try and mix the properties together
            ;;'face '(:foreground "spring green")
          ))))
(advice-add 'org-ql-view--format-element :around #'zdo/org-ql-view--format-element)

;;testing out a custom super-agenda predicate
;;It looks like if the predicate returns true, it adds to the group
(defun test-p (item)
  (message (concat "TEST:\"" (string-trim item) "\""))
  )

;;shorthand function to return a string with the following format:
;;"keyname":"keyvalue"
;;has some additional handling to return null if "value" is nil
(defun json-str-property (key value)
  (format "\"%s\":%s" key (if value (format "\"%s\"" value) "null"))
  )

;;shorthand function to return an array with the following format:
;;"keyname": [list contents, no modifications done, you'll still need to do that yourself]
(defun json-array-property (key listvalue)
  (format "\"%s\":[%s]" key (mapconcat (lambda (x)
                                         (if (eq (car (last listvalue)) x)
                                           (format "%s" x) ;;if last item
                                           (format "%s,\n" x)
                                           ))
                                       listvalue)))

;;Helper function that removes the last character of a string only if it matches the substring
(defun remove-last-substring (str sub)
  (if (and str (> (length str) 0) (string= (substring str (* -1 (length sub))) sub))
      (substring str 0 (* -1 (length sub)))
    str
  ))

;;Formats an org timestamp to iso8601
(defun format-org-timestamp-to-iso (timestamp)
  (format-org-timestamp timestamp "%Y-%m-%dT%H:%M:%S%:z")
  )

(defun escape-double-quotes (str)
  "Escape double quotes in the given string."
  (replace-regexp-in-string "\"" "\\\\\"" str))

;;ChatGPT-ass funnction to get the logbook entries of an org element
(defun get-element-logbook-entries ()
  "Retrieve logbook entries for the Org element at point, even if point is not directly on the :LOGBOOK: keyword."
  (interactive)
  (save-excursion
    (let* ((element (org-element-at-point))
           (beg (org-element-property :begin element))
           (end (org-element-property :end element))
           (logbook-entries))
      (goto-char beg)
      (while (re-search-forward ":LOGBOOK:" end t)
        (let* ((drawer (org-element-at-point))
               (drawer-name (org-element-property :drawer-name drawer)))
          (when (string= drawer-name "LOGBOOK")
            (let ((contents (buffer-substring-no-properties
                             (org-element-property :contents-begin drawer)
                             (org-element-property :contents-end drawer))))
              (setq logbook-entries
                    (append logbook-entries
                            (mapcar #'escape-double-quotes
                                    (split-string contents "\n" t))))))))
      (if logbook-entries
          (message "Logbook entries: %s" logbook-entries)
        (message "No logbook entries found"))
      logbook-entries)))
;;Go through agenda files and get data on TODO items
(defun get-org-data-as-json ()
  ;;First, set up the beginning of the json-output
  (let ((json-output "{\n\t\"entries\":[\n"))
    ;;Then, call a function on every org element that matches our criteria.
    (org-map-entries (lambda () (let ((element (org-element-at-point)))
        (setq json-output (concat json-output "\t\t" (format "{%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s}"
          (json-str-property "name" (escape-double-quotes (org-element-property :title element)))
          (json-str-property "todo_keyword" (org-element-property :todo-keyword element))
          (json-str-property "priority" (org-element-property :priority element))
          (json-str-property "created_date" (format-org-timestamp-to-iso (org-entry-get (point) "CREATED")))
          (json-str-property "scheduled_date" (format-org-timestamp-to-iso (org-entry-get (point) "SCHEDULED")))
          (json-str-property "closed_date" (format-org-timestamp-to-iso (org-entry-get (point) "CLOSED")))
          (json-str-property "schedule_count" (org-entry-get (point) "SCHEDULE_COUNT"))
          (json-str-property "style" (org-entry-get (point) "STYLE"))
          (json-str-property "bucket" (org-with-point-at (org-element-property :org-hd-marker element)
                          (org-back-to-heading t)
                          ;;Go up to heading
                          (org-up-heading-safe)
                          (let (
                                (heading (nth 4 (org-heading-components)))
                                (todo-state (nth 2 (org-heading-components)))
                                ) 
                              ;;Return nil if a task is a subtask. Return heading if not.
                              (if todo-state nil heading)
                            )
                        ))
          (json-array-property "tags" (mapcar '(lambda (x) (format "\"%s\"" x) ) (org-element-property :tags element)))
          (json-array-property "logbook_entries" (mapcar '(lambda (x) (format "\t\t\"%s\"" x) ) (get-element-logbook-entries)))
        ) ",\n" )))
      )
      "TODO=\"TODO\"|TODO=\"WAIT\"|TODO=\"DONE\"" ;;the documentation for this is crap, I didn't know I could do this.
      'agenda ;;call the function on all agenda files
    )
    ;;Lastly, trim the last comma, and end the json off
    (concat (remove-last-substring json-output ",\n") "\n\t],\n\t\"last_updated\":\"" (format-time-string "%Y-%m-%dT%H:%M:%S%:z" (seconds-to-time (float-time) )) "\"\n}\n" )
    )
  )

;;Write the org json data to a hard-coded file. I'm not gonna bother :P
(defun write-json-org-data (&rest r)
  (interactive)
  (write-region (get-org-data-as-json) nil (concat (expand-file-name "~") "/org-data.json") )
  )

(advice-add #'org-save-all-org-buffers :after #'write-json-org-data)

;;Returns a dynamic regex that matches for a habit that was last repeated today.
(defun get-habit-last-repeat-regex ()
 (format-time-string "LAST_REPEAT: \\[%Y-%m-%d" (seconds-to-time (float-time)))
  )

;;define some groups here so I can reuse them for both the "today" view and the "get ahead" view
(setq shared-super-agenda-groups `(
                                    ;;Hide tasks that were completed in the past
                                    ;;NOTE: Marking a habit as done will NOT update the todo, instead, it will move the scheduled date to the next day, and update some additional metadata.
                                    (:name "Tasks Closed Today" :todo "DONE"
                                           :face (:strike-through t) 
                                           )
                                    (:name "Habits for Today"
                                           :and (
                                             :habit t
                                             :scheduled today
                                             :not (:scheduled future)
                                           )
                                    )
                                    (:name "Habits coming up"
                                           :and (
                                             :habit t
                                             :scheduled future
                                             :not (:regexp ,(get-habit-last-repeat-regex))
                                           )
                                    )
                                    (:name "Habits Completed Today"
                                           :and (
                                             :habit t
                                             :regexp ,(get-habit-last-repeat-regex)
                                             :scheduled future
                                           )
                                           :face (:strike-through t) 
                                           )
                                    (:name "Vague Tasks"
                                           :face (:foreground "medium purple" :slant t)
                                           :tag "vague"
                                           :order 9
                                           )
                                    (:name "Projects"
                                          :children todo
                                          :order 10
                                          )
                                    (:name "Overdue"
                                           :face (:foreground "red")
                                           :scheduled past)
                                    (:name "Unscheduled"
                                           :face (:foreground "gold")
                                           :scheduled nil
                                           :order 8
                                           )
                                    (:name "Waiting"
                                          :todo "WAIT")
                                    (:name "Contacting/Booking"
                                          :tag ("calling" "emailing" "texting" "contacting" "booking")
                                          )
                                    ;;I don't want to use the word "research", in case I ever do it in the future.
                                    (:name "Digging around"
                                          :tag ("digging" "searching" "investigation")
                                          )
                                    (:name "Tasks"
                                          :todo "TODO"
                                    )
                                  ))

(defun custom-agenda-inbox ()
  "Opens the Inbox / Someday Agenda"
  (interactive)
  (progn
    (setq org-agenda-custom-commands
            '(("i" "Custom Agenda - Inbox"
               (
                ;;Get the Inbox
                (org-ql-block '(and (todo) (ancestors (heading "Inbox"))) ((org-ql-block-header "Inbox")) )
                ;;Get Someday Tasks
                (org-ql-block '(and (todo) (ancestors (heading "Someday"))) ((org-ql-block-header "Someday")) )
               )))
          org-super-agenda-groups '(
                                    (:name "Scheduled. Please move!" 
                                           :scheduled t
                                           :face (:foreground "spring green")
                                           )
                                    (:auto-category t)
                                    ))
    (org-agenda nil "i")
  ))


(defun custom-agenda-get-ahead ()
  "Opens a view for Next Actions that are scheduled in the future"
  (interactive)
  (progn
    (setq org-agenda-custom-commands
            '(("g" "Custom agenda - Next Actions -> Get Ahead"
               (
                ;;Get the items scheduled in the future, excluding habits.
                (org-ql-block '(or
                                 (and
                                   (todo)
                                   (ancestors (heading "Next Actions"))
                                   (scheduled :from 1)
                                   (not (habit))
                                  )
                                 (and 
                                   (habit)
                                   (scheduled :from 1)
                                   (not (regexp (get-habit-last-repeat-regex)))
                                  )
                                 )
                               ((org-ql-block-header "Get Ahead")))
                ;;Get the closed items for today, including habits
                (org-ql-block '(or
                                (and (todo "DONE") (closed :on today))
                                ;;Select Habits that are NOT completed today, but are scheduled in the future
                                (and (habit)
                                     (regexp (get-habit-last-repeat-regex) )
                                     )
                                )
                               ((org-ql-block-header "Closed")))
               )))
          
          org-super-agenda-groups shared-super-agenda-groups
          ;;NOTE: org-ql doesnt support prefix formatting. As of 5/11/24, they're working on it, but it looks like the only way to add it is via function advice.
          org-agenda-prefix-format '(
                                      (agenda . " %i %-12:c%?-12t%s")
                                      (timeline . " %i %-12:c%?-12t%s")
                                      (todo . " %i %-12:c%?-12t%s")
                                      (tags . " %i %-12:c%?-12t%s")
                                      (search . " %i %-12:c%?-12t%s")
                                      )
          )
    ;;Customize the agenda faces
    (set-face-attribute 'org-agenda-structure nil
                        :height 1.2
                        )
    (set-face-attribute 'org-super-agenda-header nil
                        :height 1.2
                        )
    (org-agenda nil "g")
    ))

(defun custom-agenda-next-actions ()
  "Opens the Next Actions / Delegate / Wait Agenda"
  (interactive)
  (progn
    ;;TODO: parse entries and update entry metadata
    (setq org-agenda-custom-commands
            '(("t" "Custom agenda - Next Actions for Today"
               (
                ;;Get only the items under "Next Actions"
                (org-ql-block '(and (todo) (ancestors (heading "Next Actions")) (or (scheduled :to today) (not (scheduled)))) ((org-ql-block-header "Next Actions (Today)" )) )
                ;;Get Delegated Tasks
                (org-ql-block '(and (todo) (ancestors (heading "Delegate"))) ((org-ql-block-header "Delegate")) )
                ;;Get the closed items for today, including habits
                (org-ql-block '(or
                                (and (todo "DONE") (closed :on today))
                                (and (habit) (regexp (get-habit-last-repeat-regex)))
                                )
                                ((org-ql-block-header "Closed"))
                                )
               )))
          
          org-super-agenda-groups shared-super-agenda-groups
          ;;NOTE: org-ql doesnt support prefix formatting. As of 5/11/24, they're working on it, but it looks like the only way to add it is via function advice.
          org-agenda-prefix-format '(
                                      (agenda . " %i %-12:c%?-12t%s")
                                      (timeline . " %i %-12:c%?-12t%s")
                                      (todo . " %i %-12:c%?-12t%s")
                                      (tags . " %i %-12:c%?-12t%s")
                                      (search . " %i %-12:c%?-12t%s")
                                      )
          )
    ;;Customize the agenda faces
    ;;TODO: eventually, make these temporary so we can save these per-agenda view
    (set-face-attribute 'org-agenda-structure nil
                        :height 1.2
                        )
    (set-face-attribute 'org-super-agenda-header nil
                        :height 1.05
                        )
    (org-agenda nil "t")
  ))
