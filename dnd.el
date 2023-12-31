     (defvar dnd/combat-table-template
       "| turn | name | kind | ord | ac | chp | thp | spec | dist |
     |------+------+------+-----+----+-----+-----+------+------|"
       "Template to use when creating combat table."
       )

     (defun dnd/create-combat-table ()
       "Inserts a combat table into the document."
       (interactive)
       (let ((start (point)))
         (insert dnd/combat-table-template)
         (indent-region start (point))
         )
       )

     (defvar dnd/turn-functions '()
       "A hook for arriving upon a turn. Provides a ROW."
       )

     (defcustom dnd/turn-indicator-string ">>>>" "String to use for the current turn.")

      (defun dnd//set-turn (row)
        (org-table-put row 1 dnd/turn-indicator-string t)
        (org-table-goto-line row)
        ;; It would be nice to send the row data, but alas.
        (run-hook-with-args 'dnd/turn-functions row)
        )

     (defun dnd//get-table-lines ()
       (let* (
              (beg (org-table-begin))
              (end (copy-marker (org-table-end)))
              )
         (- (length (split-string
                     (filter-buffer-substring beg end)
                     "\n"
                     t
                     )
                    )
            3))
       )

     (defun dnd//walk-column-for-indicator (lines line)
       (message "value %s" (org-table-get (+ 2 line) 1))
       (if (< line (+ 1 lines))
           (if (string-equal (org-table-get (+ 2 line) 1) dnd/turn-indicator-string)
               line
             (dnd//walk-column-for-indicator lines (+ 1 line))
             )
         nil
         )
       )

     (defun dnd//add-indicator (line)
       (org-table-put line 1 dnd/turn-indicator-string)
       2
       )


     (defun dnd/find-turn-indicator ()
       (if (org-at-table-p)
           (let* (
                  (lines (dnd//get-table-lines))
                  (indicator (dnd//walk-column-for-indicator lines 0))
                  )
             (message "indicator? %s" indicator)
             (if indicator (+ 2 indicator) nil)
             )
         nil
         )
       )

     (defun dnd/next-round ()
       (interactive)
       (if (org-at-table-p)
           (let* (
                  (indicator (dnd/find-turn-indicator))
                  (next (+ (or indicator 0) 1))
                  (lines (dnd//get-table-lines))
                  )
             (if indicator
                 (progn
                   (org-table-put indicator 1 "" t)

                   (if (< next (+ 3 lines))
                       (progn
                         (dnd//set-turn next)
                         )
                     (progn
                       (dnd//set-turn 2)
                       )
                     )
                   )
               (progn

                 (dnd//set-turn 2)
                 )
               )
             )
         (message "Not on a table!")
         )
       )

     (defvar dnd/profile-path (expand-file-name "~/Dropbox/notes/dnd5e.org"))

    (defvar dnd/name-column 2)
    (defvar dnd/kind-column 3)

      (defun dnd//open-profile (row)
        (let ((kind (org-table-get row dnd/kind-column)))
          (if (string-empty-p kind)
              nil
              (progn
                (org-link-open-from-string
                 (format "[[file:%s::*%s][%s]]"
                         dnd/profile-path
                         kind
                         kind
                         ))
                (org-show-entry)
                (select-window (previous-window))
                )
            )
          ;; (org-mark-ring-goto)
          )
        )

     (add-hook 'dnd/turn-functions #'dnd//open-profile)

    ;; Let's not inflict Spacemacs upon others. But we can be kind to those that have it.
    (defmacro dnd/on-spacemacs (&rest body)
      "Execute BODY if this Emacs is running Spacemacs."
      (if (boundp 'spacemacs-version)
        `(progn ,@body)
        nil
        )
      )
    (dnd/on-spacemacs
      (spacemacs/declare-prefix-for-mode 'org-mode "mD" "dnd" "dnd")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode (kbd "D n") 'dnd/next-round)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode (kbd "D c") 'dnd/create-combat-table)
      )
