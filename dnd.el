;;; dnd.el --- D&D combat tracking for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Logan Barnett

;; Author: Logan Barnett <logustus@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (dash "2.0") (request "0.3") (org "9.0"))
;; Keywords: games, org
;; URL: https://github.com/LoganBarnett/org-dnd

;;; Commentary:
;; D&D combat tracking tools integrated with org-mode tables.

;;; Code:

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

;; Blatant theft from:
;; https://www.reddit.com/r/orgmode/comments/ot8g0j/orgnexttable_and_orgprevioustable_jump_to/
;; We should probably make an extended org-table function module, or add it to
;; an existing one.
(defun org-next-table (&optional arg)
  "Jump to the next table.

With a prefix argument ARG, jump forward ARG many tables."
  (interactive "p")
  (cl-loop
   for n below (abs arg)
   with backward = (< arg 0)
   with search-fn = (if backward #'re-search-backward #'re-search-forward)
   do
   (setq pt (point))
   (when (org-at-table-p)
     (funcall search-fn org-table-border-regexp nil :move))
   if (funcall search-fn org-table-line-regexp nil t) do
   (when (org-invisible-p)
     (org-reveal t)
     (org-show-entry)
     (unless (org-at-table-p)
       (cl-decf n)))
   else return (goto-char pt)
   finally (when backward
             (when (funcall search-fn org-table-border-regexp nil :move)
               (next-line))
             (forward-char))))

(defun org-previous-table (&optional arg)
  "Jump to the previous table.

With a prefix argument ARG, jump backward ARG many tables."
  (interactive "p")
  (org-next-table (- arg)))

;; End blatant theft.

;; More things that we should just have...

(defun org-table-get-column-index (column-name)
  "Get the index of COLUMN-NAME at current table."
  (let (
        (index (-find-index
               (lambda (column) (message "column: %s" column) (string= (string-trim column) column-name))
               (car (org-table-to-lisp))
               ))
        )
    (progn
      ;; (message "Found index for %s: %s" column-name index)
      ;; We aren't actually working with an index but a number.  They start
      ;; at 1.
      (+ 1 index)
      )
    )
  )

(defun dnd-table-populate ()
  (interactive)
  ;; Get the table we're on.  We need to save it so we can modify it later.
  (let ((original-table-location (point)))
    (-map-indexed
     ;; for each row
     (lambda (row-index row)
       ;; if it has a type
       (let ((kind (org-table-get row-index (org-table-get-column-index "kind"))))
         (message "row-index: %s kind: %s" row-index kind)
         (if (or (< row-index 2) (string-empty-p kind) (not kind))
             nil
           (progn
             (let (
                   (vals (save-window-excursion
                           (dnd/seek-profile row-index)
                           ;; (org-narrow-to-subtree)
                           (org-next-table 1)
                           (let (
                                 ;; TODO: Assuming these row and column indexes is
                                 ;; dangerous.  Look these up intelligently.
                                 (ac (org-table-get 2 2))
                                 (hp (org-table-get 3 4))
                                 (init (org-table-get 4 2))
                                 )
                             (message "Found ac: %s hp: %s init: %s" ac hp init)
                             (list ac hp init)
                             )
                           ;; Now that we are in the subtree, we find the first table.

                           ;; Get us an org-link from the type field (we should just
                           ;; have a function for this already).
                           ;; dnd/profile-path
                           ;; "creatures"
                           ;; kind
                           ;; Get information from the stat block:
                           ;; - Get the final iniative modifier.
                           ;; - Get the final armor class.
                           ;; - Get the hit points.
                           )
                         )
                   )
               ;; TODO: Use let binding to assign vars instead of using inline
               ;; indxes.
               (progn
                 (goto-char original-table-location)
                 (org-table-put
                  row-index
                  (org-table-get-column-index "ac")
                  (nth 0 vals)
                  )
                 (org-table-put
                  row-index
                  (org-table-get-column-index "chp")
                  (nth 1 vals)
                  )
                 (org-table-put
                  row-index
                  (org-table-get-column-index "thp")
                  (nth 1 vals)
                  )
                 (org-table-put
                  row-index
                  (org-table-get-column-index "ord")
                  (number-to-string
                   (+ (+ 1 (random 20)) (string-to-number (nth 2 vals)))
                   )
                  )
                 (org-table-align)
                 )
               )
             )
           )
         )
       )
     (org-table-to-lisp)
     )
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

(defun dnd/seek-profile (row)
  "Seek D&D profile at ROW."
  (let ((kind (org-table-get row (org-table-get-column-index "kind"))))
    (if (or (string-empty-p kind) (not kind))
        (progn
          (message "Column \"kind\" could not be found at row %s: %s" row kind)
        )
      (progn
        (org-link-open-from-string
         (format "[[file:%s::*%s][%s]]"
                 dnd/profile-path
                 kind
                 kind
                 ))
        )
      )
    ;; (org-mark-ring-goto)
    )
  )

(defun dnd//open-profile (row)
  "Open D&D profile at ROW for user viewing."
  (if
      (dnd/seek-profile row)
      (progn
        (org-show-entry)
        (select-window (previous-window))
        )
    nil
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

(provide 'dnd)
;;; dnd.el ends here
