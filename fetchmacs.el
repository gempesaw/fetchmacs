(load-file "auth.el")

(defvar fetchmacs-all-notes nil)
(defvar fetchmacs-edit-buffer "*fetchmacs-edit-buffer*")
(defvar fetchmacs-view-notes-buffer "*fetchmacs-view-notes-buffer*")
(defvar fetchmacs-editing-existing-note-p nil)
(defvar fetchmacs-existing-note-id nil)
(defvar fetchmacs-buffer-internal nil)
(defvar fetchmacs-old-window-config nil)

(defun fetchmacs-get-notes-for-author (author)
  (setq fetchmacs-all-notes nil)
  (let ((path (concat "authors/" author "/notes"))
        (response nil)
        (json-response-as-alist nil))
    (setq json-response-as-alist (fetchmacs-get-json-from-http-request path nil "GET"))
    (when (string= (cdr (assoc 'status json-response-as-alist)) 'success)
      (setq fetchmacs-all-notes (cdr (assoc 'response json-response-as-alist))))))

(defvar fetchmacs-view-mode-hook nil)
(put 'fetchmacs-view-mode 'mode-class 'special)
(defun fetchmacs-view-mode ()
  "View all of your fetchnotes and act on them.

\\{fetchmacs-view-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'fetchmacs-view-mode
        mode-name "Fetchmacs"
        mode-line-process "Fetch")
  (use-local-map fetchmacs-view-mode-map)
  (run-mode-hooks 'fetchmacs-view-mode-hook))

(defvar fetchmacs-view-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "n") 'fetchmacs-goto-next-note)
    (define-key map (kbd "p") 'fetchmacs-goto-previous-note)
    (define-key map (kbd "g") 'fetchmacs-refresh)

    (define-key map (kbd "c") 'fetchmacs-create-new-note)

    (define-key map (kbd "e") 'fetchmacs-view-edit-note-at-point)
    (define-key map (kbd "o") 'fetchmacs-view-edit-note-at-point)
    (define-key map (kbd "RET") 'fetchmacs-view-edit-note-at-point)

    (define-key map (kbd "/") 'fetchmacs-search)
    (define-key map (kbd "t") 'fetchmacs-filter-by-tag)

    (define-key map (kbd "d") 'fetchmacs-delete-note-at-point)

    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "z") 'kill-this-buffer)
    map))


(defun fetchmacs-create-new-note ()
  (interactive)
  (fetchmacs-save-window-config)
  (let ((edit-buffer (get-buffer-create fetchmacs-edit-buffer)))
    (pop-to-buffer edit-buffer)
    (fetchmacs-edit-mode)
    (make-local-variable 'fetchmacs-editing-existing-note-p)
    (setq fetchmacs-editing-existing-note-p nil)))

(defun fetchmacs-save-note ()
  (interactive)
  (let ((path (concat "authors/" fetchmacs-author "/notes"))
        (note-body (buffer-substring (point-min) (point-max)))
        (save-response nil)
        (args nil))
    (if fetchmacs-editing-existing-note-p
        (setq path (concat path "/" fetchmacs-existing-note-id)))
    (setq args `(("text" . ,note-body)))
    (setq save-response (fetchmacs-get-json-from-http-request path args "POST"))
    (if (string= (cdr (assoc 'status save-response)) 'success)
        (let ((fetchmacs-single-note
               (cdr (assoc 'response save-response)))
              (note-id nil))
          (setq note-id (cdr (assoc '_id fetchmacs-single-note)))
          (setq fetchmacs-all-notes
                (vconcat (vector fetchmacs-single-note)
                         (remove* note-id fetchmacs-all-notes
                                  :key 'cdar
                                  :test 'string=)))))
    (erase-buffer)
    (bury-buffer)
    (fetchmacs-restore-window-config)
    (fetchmacs-view-notes)))

(defvar fetchmacs-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'fetchmacs-save-note)
    (define-key map (kbd "C-c C-k") 'fetchmacs-cancel-edit)
    (define-key map (kbd "C-x C-s") (lambda ()
                                      (interactive)
                                      (message "Not saved. Use C-c C-c to save this note.")))
    map))

(define-derived-mode fetchmacs-edit-mode text-mode "Fetchmacs Edit")

(defun fetchmacs-cancel-edit ()
  (interactive)
  (erase-buffer)
  (make-local-variable 'fetchmacs-editing-existing-note-p)
  (setq fetchmacs-editing-existing-note-p nil)
  (bury-buffer)
  (fetchmacs-restore-window-config))

(defun fetchmacs-save-window-config ()
  (setq fetchmacs-old-window-config
        (current-window-configuration)))

(defun fetchmacs-restore-window-config ()
  (when fetchmacs-old-window-config
    (set-window-configuration fetchmacs-old-window-config)
    (setq fetchmacs-old-window-config nil)))

(defun fetchmacs-print-notes (&optional filter)
  (when fetchmacs-all-notes
    (mapc (lambda (single-note)
            (let ((note-text (cdr (assoc 'text single-note)))
                  (buffer (current-buffer))
                  (begin-note (point)))
              (if (and (stringp filter))
                  (when (string-match-p filter note-text)
                    (princ note-text buffer)
                    (put-text-property begin-note (point) 'note-properties single-note)
                    (princ "\n----\n" buffer))
                (princ note-text buffer)
                (put-text-property begin-note (point) 'note-properties single-note)
                (princ "\n----\n" buffer))))
          fetchmacs-all-notes)))

(defun fetchmacs-view-notes (&optional filter)
  (interactive)
  (if (and (stringp fetchmacs-private-key)
           (vectorp fetchmacs-all-notes))
      (let ((view-buffer (get-buffer-create fetchmacs-view-notes-buffer)))
        (pop-to-buffer view-buffer)
        (save-excursion
          (setq buffer-read-only nil)
          (erase-buffer)
          (fetchmacs-print-notes filter)
          (fetchmacs-view-mode)))
    (progn
      (fetchmacs-provision-keys-for-user fetchmacs-user-email fetchmacs-user-pass)
      (fetchmacs-get-notes-for-author fetchmacs-author)
      (message "yo shit broke, let's try that again")
      (if (and (stringp fetchmacs-private-key)
               (vectorp fetchmacs-all-notes))
          (fetchmacs-view-notes filter)
        (message "hmm, something's wrong. sorry.")))))

(defun fetchmacs-refresh ()
  (interactive)
  (if (equal (current-buffer) (get-buffer-create fetchmacs-view-notes-buffer))
      (progn
        (fetchmacs-get-notes-for-author fetchmacs-author)
        (fetchmacs-view-notes)
        (message "Refreshed your notes!"))
    (message "Refusing to refresh, try calling fetchmacs-refresh from within a fetchmacs-view-mode buffer.")))

(defun fetchmacs-goto-previous-note ()
  (interactive)
  (when (equal (current-buffer) (get-buffer-create fetchmacs-view-notes-buffer)))
    (beginning-of-line 1)
    (search-backward "----")
    (next-line -1 1)
    (beginning-of-line 1))

(defun fetchmacs-goto-next-note ()
  (interactive)
  (when (equal (current-buffer) (get-buffer-create fetchmacs-view-notes-buffer)))
    (beginning-of-line 1)
    (search-forward "----")
    (next-line 1 1)
    (beginning-of-line 1))

(defun fetchmacs-view-edit-note-at-point ()
  (interactive)
  (fetchmacs-save-window-config)
  (let ((note-properties (get-text-property (point) 'note-properties))
        (note-id-to-edit nil)
        (old-note-text nil))
    (setq note-id-to-edit (cdr (assq '_id note-properties)))
    (setq old-note-text (cdr (assq 'text note-properties)))
    (pop-to-buffer (get-buffer-create fetchmacs-edit-buffer))
    (erase-buffer)
    (insert old-note-text)
    (fetchmacs-edit-mode)
    (make-local-variable 'fetchmacs-editing-existing-note-p)
    (setq fetchmacs-editing-existing-note-p t)
    (make-local-variable 'fetchmacs-existing-note-id)
    (setq fetchmacs-existing-note-id note-id-to-edit)))

(defun fetchmacs-delete-note-at-point ()
  (interactive)
  (if (y-or-n-p "Are you sure you want to delete this note? ")
      (let ((note-properties (get-text-property (point) 'note-properties))
            (path (concat "authors/" fetchmacs-author "/notes/"))
            (args '(("delete" . "true")))
            (delete-response nil)
            (note-id nil))
        (setq note-id (cdr (assq '_id note-properties)))
        (setq path (concat path note-id))
        (message path)
        (setq delete-response (fetchmacs-get-json-from-http-request path args "POST"))
        (if (string= (cdr (assoc 'status delete-response)) 'success)
            (setq fetchmacs-all-notes
                  (remove* note-id fetchmacs-all-notes
                           :key 'cdar
                           :test 'string=)))
        (fetchmacs-view-notes))))

(defun fetchmacs-filter-by-tag ()
  (interactive)
  (save-excursion
    (beginning-of-thing 'word)
    (if (string= "#" (string (preceding-char)))
        (fetchmacs-view-notes (thing-at-point 'word)))
    (fetchmacs-search)))

(defun fetchmacs-search ()
  (interactive)
    (fetchmacs-view-notes (read-string "What do you want to search for? "))))
