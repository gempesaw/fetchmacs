(load-file "auth.el")

(defvar fetchmacs-all-notes nil)
(defvar fetchmacs-edit-buffer "*fetchmacs-edit-buffer*")
(defvar fetchmacs-view-notes-buffer "*fetchmacs-view-notes-buffer*")
(defvar fetchmacs-edit-action nil)
(defvar fetchmacs-buffer-internal nil)
(defvar fetchmacs-old-window-config nil)

(defun fetchmacs-get-notes-for-author (author)
  (let ((path (concat "authors/" author "/notes"))
        (response nil)
        (json-response-as-alist nil))
    (setq json-response-as-alist (fetchmacs-get-json-from-http-request path nil "GET"))
    (when (string= (cdr (assoc 'status json-response-as-alist)) 'success))
    (setq fetchmacs-all-notes (cdr (assoc 'response json-response-as-alist)))))

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
    (fetchmacs-edit-mode)))

(defun fetchmacs-save-note ()
  (interactive)
  (let ((path (concat "authors/" fetchmacs-author "/notes"))
        (note-body (buffer-substring (point-min) (point-max)))
        (args nil))
    (setq args `(("text" . ,note-body)))
    (fetchmacs-get-json-from-http-request path args "POST")
    (erase-buffer)
    (bury-buffer)
    (fetchmacs-restore-window-config)))

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

(defun fetchmacs-view-edit-note-at-point (&optional action)
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
    (make-local-variable fetchmacs-edit-action)
    (if (stringp action)
        (setq fetchmacs-edit-action action)
      (setq fetchmacs-edit-action "edit"))
    (fetchmacs-edit-mode)))
