(load-file "auth.el")

(defvar fetchmacs-all-notes nil)
(defvar fetchmacs-edit-buffer "*fetchmacs-edit-buffer*")
(defvar fetchmacs-buffer-internal nil)
(defvar fetchmacs-old-window-config nil)

(defun fetchmacs-get-notes-for-author (author)
  (let ((path (concat "authors/" author "/notes"))
        (response nil)
        (json-response-as-alist nil))
    (setq json-response-as-alist (fetchmacs-get-json-from-http-request path nil "GET"))
    (when (string= (cdr (assoc 'status json-response-as-alist)) 'success))
    (setq fetchmacs-all-notes (cdr (assoc 'response json-response-as-alist)))))

(defvar fetchmacs-mode-hook nil)
(put 'fetchmacs-mode 'mode-class 'special)
(defun fetchmacs-mode ()
  "View all of your fetchnotes and act on them.

\\{fetchmacs-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'fetchnotes-mode
        mode-name "Fetchnotes"
        mode-line-process "")
  (use-local-map fetchmacs-mode-map)
  (run-mode-hooks 'fetchmacs-mode-hook))

(defvar fetchmacs-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "n") 'fetchmacs-goto-next-note)
    (define-key map (kbd "p") 'fetchmacs-goto-previous-note)
    (define-key map (kbd "g") 'fetchmacs-refresh)

    (define-key map (kbd "c") 'fetchmacs-create-new-note)

    (define-key map (kbd "e") 'fetchmacs-edit-note-at-point)
    (define-key map (kbd "o") 'fetchmacs-edit-note-at-point)
    (define-key map (kbd "RET") 'fetchmacs-edit-note-at-point)

    (define-key map (kbd "/") 'fetchmacs-search)
    (define-key map (kbd "t") 'fetchmacs-filter-by-tag)

    (define-key map (kbd "d") 'fetchmacs-delete-note-at-point)

    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "z") 'kill-this-buffer)))


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

