(load-file "auth.el")

(defvar fetchmacs-all-notes nil)
(fetchmacs-provision-keys-for-user fetchmacs-user-email fetchmacs-user-pass)
(fetchmacs-get-notes-for-author fetchmacs-author)

(defun fetchmacs-get-notes-for-author (author)
  (let ((url (concat fetchmacs-hostname "authors/" author "/notes"))
        (response nil)
        (json-response-as-alist nil))
    (setq json-response-as-alist (fetchmacs-get-json-from-http-request url nil "GET"))
    (when (string= (cdr (assoc 'status json-response-as-alist)) 'success))
    (setq fetchmacs-all-notes (cdr (assoc 'response json-response-as-alist)))))



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
