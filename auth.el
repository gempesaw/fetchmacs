(require 'url)
(require 'json)
(load-file "api-urls.el")

(defun fetchmacs-url-http-post (url args)
  "Send ARGS to URL as a POST request.

Returns JSON body."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&"))
        (buffer nil)
        (json nil))
    (setq buffer (url-retrieve-synchronously url))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))




