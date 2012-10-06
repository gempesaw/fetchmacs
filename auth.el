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




