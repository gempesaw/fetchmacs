(require 'url)
(require 'json)
(load-file "api-urls.el")

(defvar fetchmacs-public-key nil)
(defvar fetchmacs-private-key nil)
(defvar fetchmacs-author nil)

(defun fetchmacs-extract-json-from-http-response (buffer)
  (let ((json nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun fetchmacs-get-json-from-http-request (url args request-method)
  "Send ARGS to URL as a POST request; returns JSON body."
  (when (fetchmacs-request-needs-signature url)
    (let ((signature (fetchmacs-construct-signature args)))
      (add-to-list 'args `("signature" . ,signature))))
  (let ((url-request-method request-method)
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (fetchmacs-parse-json-as-alist
     (fetchmacs-extract-json-from-http-response
      (url-retrieve-synchronously url)))))

(defun fetchmacs-request-needs-signature (url)
  (not (string= "http://www.fetchnotes.com/keys" url)))

(defun fetchmacs-parse-json-as-alist (json)
  (let ((json-object-type 'alist))
    (json-read-from-string json)))

(defun fetchmacs-provision-keys-for-user (user password)
  "Sets the public key, private key, and author values from
fetchnotes"
  ;; TODO: change return of this function to say yes or no indicating
  ;; whether it succeeded...?
  (let ((url (concat fetchmacs-hostname fetchmacs-provision-keys-url))
        (args `(("name" . ,fetchmacs-project-name)
                ("email" . ,fetchmacs-dev-email)
                ("username" . ,user)
                ("password" . ,password))))
    (fetchmacs-set-keys-and-author
     (fetchmacs-get-json-from-http-request url args "POST"))))

(defun fetchmacs-set-keys-and-author (json-response-as-alist)
  (let ((response (cdr (assoc 'response json-response-as-alist)))
        (status (cdr (assoc 'status json-response-as-alist)))
        (errors (cdr (assoc 'errors json-response-as-alist))))
    (when (string= status 'success)
      (setq fetchmacs-public-key (cdr (assoc '_id response)))
      (setq fetchmacs-private-key (cdr (assoc 'private_key response)))
      (setq fetchmacs-author (cdr (assoc 'author response))))))

(defun fetchmacs-construct-signature (&optional request-params-alist)
  (if request-params-alist
      (progn
        (let ((sorted-alist (fetchmacs-sort-alist-by-car request-params-alist))
              (unhashed-signature nil)
              (sorted-values nil))
          (setq sorted-values (mapcar (lambda (key-value-pair)
                                        (concat (cdr (car key-value-pair)) sorted-values))
                                      sorted-alist))
          (sha1 (concat
                 fetchmacs-private-key (mapconcat 'identity sorted-values "")))))
    (sha1 fetchmacs-private-key)))

(defun fetchmacs-sort-alist-by-car (alist)
  (let ((params nil)
        (ordered-alist nil)
        (sorted-cars nil))
    (let ((cars (car (last
                      (mapcar (lambda (param)
                                (setq params (cons (car param) params)))
                              alist)))))
      (mapcar (lambda (key)
                (let ((value (cdr (assoc key alist))))
                  (append `((,key . ,value)) sorted-cars)))
              (sort cars 'string<))
      )))
