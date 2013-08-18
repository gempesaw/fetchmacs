(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (compile
                   (concat
                    "emacs -batch  -f ert-run-tests-batch-and-exit "
                    ;; "-L \"" default-directory "\ "
                    "-L ert "
                    "-L fetchmacs-ert.el "))))

(require 'fetchmacs)

(ert-deftest should-provision-key-pair ()
  (let ((user fetchmacs-dev-email)
        (pass (insert-file-contents ".creds")))
    (should (message "%s:%s" user pass))))
