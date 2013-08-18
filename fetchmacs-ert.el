(global-set-key (kbd "C-c <f5>")
                (lambda ()
                  (interactive)
                  (compile
                   (concat
                    "emacs -batch "
                    "-L \"" default-directory "\" "
                    "-l ert "
                    "-l \"fetchmacs-creds.el\" "
                    "-l \"fetchmacs-ert.el\" "
                    "-f ert-run-tests-batch-and-exit "))))

(require 'fetchmacs)
(require 'fetchmacs-creds)

(ert-deftest should-provision-key-pair ()
  (let ((user fetchmacs-my-email)
        (pass fetchmacs-my-pass))
    (fetchmacs-provision-keys-for-user user pass)
    (should-not (eql nil fetchmacs-public-key))
    (should-not (eql nil fetchmacs-private-key))
    (should-not (eql nil fetchmacs-author))))
