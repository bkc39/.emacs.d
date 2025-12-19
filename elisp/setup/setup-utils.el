;;; setup-utils.el --- Setups for various Emacs utilities.
;;; Commentary:
;;;   Customizations for general utilities:
;;;     - ido-mode
;;;     - auto-complete-mode
;;;     - the emacs shell path
;;;     - the jabber shell path

;;; Code:

;; Sets the PATH environment variable
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-c C-m C-c") 'mc/edit-lines)

;; Jabber -- sets up gchat
(setq jabber-account-list
      (cons (list "pl.proofs@gmail.com"
                  '(:network-server . "talk.google.com")
                  '(:connection-type . ssl))
            jabber-account-list))

(message "end of setup-utils")

(provide 'setup-utils)
;;; setup-utils.el ends here
