;;; setup-utils.el --- Setups for various Emacs utilities.
;;; Commentary:
;;;   Customizations for general utilities:
;;;     - ido-mode
;;;     - auto-complete-mode
;;;     - flycheck-mode
;;;     - the emacs shell path
;;;     - the jabber shell path

;;; Code:

;; Sets the PATH environment variable
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(ido-mode t)

;; Auto complete
;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (autopair-global-mode 1)

;; Flycheck stuff
(custom-set-variables
 ;; Turn off the damn automatic checking.
 '(flycheck-check-syntax-automatically '(mode-enabled))
 '(flycheck-idle-change-delay 1000))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Jabber -- sets up gchat
(setq jabber-account-list
      (cons (list "pl.proofs@gmail.com"
                  '(:network-server . "talk.google.com")
                  '(:connection-type . ssl))
            jabber-account-list))

(message "end of setup-utils")

(provide 'setup-utils)
;;; setup-utils.el ends here
