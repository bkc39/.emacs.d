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
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-c C-m C-c") 'mc/edit-lines)

(add-hook
 'git-commit-mode-hook
 (lambda ()
   (interactive)
   (let* ((current-branch (magit-get-current-branch))
          (issue-number (infer-issue-number-from-branch-name current-branch)))
     (unless (issue-prefix-is-there)
       (goto-char (point-min))
       (insert "[#" issue-number "] ")))))

(defun infer-issue-number-from-branch-name (branch-name)
  nil)

(defun issue-prefix-is-there ()
  t)

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
