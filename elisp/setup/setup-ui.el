;;; setup-ui.el --- Customizations for the Emacs UI
;;; Commentary:
;;;   General UI configs.
;;;

;;; Code:

;; Sets the default font
(if (member "Anonymous Pro" (font-family-list))
    (progn
      (add-to-list 'default-frame-alist '(font . "Anonymous Pro-9"))
      (set-face-attribute 'default nil :font "Anonymous Pro-9")
      (set-face-attribute 'default nil :height 100))
  (progn
    (let ((default-font
            (if (and (null
                      (string=
                       ""
                       (shell-command-to-string "which fc-list")))
                     (null
                      (string=
                       ""
                       (shell-command-to-string "fc-list 'Anonymous Pro'"))))
                "Anonymous Pro 9"
              "Monospace 9")))
      (progn
        (set-default-font default-font)
        (add-to-list 'default-frame-alist `(font . ,default-font))))))

;; Enables line numbers
(global-linum-mode 1)

;; Enables column numbers
(setq column-number-mode t)

;; Set window size to 80 characters
(add-to-list 'default-frame-alist '(width . 81))

;; Turn off the scroll bar.
(scroll-bar-mode -1)

;; Turn off the damn bell
(setq ring-bell-function 'ignore)

;; Turn off start message
(setq inhibit-startup-message t)

;; Turns off the tool bar
(tool-bar-mode -1)

;; Turn off the menu bar
(menu-bar-mode -1)

;; Color theme
(load-theme 'zenburn t t)
(enable-theme 'zenburn)

;; Spaces for tabs
(setq-default indent-tabs-mode nil)

;; Don't scroll when pointer reaches end of screen
(setq auto-window-vscroll nil)
(setq backup-inhibited t)

;; Prevent autoscroll from jumping
(setq scroll-conservatively 10000)

;; Sets the initial prompt
(setq
 initial-scratch-message
 ";; Appreciate every single person. Look at them like a golden,
;; million-dollar baby.
;;                    - Lil B 'The BASED God'")

;; Don't wrap lines
(setq-default truncate-lines t)

;; Accept 'y' and 'n' as answers to yes/no questions
(defalias 'yes-or-no-p 'y-or-n-p)
(defvar setup-ui:dabbrev-case-distinction)
(defvar setup-ui:dabbrev-case-fold-search)
(defvar setup-ui:windmove-wrap-around)

(setq setup-ui:dabbrev-case-distinction nil)
(setq setup-ui:dabbrev-case-fold-search nil)
(setq setup-ui:windmove-wrap-around t)
(setq echo-keystrokes 0.1)
(setq delete-active-region nil)

;; set C-x p to previous buffer
(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

;; Binds 'git status' to C-c m
(define-key global-map (kbd "C-c m") 'magit-status)
(advice-add
 #'magit-key-mode-popup-committing :after
 (lambda ()
   (magit-key-mode-toggle-option
    (quote committing)
    "--no-verify")))

(add-hook
 'git-commit-mode-hook
 (lambda ()
   (interactive)
   (let* ((current-branch (magit-get-current-branch))
          (issue-number (infer-issue-number-from-branch-name current-branch)))
     (unless (or (issue-prefix-is-there) (string= issue-number ""))
       (goto-char (point-min))
       (insert "[#" issue-number "] ")))))

(defun infer-issue-number-from-branch-name (branch-name)
  "Gets the implied issue number out of the current branch"
  (if (string-match "\\([[:alpha:]]+\\)\\([[:digit:]]+\\).*" branch-name)
      (match-string 2 branch-name)
    (progn
      (message "failed to infer branch name")
      "")))


(defun issue-prefix-is-there ()
  "Check if the buffer is prefixed by the issue prefix [#ISSUE-NUMBER]"
  (let ((buffer-prefix (car (split-string (buffer-string)))))
    (string-match "\\[\\#[[:digit:]]+\\]" buffer-prefix)))

;; globally enable company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq user-mail-address "bkc@botlab.trade")

;; always delete trailing whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

(provide 'setup-ui)
;;; setup-ui.el ends here
