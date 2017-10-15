;;; setup-ui.el --- Customizations for the Emacs UI
;;; Commentary:
;;;   General UI configs.
;;;

;;; Code:

;; Sets the default font
(set-frame-font
 (cond
  ((member "Anonymous Pro" (font-family-list)) "Anonymous Pro 12")
  ((member "Monaco" (font-family-list)) "Monaco 12")
  (t (message "Desired font not found -- using default"))))

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
(color-theme-initialize)
(load-theme 'zenburn t)

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

;; turn this nonsense off. It screws up various modes indentation
(electric-indent-mode nil)

(setq user-mail-address "bkc@botlab.trade")

(provide 'setup-ui)
;;; setup-ui.el ends here
