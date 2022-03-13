;;; setup-lisp.el --- Setup for general lisps
;;; Commentary:
;;;   Setup for general lisp stuff. Just paredit for now

;;; Code:

(defun insert-begin-end-block ()
  (interactive)
  (insert "begin")
  (newline 2)
  (insert "end")
  (forward-line -1)
  (indent-according-to-mode))

(add-hook
 'lean-mode-hook
 #'(lambda ()
     (define-key
       lean-mode-map
       (kbd "M-SPC")
       #'company-complete)
     (define-key
       lean-mode-map
       (kbd "C-c C-e")
       #'insert-begin-end-block)))


(provide 'setup-lean)
;;; setup-lean.el ends here
