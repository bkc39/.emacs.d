;;; setup-R.el --- Customizations for R
;;; Commentary:
;;;   Customizations to R using ESS mode

;;; Code:

(autoload 'R-mode "ess-site.el" "" t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))

(add-hook 'R-mode-hook
          (lambda ()
            ;; for some reason this gets set to 4 somewhere
            (setq ess-indent-level 2)
            ;; also gets set to electric-indent for some reason
            (local-set-key (kbd "C-j") 'newline-and-indent)
            ;; disable '_' ==> '<-'
            (local-unset-key "_")
            (ess-toggle-underscore nil)
            (local-set-key (kbd "C-c {")
                           #'(lambda ()
                               (interactive)
                               (insert "{")
                               (newline 2)
                               (insert "}")
                               (ess-indent-or-complete)
                               (forward-line -1)
                               (ess-indent-or-complete)))))

(custom-set-variables
 '(ess-R-font-lock-keywords
   (quote ((ess-R-fl-keyword:modifiers  . t)
           (ess-R-fl-keyword:fun-defs   . t)
           (ess-R-fl-keyword:keywords   . t)
           (ess-R-fl-keyword:assign-ops . t)
           (ess-R-fl-keyword:constants  . t)
           (ess-fl-keyword:fun-calls    . t)
           (ess-fl-keyword:numbers      . t)
           (ess-fl-keyword:operators    . t)
           (ess-fl-keyword:delimiters   . t)
           (ess-fl-keyword:=)
           (ess-R-fl-keyword:F&T)))))

(provide 'setup-R)
;;; setup-R.el ends here
