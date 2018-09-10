;;; setup-scala.el --- Customizations for Scala
;;; Commentary:
;;;   Setup functions for scala-mode2

;;; Code:
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'scala-mode-hook 'rainbow-delimiters-mode)

(add-hook 'scala-mode-hook
          '(lambda ()
             ;; These are needed for complex scala files
             (setq max-lisp-eval-depth 50000)
             (setq max-specpdl-size 5000)

             (setq scala-indent:align-forms t)
             (setq scala-indent:indent-value-expression t)
             (setq scala-indent:align-parameters t)

             (local-set-key (kbd "<backtab>")
                            'scala-indent:indent-with-reluctant-strategy)
             (scala-mode:goto-start-of-code)))

(provide 'setup-scala)
;;; setup-scala.el ends here
