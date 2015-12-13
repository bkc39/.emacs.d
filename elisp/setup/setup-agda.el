;;; setup-agda.el --- Customizations for Agda
;;; Commentary:
;;;   Agda using agda2-mode as installed by cabal.

;;; Code:

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(add-hook 'agda2-mode-hook
         (lambda ()
           (customize-set-variable 'agda2-highlight-face-groups
                                   'default-faces)
           (customize-set-variable
            'agda2-include-dirs
            (list "/Users/bkc39/Documents/cs/agda/agda-stdlib/src" "."))))

(provide 'setup-agda)
;;; setup-agda.el ends here
