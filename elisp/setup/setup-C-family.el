;;; setup-C-family.el --- Setup for C/C++
;;; Commentary:
;;;   For now, sets up C/C++ autocomplete.

;;; Code:

(add-hook 'c-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-c-headers)
             (add-to-list 'ac-sources 'ac-source-clang)))

(add-hook 'c++-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-c-headers)
             (add-to-list 'ac-sources 'ac-source-clang)))

(provide 'setup-C-family)
;;; setup-C-family.el ends here
