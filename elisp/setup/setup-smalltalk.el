;;; setup-smalltalk.el --- Customizations for Smalltalk
;;; Commentary:
;;;   Sets up Smalltalk mode in line with a homebrew installation

;;; Code:

(defvar setup-smalltalk:smalltalk-elisp-dir
  "/usr/local/Cellar/gnu-smalltalk/3.2.5_1/share/emacs/site-lisp/"
  "Directory where the Smalltalk elisp files are located.")

(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
               auto-mode-alist))

(autoload
  'smalltalk-mode
  (concat setup-smalltalk:smalltalk-elisp-dir "smalltalk-mode.elc")
  ""
  t)

(autoload
  'gst
  (concat setup-smalltalk:smalltalk-elisp-dir "gst-mode.elc")
  ""
  t)

(add-hook 'smalltalk-mode-hook
          '(lambda ()
             (autopair-mode -1)))

(provide 'setup-smalltalk)
;;; setup-smalltalk.el ends here
