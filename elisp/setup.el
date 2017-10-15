;;; setup.el --- Entry point to setup
;;; Commentary:
;;;   This is the entry point to all the setup utilities in the setup
;;;   directory. Provides SETUP/SETUP-ALL which should be added to the
;;;   AFTER-INIT-HOOK. Also handles the installation of the necessary
;;;   packages

;;; Code:

(defun setup/setup-all ()
  "Wrapper to setup all of the packages defined in setup.

Intended to be added to the AFTER-INIT-HOOK"
  (require 'setup-ui)
  (require 'setup-utils)
  
  (require 'setup-haskell)
  (require 'setup-ocaml)
  (require 'setup-lisp)
  (require 'setup-clojure)
  (require 'setup-racket)
  ;; (require 'setup-scala)
  (require 'setup-latex)
  (require 'setup-markdown)
  (require 'setup-R)
  (require 'setup-python)
  (require 'setup-agda)
  (require 'setup-smalltalk)
  (require 'setup-C-family)
  (require 'setup-web-family))

(provide 'setup)
;;; setup.el ends here
