;;; setup-python.el --- Customizations for Python
;;; Commentary:
;;;   Python with jedi enabled. Also setup IPython and matplotlib.

;;; Code:

(defvar setup-python:python-framework-directory
  "/Library/Frameworks/Python.framework/Versions/3.4/"
  "Path to the system version of Python.")

(add-hook 'python-mode-hook 'jedi:setup)

;; turn off warnings for the topevel setq's
(with-no-warnings
  ;; set up the python shell to be ipython
  (setq python-shell-completion-native nil)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i --matplotlib")
  (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (setq python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion")
  (setq python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n")
  (setq  python-shell-completion-string-code
         "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  
  ;; set up jedi
  ;; (setq jedi:server-command
  ;;       (list (concat setup-python:python-framework-directory
  ;;                     "lib/python3.4/site-packages/jediepcserver.py")))
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))

(provide 'setup-python)
;;; setup-python.el ends here
