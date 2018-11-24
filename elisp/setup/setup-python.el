;;; setup-python.el --- Customizations for Python
;;; Commentary:
;;;   Python with jedi enabled. Also setup IPython and matplotlib.

;;; Code:

(defvar setup-python:shell-python-command
  "python3"
  "Command to run python from the shell")

(defvar setup-python:python-framework-directory
  (replace-regexp-in-string
   "\n$"
   ""
   (shell-command-to-string
    (concat setup-python:shell-python-command
            " "
            "-m site --user-site")))
  "Path to the system version of Python.")

(defvar setup-python:jedi-emacs-pkg-dir
  (car (directory-files "~/.emacs.d/elpa" t "jedi-core-*"))
  "Directory where the jedi emacs package is installed

Defaults to install location in ~/.emacs.d/elpa")

(setq jedi:install-server--command
      `("pip3"
        "install"
        "--upgrade"
        ,setup-python:jedi-emacs-pkg-dir))

(setq jedi:server-args
      (list "--sys-path" setup-python:python-framework-directory))

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
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))

(provide 'setup-python)
;;; setup-python.el ends here
