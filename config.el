;;; config.el --- Core configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ansi-color)
(require 'package)
(require 'subr-x)

(defmacro add-hookq (hook-name fn)
  "Add FN to the list of functions to be run by HOOK-NAME."
  `(add-hook ',hook-name ,fn))

(defmacro package-install-if-not-there* (&rest packages)
  "Install PACKAGES if they are not already installed."
  (let ((pkg (gensym))
        (to-install (gensym)))
    `(let ((,to-install
            (cl-remove-if #'package-installed-p '(,@packages))))
       (unless (null ,to-install)
         (package-refresh-contents))
       (dolist (,pkg ,to-install)
         (package-install ,pkg)))))

(defmacro defun/who (name args &rest body)
  "Define a function NAME with arguments ARGS and body BODY.
Includes the function name as a local variable WHO within the body.

Special keyword arguments:
  :command -- If provided, make the function interactive."
  (let* ((docstring (if (stringp (car body)) (pop body)))
         (options (when (keywordp (car body)) (pop body)))
         (command (eq :command options)))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,@(when command '((interactive)))
       (let ((who ',name))
         ,@body))))

(defun indent-like-defun (sym)
  "Set the `lisp-indent-function` property of SYM to `defun`."
  (unless (get sym 'lisp-indent-function)
    (put sym 'lisp-indent-function 'defun)))

(indent-like-defun 'defun/who)

(defun/who expand-syscond-clause (clause)
  "Expand a single system type clause in a `syscond` macro."
  (let ((valid-system-types
         '(gnu
           gnu/linux
           gnu/kfreebsd
           darwin
           ms-dos
           windows-nt
           cygwin
           berkeley-unix
           aix
           hpux
           usg-unix-v)))
    (pcase clause
      (`(,x ,y . ,rest)
       (unless (memq x valid-system-types)
         (user-error "In %s during the expansion of %s: invalid system type %s"
                     'syscase
                     who
                     x))
       `((equal ',x system-type)
         (progn ,y ,@rest))))))

(defmacro syscase (&rest clauses)
  "Conditionally evaluate forms based on the system type."
  (let ((clause-expansions
         (mapcar #'expand-syscond-clause clauses)))
    `(cond
      ,@clause-expansions)))

(defmacro on-macos (&rest body)
  "Execute BODY if the current system is macOS."
  `(syscase (darwin ,@body)))

(defmacro on-linux (&rest body)
  "Execute BODY if the current system type is GNU/Linux."
  `(syscase (gnu/linux ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance and helpers

(defun my/company-face-tweaks ()
  (set-face-attribute
   'company-tooltip-selection nil
   :background nil
   :foreground "gray")
  (set-face-attribute
   'company-tooltip-common-selection nil
   :background nil
   :foreground "gray")
  (set-face-attribute
   'company-tooltip nil
   :background nil
   :foreground "gray")
  (set-face-attribute
   'company-tooltip-common nil
   :background nil
   :foreground "gray"))

(add-hookq after-load-theme-hook #'my/company-face-tweaks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org helpers

(defvar bkc-org-directory "~/.org"
  "Default value for ORG-DIRECTORY.")

(defvar bkc-default-notes "todo.org"
  "Default todo list file.")

(defun bkc-todo-file ()
  "Get the default value of the todo list file."
  (file-name-concat bkc-org-directory bkc-default-notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python / LSP helpers

(defun check-for-python-executable-in-dir (dir bin-name)
  (let ((executable (concat dir "/" bin-name)))
    (and (file-exists-p executable) executable)))

(defun organize-python-imports ()
  "Organize Python imports using lsp-pyright-organize-imports."
  (interactive)
  (when (and (eq major-mode 'python-mode) (bound-and-true-p lsp-mode))
    (lsp-pyright-organize-imports)))

(defun python-file-to-import-path ()
  "Return the Python import path of the current python file."
  (interactive)
  (let* ((filename (or buffer-file-name default-directory))
         (root (file-name-as-directory (getenv "MATE_ROOT"))))
    (if (and filename (string-match-p "\\.py\\'" filename))
        (let* ((relative (if (string-prefix-p root filename)
                             (substring filename (length root))
                           filename))
               (base (file-name-sans-extension relative))
               (import-path (concat "import twix." (replace-regexp-in-string "/" "." base))))
          (kill-new import-path)
          (message "%s" import-path)
          import-path)
      (user-error "Not visiting a .py file"))))

(defun reset-lsp-python-workspace (folder)
  "Reset LSP Python workspace to FOLDER."
  (interactive
   (let ((inferred-project-root
          (or (locate-dominating-file default-directory "venv")
              (locate-dominating-file default-directory ".git"))))
     (list (read-directory-name
            "Select new folder: "
            inferred-project-root))))
  (lsp-workspace-folders-remove (lsp-workspace-root))
  (lsp-workspace-folders-add folder)
  (lsp-restart-workspace))

(defun run-python-with-extra-pythonpaths (paths)
  "Run Python with additional PYTHONPATHS."
  (interactive
   (let ((default-path (lsp-workspace-root)))
     (if current-prefix-arg
         (list (read-directory-name
                "Specify additional PYTHONPATH: " default-path))
       (list default-path))))
  (unless (and (boundp 'pyvenv-virtual-env) pyvenv-virtual-env)
    (message "WARNING (%s): No virtual environment activated"
             'run-python-with-extra-pythonpaths))
  (setq python-shell-extra-pythonpaths
        (append (if (listp paths) paths (list paths))
                python-shell-extra-pythonpaths))
  (run-python)
  (python-shell-switch-to-shell
   "run-python-with-extra-pythonpaths: could not switch to python shell?"))

(defun rebind-run-python-hotkey ()
  "Rebind the run python hotkey to `run-python-with-extra-pythonpaths`."
  (define-key python-mode-map
              (kbd "C-c C-p")
              'run-python-with-extra-pythonpaths))

(defvar system-name-file "~/.vm-name")

(defun on-pinely-host ()
  "Check if we are on the pinely host."
  (interactive)
  (let ((absolute-path (expand-file-name system-name-file)))
    (if (not (file-exists-p absolute-path))
        (progn
          (warn "File not found: %s" absolute-path)
          nil)
      (let ((file-content (with-temp-buffer
                            (insert-file-contents absolute-path)
                            (string-trim (buffer-string)))))
        (string= file-content (system-name))))))

(when (and (on-pinely-host) (not (getenv "MATE_ROOT")))
  (setenv "MATE_ROOT" "/home/bkc/twix"))

(defun python-buffer-whitespace-cleanup ()
  "Clean up whitespace in the current Python buffer."
  (when (and (buffer-file-name)
             (string-match-p "\\.py'" (buffer-file-name)))
    (whitespace-cleanup)))

(defun try-locate-venv-named (venv-name)
  "Try to locate a virtual environment named VENV-NAME."
  (let ((dir
         (locate-dominating-file default-directory venv-name)))
    (when dir
      (concat (file-name-as-directory dir) venv-name))))

(defun activate-default-venv (venv-path)
  "Activate a Python virtual environment located at VENV-PATH."
  (interactive
   (list
    (read-directory-name
     "venv: "
     (or (try-locate-venv-named "venv")
         (try-locate-venv-named ".venv")
         default-directory))))
  (pyvenv-activate venv-path))

(defmacro make-search-venv-executable-function (name executables fallback)
  "Create a function NAME to search for the right executable in the current venv."
  `(defun ,name ()
     "Search for the right executable in the current virtual environment."
     (let ((venv-bin (concat (lsp-pyright--locate-venv) "/bin")))
       (or ,@(mapcar (lambda (exe)
                       `(check-for-python-executable-in-dir venv-bin ,exe))
                     executables)
           ,fallback))))

(make-search-venv-executable-function
 search-venv-for-python-executable
 ("ipython" "python3" "python")
 "python")

(make-search-venv-executable-function
 search-venv-for-pytest-watch-executable
 ("pytest-watch" "ptw")
 "ptw")

(make-search-venv-executable-function
 search-venv-for-black-executable
 ("black")
 "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General helpers and hooks

(defun slurp-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun read-file-into-string (file-path)
  "Return the contents of the file at FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(add-hookq
 after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))

(add-hookq
 tex-mode-hook
 (lambda ()
   (local-unset-key (kbd "C-c RET"))))

(add-hookq
 emacs-startup-hook
 (lambda ()
   (find-file-noselect user-init-file)))

(add-hookq
 python-mode-hook
 (lambda ()
   (when (on-pinely-host)
     (setq python-shell-interpreter "twix-python")
     (setq python-shell-interpreter-args "-m IPython --simple-prompt -i"))))

(defun restart-server ()
  "Restart the emacs server and close all clients."
  (interactive)
  (if server-process
      (progn
        (server-force-delete)
        (server-start))
    (message "Not in a client. Exiting...")
    'ok))

(defmacro with-defined-functions (symbols &rest body)
  "Execute BODY only if all SYMBOLS are bound as functions."
  (let ((cc (gensym)))
    `(catch ',cc
       (when (and
              ,@(mapcar
                 (lambda (sym)
                   `(or (fboundp ',sym)
                        (throw
                         ',cc
                         (format "%s is not bound as a function!" ',sym))))
                 symbols))
         ,@body))))

(defun pytest-watch ()
  "Run pytest in watch mode and display the output in a buffer."
  (interactive)
  (with-defined-functions
   (lsp-pyright--locate-venv lsp-workspace-root)
   (let ((buffer (get-buffer-create "*pytest-watch*"))
         (project-root (lsp-workspace-root)))
     (with-current-buffer buffer
       (read-only-mode -1)
       (erase-buffer))
     (start-process-shell-command
      "pytest-watch"
      buffer
      (concat
       "cd " project-root " && "
       (lsp-pyright--locate-venv)
       "/bin/pytest-watch --clear")))
   (with-current-buffer "*pytest-watch*"
     (read-only-mode 1)
     (display-buffer (current-buffer)))))

(defun pyright-watch ()
  "Run pyright and display the output in a buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*pyright*"))
         (pyright-path (executable-find "pyright"))
         (cmd (concat pyright-path
                      " --pythonpath "
                      (search-venv-for-python-executable)
                      " --watch")))
    (message cmd)
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer))
    (start-process-shell-command "pyright" buffer cmd)
    (display-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language helpers

(defun rust-run-with-args ()
  "Run with cargo run and additional command line arguments."
  (interactive)
  (let ((args
         (read-string "Command line args: ")))
    (rust--compile
     "%s run -- %s"
     rust-cargo-bin
     args)))

(defun setup-tide-mode ()
  "Hook to setup tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI tweaks and keybindings

(if (<= 29 (car (version-to-list emacs-version)))
    (global-display-line-numbers-mode 1)
  (global-linum-mode 1))
(setq column-number-mode t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)

(defun reread-current-buffer-from-disk ()
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "C-x C-R") 'reread-current-buffer-from-disk)

(defun set-frame-font-to-anonymous-pro ()
  "Set frame font to anonymous pro if GUI is present."
  (when (and (display-graphic-p)
             (x-list-fonts "Anonymous Pro"))
    (add-to-list 'default-frame-alist
                 '(font . "Anonymous Pro-12"))))

(set-frame-font-to-anonymous-pro)
(setq js-indent-level 2)

(defun copy-to-clipboard/macos (beg end)
  (interactive "r")
  (on-macos
   (shell-command-on-region beg end "pbcopy")
   (deactivate-mark)))

(defun/who xclip-buffer (selection-type)
  (if (memq selection-type '(primary secondary clipboard))
      (call-process-region
       (point-min) (point-max)
       "xclip" nil nil nil "-selection" (symbol-name selection-type))
    (message "%s: invalid selection type %s"
             who selection-type)))

(defun copy-to-clipboard/linux (beg end)
  "Copy the region from BEG to END to the system clipboard."
  (interactive "r")
  (on-linux
   (when (executable-find "xclip")
     (if (use-region-p)
         (let ((text (buffer-substring-no-properties beg end)))
           (with-temp-buffer
             (insert text)
             (xclip-buffer 'primary)
             (xclip-buffer 'clipboard)))
       (message "No region selected")))))

(defun clipboard+kill-ring-save (beg end)
  "Copy selection to kill ring and system clipboard."
  (interactive "r")
  (syscase
   (darwin
    (copy-to-clipboard/macos beg end))
   (gnu/linux
    (copy-to-clipboard/linux beg end)))
  (kill-ring-save beg end))

(global-set-key (kbd "M-w") 'clipboard+kill-ring-save)

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Issue / git helpers

(defun insert-issue-prefix ()
  (interactive)
  (unless (fboundp 'magit-get-current-branch)
    (require 'magit))
  (let* ((current-branch
          (magit-get-current-branch))
         (issue-number
          (infer-issue-number-from-branch-name current-branch)))
    (unless (or (issue-prefix-is-there)
                (string= issue-number ""))
      (goto-char (point-min))
      (insert (format "[#%s] " issue-number)))))

(defun infer-issue-number-from-branch-name (branch-name)
  "Gets the implied issue number out of BRANCH-NAME."
  (if (string-match "\\([[:alpha:]]+\\)\\([[:digit:]]+\\).*" branch-name)
      (match-string 2 branch-name)
    (progn
      (message "failed to infer branch name")
      "")))

(defun issue-prefix-is-there ()
  "Check if the buffer is prefixed by the issue prefix [#ISSUE-NUMBER]."
  (let ((buffer-prefix (car (split-string (buffer-string)))))
    (string-match "\\[\\#[[:digit:]]+\\]" buffer-prefix)))

(defun add-blacken-buffer-to-on-save-hook ()
  "Add 'blacken-buffer to 'before-save-hook' when appropriate."
  (when (or
         (not (string= (system-name) "vm-3d-bkc"))
         (and (string= (system-name) "vm-3d-bkc")
              (boundp 'file-local-variables-alist)
              (assoc 'owner file-local-variables-alist)
              (string= (cdr (assoc 'owner file-local-variables-alist)) "bkc")))
    (add-hook 'before-save-hook #'blacken-buffer nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org and project specifics

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "DONE")))

(defvar twix-mate-root-directory "/home/bkc/repos/rl_dml_slave/"
  "Root directory for twix mate project.")

(defvar twix-pytest-test-directories
  '("rl_research/networks/datasets/statarb/tests/")
  "List of directories to pass to pytest.")

(defun twix--pytest-build-command ()
  (format "MATE_ROOT=%s pytest %s"
          (shell-quote-argument twix-mate-root-directory)
          (mapconcat (lambda (dir)
                       (shell-quote-argument (expand-file-name dir twix-mate-root-directory)))
                     twix-pytest-test-directories " ")))

(defun twix--pytest-filter (proc string)
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert (ansi-color-apply string))
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))))))

(defun run-twix-pytest ()
  (interactive)
  (let ((buffer-name "*twix-pytest*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (let ((proc (start-process-shell-command "twix-pytest"
                                               (current-buffer)
                                               (twix--pytest-build-command))))
        (set-process-filter proc 'twix--pytest-filter))
      (display-buffer (current-buffer)))))

(defvar my/todo-org-file "~/todo.org"
  "Path to the Org file containing my TODO items.")

(defun my/open-todo-org-file ()
  "Open `my/todo-org-file', creating it if it doesn't exist, and pop to its buffer."
  (interactive)
  (let ((file (expand-file-name my/todo-org-file)))
    (find-file file)
    (pop-to-buffer (current-buffer))))

(defvar my/kill-file "~/.kill.txt")

(defun my/region-to-kill-txt (beg end)
  "Overwrite ~/kill.txt with the contents of the region from BEG to END."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties beg end))
        (kill-file (expand-file-name my/kill-file)))
    (with-temp-file kill-file
      (insert region-text))))

(when (executable-find "ocaml")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("923cf2ebe5d4f7cf6cccf3f00ab3e6bf8070c2bc385b6a93bdd2bb8317368628"
     "f87c86fa3d38be32dc557ba3d4cedaaea7bc3d97ce816c0e518dfe9633250e34"
     "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744"
     "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background nil :foreground "gray")))))

(provide 'config)

;;; config.el ends here
