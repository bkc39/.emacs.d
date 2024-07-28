;;; init.el --- emacs init file -*- lexical-binding: t; -*-

;; URL: https://github.com/bkc39/.emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Init file using straight.el and use-package.

;;; Code:

(defmacro on-system (system &rest body)
  "Execute BODY only if SYSTEM matches the current system type."
  `(when (eq system-type ',system)
     ,@body))

(defmacro on-macos (&rest body)
  "Execute BODY if the current system is macOS."
  `(on-system darwin ,@body))

(defmacro on-linux (&rest body)
  "Execute BODY if the current system is Linux."
  `(on-system 'gnu/linux ,@body))

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
         (command (progn
                    (message "options was: %s" options)
                    (eq :command options))))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,@(when command '((interactive)))
       (let ((who ',name))
         ,@body))))

(unless (get 'defun/who 'lisp-indent-function)
  (put 'defun/who 'lisp-indent-function 'defun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; configure straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat
          "https://raw.githubusercontent.com"
          "/radian-software/straight.el/develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag)

(use-package aweshell
  :straight (:host github
                   :repo "manateelazycat/aweshell"
                   :files ("*.el" "dist"))
  :bind ("C-c x" . #'aweshell-dedicated-toggle))

(use-package blacken
  :ensure t
  :config
  (setq blacken-line-length 80)
  :hook (python-mode . blacken-mode))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3))

(use-package company-coq
  :after (proof-general)
  :hook (coq-mode . company-coq-mode)
  :init (setq company-coq-live-on-the-edge t))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'lsp-deferred)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'cider-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (define-key cider-mode-map (kbd "C-c RET") nil)
  ;; ^ delete the default key because we have that bound to gptel-send
  (define-key cider-mode-map
              (kbd "C-c C-r RET")
              'cider-macroexpand-1))

(use-package cmake-mode)

(use-package ein)

(use-package eshell-prompt-extras
  :config
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package ess
  :mode ("\\.R\\'" . R-mode))

(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns))
          (memq system-type '(darwin gnu/linux)))
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package go-mode
  :after lsp-mode
  :hook (go-mode . (lambda ()
                     (electric-pair-mode 1)))
  :config
  (if (executable-find "gopls")
      (add-hookq go-mode-hook #'lsp-deferred)
    (message "go-mode LSP plugin gopls is not installed!")))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "gpt-4o"
        gptel-api-key (get-openai-api-key)
        gptel-directives (or (read-prompt-md-files "~/.llm-prompts")
                             gptel-directives))
  (setq-default
   gptel--system-message
   (alist-get 'default gptel-directives "You are a helpful assistant."))
  (gptel-make-ollama
      "Ollama"
    :host "localhost:11434"
    :models '("llama3:8b" "codellama:latest" "codellama:13b" "gemma2:9b")
    :stream t)
  :bind (("C-c RET" . gptel-send)
         ("C-c q" . gptel-quick)
         ("C-c M-d" . gptel-diff)
         ("C-c M-p" . gptel-pull-request)
         ("C-c M-s" . gptel-document-symbol-at-point)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (js-mode . (lambda ()
                     (electric-indent-mode -1)
                     (lsp-deferred)))
  :hook (rust-mode . #'lsp-deferred)
  :custom
  (lsp-file-watch-ignored
   '("[/\\\\]\\.venv$" "[/\\\\]venv$" "[/\\\\]\\.direnv$"
     "[/\\\\]\\.mypy_cache$" "[/\\\\]\\.pytest_cache$"
     "[/\\\\]__pycache__$" "[/\\\\]build$" "[/\\\\]dist$"))
  :config
  (setq lsp-enable-file-watchers nil
        lsp-file-watch-threshold 10000))


(defun check-for-python-executable-in-dir (dir bin-name)
  (let ((executable (concat dir "/" bin-name)))
    (and (file-exists-p executable) executable)))

(defun organize-python-imports ()
  "Organize Python imports using lsp-pyright-organize-imports."
  (interactive)
  (when (and (eq major-mode 'python-mode) (bound-and-true-p lsp-mode))
    (lsp-pyright-organize-imports)))

(defun reset-lsp-python-workspace (folder)
  "Reset LSP Python workspace.

User can provide a FOLDER, defaulting to the result of
`locate-dominating-file` which looks for the nearest 'venv' or '.git'
directory."
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
  "Run Python with additional PYTHONPATHS.
Optionally prompt for user-specified PATHS if prefix argument is supplied."
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
  "Rebind the the run python hotkey.

`run-python` ==> `run-python-with-extra-pythonpaths`."
  (define-key python-mode-map
              (kbd "C-c C-p")
              'run-python-with-extra-pythonpaths))



(use-package lsp-pyright
  :hook (python-mode . lsp-deferred)
  :hook (before-save . organize-python-imports)
  :hook (python-mode . rebind-run-python-hotkey)
  :config
  (progn
    (setq
     ;; python-shell-interpreter (search-venv-for-python-executable)
     blacken-executable (search-venv-for-black-executable))
    (setq lsp-pyright-use-library-code-for-types t)
    (let* ((pyright-stubs-root-dir
            (getenv "PYRIGHT_TYPE_STUBS_ROOT"))
           (pyright-stubs-dir
            (concat pyright-stubs-root-dir
                    "/python-type-stubs")))
      (when (and pyright-stubs-root-dir
                 pyright-stubs-dir)
        (setq lsp-pyright-stubs-path
              pyright-stubs-dir)))))

(defun try-locate-venv-named (venv-name)
  "Try to locate a virtual environment named VENV-NAME.

Looks in the current or any parent directory.  Return the full path
to the virtual environment if found, or nil otherwise."
  (let ((dir
         (locate-dominating-file default-directory venv-name)))
    (when dir
      (concat (file-name-as-directory dir) venv-name))))

(defun activate-default-venv (venv-path)
  "Activate a Python virtual environment located at VENV-PATH.

If called interactively, prompt the user for the virtual
environment directory.  If a 'venv' or '.venv' virtual
environment is found in the current or any parent directory, use
that as the default suggestion."
  (interactive
   (list
    (read-directory-name
     "venv: "
     (or (try-locate-venv-named "venv")
         (try-locate-venv-named ".venv")
         default-directory))))
  (pyvenv-activate venv-path))

(use-package pyvenv
  :ensure t
  :after (lsp-pyright)
  :config
  (pyvenv-mode t)
  (setenv "WORKON_HOME"
          (expand-file-name "~/.python-virtual-envs"))
  (setq pyvenv-post-activate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter
                 (concat pyvenv-virtual-env "/bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list
         (lambda ()
           (setq python-shell-interpreter "python3"))))
  (define-key python-mode-map (kbd "C-c v a") #'activate-default-venv))


(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (on-macos
   (setq lsp-sourcekit-executable
         (string-trim
          (shell-command-to-string
           "xcrun --find sourcekit-lsp")))))

(use-package magit
  :bind (("C-c m" . magit-status))
  :hook ((git-commit-mode . insert-issue-prefix)))

(use-package multiple-cursors
  :bind (("C-c M-c" . mc/edit-lines)))

(use-package paredit
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)
    (add-hook 'racket-mode-hook #'enable-paredit-mode))
  :bind
  (:map paredit-mode-map
        ("{"     . paredit-open-curly)
        ("}"     . paredit-close-curly)
        ("C-M-[" . paredit-forward-slurp-sexp)
        ("C-M-]" . paredit-forward-barf-sexp)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda ()
                           (display-line-numbers-mode -1))))

(use-package proof-general
  :hook (coq-mode . prettify-symbols-mode)
  :custom
  (coq-prog-args
   (list
    "-emacs"
    "-R"
    (or (getenv "CPDT_SRC_DIR")
        "/Users/bkc/dev/coq/cpdt/src")
    "Cpdt")))

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :hook (racket-mode . (lambda ()
                         (racket-xp-mode t)))
  :config
  (racket-unicode-input-method-enable)
  (add-hookq racket-xp-mode-hook
             (lambda ()
               (remove-hook 'pre-redisplay-functions
                            #'racket-xp-pre-redisplay
                            t))))


(defun rust-run-with-args ()
  "Run with cargo run and additional command line arguments"
  (interactive)
  (let ((args
         (read-string "Command line args: ")))
    (rust--compile
     "%s run -- %s"
     rust-cargo-bin
     args)))

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (electric-pair-mode 1)))
  :custom
  (rust-format-on-save t)
  :bind (("C-c C-c m l" . #'lsp-rust-analyzer-open-cargo-toml)
         ("C-c C-c C-n" . #'rust-run-with-args)))

(use-package react-snippets)

(use-package swift-mode
  :hook (swift-mode . #'lsp-deferred))

(use-package solidity-mode)
;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

(defun setup-tide-mode ()
  "hook to setup tide"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package web-mode
  :mode ("\\.tsx\\'" . web-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines trailing empty))
  :hook (prog-mode . whitespace-mode)
  :hook (before-save . whitespace-cleanup))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package yasnippet
  :hook (js-mode . yas-minor-mode)
  :hook (lsp-mode . yas-minor-mode))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t t)
    (enable-theme 'zenburn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other configs

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

;; set the font to anonymous pro when in GUI if its installed
(defun set-frame-font-to-anonymous-pro ()
  "set frame font to anonymous pro if GUI is there"
  (when (and (display-graphic-p)
             (x-list-fonts "Anonymous Pro"))
    (add-to-list 'default-frame-alist
                 '(font . "Anonymous Pro-12"))))

(set-frame-font-to-anonymous-pro)
(setq js-indent-level 2)

(defun copy-to-clipboard/macos (beg end)
  (interactive "r")
  (when (eq system-type 'darwin)
   (shell-command-on-region beg end "pbcopy"))
  (deactivate-mark))

(defun clipboard+kill-ring-save (beg end)
  "Copies selection to x-clipboard."
  (interactive "r")
  (copy-to-clipboard/macos beg end)
  (kill-ring-save beg end))

(global-set-key (kbd "M-w") 'clipboard+kill-ring-save)

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(add-hookq
 after-change-major-mode-hook
 #'(lambda ()
     (electric-indent-mode -1)))

;; open up the init file in the background on startup
(add-hookq
 emacs-startup-hook
 (lambda ()
   (find-file-noselect user-init-file)))

;; (add-hookq after-make-frame-functions
;;            #'set-frame-font-to-anonymous-pro)

(defun restart-server ()
  "Restart the emacs server and close all clients"
  (interactive)
  (if server-process
      (progn
        (server-force-delete)
        (server-start))
    (message "Not in a client. Exiting...")
    'ok))

(defun load-openai-api-key ()
  "Read the OpenAI API key from the ~/.openai file and store it in the
environment variable OPENAI_API_KEY."
  (let ((api-key-file (expand-file-name "~/.openai")))
    (if (file-readable-p api-key-file)
        (with-temp-buffer
          (insert-file-contents api-key-file)
          (let ((api-key (string-trim (buffer-string))))
            (setenv "OPENAI_API_KEY" api-key)
            (message "OpenAI API key loaded successfully.")))
      (message "Error: ~/.openai file not found or is not readable."))))

(defun get-openai-api-key ()
  "Get the OpenAI API key from the environment variable OPENAI_API_KEY.
If the environment variable is not defined, load the key from the
~/.openai file.  Return the API key as a string."
  (let ((api-key (getenv "OPENAI_API_KEY")))
    (unless api-key
      (load-openai-api-key)
      (setq api-key (getenv "OPENAI_API_KEY")))
    api-key))

(defmacro with-defined-functions (symbols &rest body)
  "Execute BODY only if all SYMBOLS are bound as functions.

If any are not then display an error message with the first
undefined symbol."
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

(defmacro make-search-venv-executable-function (name executables fallback)
  "Create a function NAME to search for the right executable in the current
virtual environment.

EXECUTABLES is a list of executable names to search for.
FALLBACK is the fallback executable if none of the EXECUTABLES are found."
  `(defun ,name ()
     "Search for the right executable in the current virtual environment."
     (let ((venv-bin (concat (lsp-pyright--locate-venv) "/bin")))
       (or ,@(mapcar (lambda (exe)
                       `(check-for-python-executable-in-dir venv-bin ,exe))
                     executables)
           ,fallback))))

;; Usage
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

(defun slurp-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(defun read-prompt-md-files (directory)
  "Read files in DIRECTORY of the form PROMPT.md.

Returns an alist (PROMPT . CONTENTS-OF-FILE)."
  (let ((files (directory-files directory t "\\`[^.].*\\.md\\'"))
        result)
    (dolist (file files)
      (when (string-match "\\(.*\\)\\.md\\'" (file-name-nondirectory file))
        (let ((prompt (match-string 1 (file-name-nondirectory file)))
              (contents (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
          (push (cons (intern prompt) contents) result))))
    result))

(defun gptel-request/require (&rest args)
  "Ensure gptel-request is bound, require 'gptel if not, then call
gptel-request with ARGS."
  (unless (fboundp 'gptel-request)
    (require 'gptel))
  (apply #'gptel-request args))

(defun ensure-gptel-directives-loaded ()
  "Ensure that `gptel-directives` is defined."
  (unless (boundp 'gptel-directives)
    (setq gptel-directives (or (read-prompt-md-files "~/.llm-prompts") '()))))

(defvar gptel-quick--history nil
  "History list for `gptel-quick' prompts.")

(defun dynamic-prompt (make-prompt callback gptel-request-args)
  "Send a dynamically calculated request to an LLM.

MAKE-PROMPT is a thunk to calculate the prompt.  CALLBACK is an arity 2
function whose arguments are the LLM response and info which is passed
to gptel-request.  The of the GPTEL-REQUEST-ARGS are forwarded to the call to
gptel-request"
  (let* ((prompt
          (funcall make-prompt))
         (request-args
          (cons prompt
                (append (funcall gptel-request-args)
                        (list :callback callback)))))
    ;; (message "prompt: %s" prompt)
    ;; (message "request args: %s" request-args)
    (ensure-gptel-directives-loaded)
    (apply #'gptel-request/require request-args)))

(defmacro defgptelfn (name args &rest stx)
  "Define a function with dynamic prompt and body.
NAME is the function name. ARGS are the arguments taken by the function.
:command is the interactive command. :prompt is the prompt body.
:body is the body of the function. :extra-args is for additional request arguments."
  (let ((docstring
         (when (stringp (car stx))
           (pop stx)))
        (stx-kwargs (-partition 2 stx))
        (make-prompt (gensym))
        (request-callback (gensym))
        (request-args-thunk (gensym)))
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,@(awhen (assoc :command stx-kwargs)
           `((interactive ,(cadr it))))
       (let ((,make-prompt
              (lambda ()
                ,(aif (assoc :prompt stx-kwargs)
                     (cadr it)
                   (error
                    "defgptelfn: missing required keyword argument :prompt"))))
             (,request-callback
              (lambda (*gptel-response* *gptel-response-info*)
                ,(aif (assoc :body stx-kwargs)
                     (cadr it)
                   (error
                    "defgptelfn: missing required keyword argument :body"))))
             (,request-args-thunk
              (lambda ()
                ,@(aif (assoc :extra-args stx-kwargs)
                      it
                    (list nil)))))
         (dynamic-prompt
          ,make-prompt
          ,request-callback
          ,request-args-thunk)))))

(defgptelfn gptel-quick (prompt)
  "Send PROMPT to ChatGPT and display the response in a special buffer.
If the PROMPT is empty, signals a user error."
  :command (list (read-string "Ask ChatGPT: " nil gptel-quick--history))
  :prompt (progn
            (if (string= prompt "")
                (user-error "A prompt is required")
              prompt))
  :body (progn
          (if (not *gptel-response*)
              (message "gptel-quick failed with message: %s"
                       (plist-get *gptel-info* :status))
            (with-current-buffer (get-buffer-create "*gptel-quick*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert *gptel-response*))
              (special-mode)
              (display-buffer (current-buffer)))))
  :extra-args (list :system
                    (alist-get 'default gptel-directives
                               "You are a helpful assistant.")))

(defgptelfn gptel-diff ()
  "Generate a git commit message.

This function uses `magit-diff-staged` to obtain the current staged diffs
in a temporary buffer.  It then constructs a request string from these diffs
and sends it to the `gptel-request` function, which interacts with the GPT
model to generate a commit message.  Upon receiving the response, it places
the generated message in a special buffer named *gptel-diff* and copies it
to the kill ring.  If a buffer named 'COMMIT_EDITMSG' is also present, it
will switch to that buffer and notify the user that the commit message is
in the kill ring."
  :command nil
  :prompt
  (let ((diff-buffer
           (with-temp-buffer
             (magit-diff-staged)
             (buffer-name))))
      (with-current-buffer diff-buffer
        (buffer-substring-no-properties (point-min) (point-max))))
  :body
  (if (not *gptel-response*)
         (message
          "gptel-diff failed with message: %s"
          (plist-get *gptel-request-info* :status))
       (kill-new *gptel-response*)
       (with-current-buffer (get-buffer-create "*gptel-diff*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert *gptel-response*))
         (special-mode)
         (display-buffer (current-buffer)))
       (when (get-buffer "COMMIT_EDITMSG")
         (message "commit message in kill ring")
         (pop-to-buffer "COMMIT_EDITMSG")))
  :extra-args
  (list
   :system
   (alist-get
    'commiter
    gptel-directives
    "Write a git commit message for this diff. Include ONLY the message.
Be terse. Provide messages whose lines are at most 80 characters")))

(defgptelfn gptel-pull-request ()
  "Generate a GitHub pull request description by diffing with origin/master.

Upon receiving the response from gptel, it places the generated message
in a special buffer named *gptel-pull-request* and copies it to the
clipboard and kill ring."
  :command nil
  :prompt
  (let ((diff-buffer
         (with-temp-buffer
           (magit-diff-range "origin/master")
           (buffer-string))))
    (concat
     "Generate a pull request description summarizing the changes:\n\n"
     diff-buffer))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-pull-request
               (plist-get *gptel-request-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-pull-request*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (clipboard+kill-ring-save (point-min) (point-max))
      (message "pull request body in kill ring")))
  :extra-args
  (list
   :system
   (alist-get
    'PullRequest
    gptel-directives
    "Summarize the changes for a GitHub pull request description.")))

(defvar gptel-document-symbol-at-point--history nil)

(defgptelfn gptel-document-symbol-at-point (sym)
  "Generate documentation for the symbol at point.

This function prompts the user to input a symbol, suggests the symbol at point by default, and generates documentation for that symbol. The generated documentation is then displayed in a special mode buffer.

Usage:
  1. Place the cursor on or near the symbol you want to document.
  2. Run the command `gptel-document-symbol-at-point`.
  3. Confirm or modify the prompted symbol name.
  4. The generated documentation will be displayed in a special mode buffer.

Arguments:
  SYM: The symbol for which documentation is to be generated.

Prompts:
  - The function reads a symbol from the user, suggesting the symbol at point by default.

Output:
  - Displays the generated documentation in a buffer named `*gptel-document-symbol-at-point*`.

The function leverages ChatGPT to generate high-quality documentation.

Example:
```
  (defun example-fun (arg)
     \"An example function.\"
     ;; Place the cursor here and run `M-x gptel-document-symbol-at-point`
     ;; Confirm or modify the prompted symbol name (e.g., `example-fun`)
     ;; The generated documentation will appear in the buffer `*gptel-document-symbol-at-point*`
     (message \"Example argument: %s\" arg))

History:
  The function maintains a history of user inputs for the symbol prompt.
"
  :command
  (list
   (read-string "Documentation for: "
                (symbol-name (symbol-at-point))
                gptel-document-symbol-at-point--history))
  :prompt
  (progn
    (format
     "Add documentation for %s defined below:\n%s"
     sym
     (buffer-string)))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-document-symbol-at-point
               (plist-get *gptel-response-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-document-symbol-at-point*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (display-buffer (current-buffer))))
  :extra-args
  (list
   :system
   (alist-get 'Document gptel-directives
              "Prefer making a docstring")))

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

(provide 'init)
;;; init.el ends here
