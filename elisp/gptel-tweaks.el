;;; gptel-tweaks.el --- Custom GPTel helpers -*- lexical-binding: t; -*-

(require 'config)
(require 'dash)
(require 'anaphora)
(require 'subr-x)

(indent-like-defun 'defgptelfn)

(defun load-openai-api-key ()
  "Read the OpenAI API key from the ~/.openai file into OPENAI_API_KEY."
  (let ((api-key-file (expand-file-name "~/.openai")))
    (if (file-readable-p api-key-file)
        (with-temp-buffer
          (insert-file-contents api-key-file)
          (let ((api-key (string-trim (buffer-string))))
            (setenv "OPENAI_API_KEY" api-key)
            (message "OpenAI API key loaded successfully.")))
      (message "Error: ~/.openai file not found or is not readable."))))

(defun get-api-key-from-env-or-file (env-var api-key-file)
  "Get the API key from ENV-VAR or from API-KEY-FILE."
  (let ((api-key (getenv env-var))
        (key-file (expand-file-name api-key-file)))
    (unless api-key
      (if (file-readable-p key-file)
          (with-temp-buffer
            (insert-file-contents key-file)
            (setq api-key (string-trim (buffer-string)))
            (setenv env-var api-key)
            (message "API key loaded successfully."))
        (message "Error: %s file not found or is not readable." key-file)))
    api-key))

(defun get-openai-api-key ()
  "Get the OpenAI API key, loading from ~/.openai if needed."
  (get-api-key-from-env-or-file "OPENAI_API_KEY" "~/.openai"))

(defun get-anthropic-api-key ()
  "Get the Anthropic API key, loading from ~/.anthropic if needed."
  (get-api-key-from-env-or-file "ANTHROPIC_API_KEY" "~/.anthropic"))

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
    (message "prompts loaded from %s" directory)
    result))

(defun gptel-request/require (&rest args)
  "Ensure gptel-request is bound, require 'gptel if not, then call it with ARGS."
  (unless (fboundp 'gptel-request)
    (require 'gptel))
  (apply #'gptel-request args))

(defvar *llm-prompts-dir* "~/.llm-prompts"
  "Directory containing the PROMPT.md files.")

(defun/who reload-gptel-directives ()
  "Reload `gptel-directives` from `*llm-prompts-dir*` if it exists."
  :command
  (if (file-exists-p *llm-prompts-dir*)
      (setq gptel-directives
            (read-prompt-md-files *llm-prompts-dir*))
    (message "%s: LLM prompts directory %s does not exist!"
             who
             *llm-prompts-dir*)))

(defun ensure-gptel-directives-loaded ()
  "Ensure that `gptel-directives` is defined."
  (unless (boundp 'gptel-directives)
    (reload-gptel-directives)))

(defun dynamic-prompt (make-prompt callback gptel-request-args)
  "Generate a dynamic prompt for ChatGPT queries."
  (let* ((prompt (funcall make-prompt))
         (request-args
          (cons prompt
                (append (funcall gptel-request-args)
                        (list :callback callback)))))
    (ensure-gptel-directives-loaded)
    (apply #'gptel-request/require request-args)))

(defmacro defgptelfn (name args &rest stx)
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
                    "Error - defgptelfn: missing required keyword argument :prompt"))))
             (,request-callback
              (lambda (*gptel-response* *gptel-response-info*)
                ,(aif (assoc :body stx-kwargs)
                     (cadr it)
                   (error
                    "Error - defgptelfn: missing required keyword argument :body"))))
             (,request-args-thunk
              (lambda ()
                ,@(aif (assoc :extra-args stx-kwargs)
                      it
                    (list nil)))))
         (dynamic-prompt
          ,make-prompt
          ,request-callback
          ,request-args-thunk)))))

(indent-like-defun 'defgptelfn)

(defun make-gptel-system-prompt-args (directive default-directive)
  "Generate system prompt arguments for GPTel requests."
  (ensure-gptel-directives-loaded)
  (list
   :system (alist-get directive gptel-directives default-directive)))

(defvar gptel-quick--history nil
  "History list for `gptel-quick' prompts.")

(defgptelfn gptel-quick (prompt)
  "Send PROMPT to ChatGPT and display the response in a special buffer."
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
  :extra-args
  (make-gptel-system-prompt-args 'default "You are a helpful assistant."))

(defun my/magit-diff-master ()
  (let ((diff-buffer
         (with-temp-buffer
           (magit-diff-range "origin/master")
           (buffer-string))))
    (concat
     "Generate a pull request description summarizing the changes:\n\n"
     diff-buffer)))

(defun my/magit-pull-master-body (gptel-response)
  (if (not gptel-response)
      (message "%s failed with message: %s"
               'gptel-pull-request
               (plist-get *gptel-request-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-pull-request*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert gptel-response))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (clipboard+kill-ring-save (point-min) (point-max))
      (message "pull request body in kill ring"))))

(defgptelfn gptel-diff ()
  "Generate a git commit message from staged changes."
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
  (make-gptel-system-prompt-args
   'commiter
   "Write a git commit message for this diff. Include ONLY the message.
Be terse. Provide messages whose lines are at most 80 characters"))

(defgptelfn gptel-pull-request ()
  "Generate a GitHub pull request description by diffing with origin/master."
  :command nil
  :prompt
  (my/magit-diff-master)
  :body
  (my/magit-pull-master-body *gptel-response*)
  :extra-args
  (make-gptel-system-prompt-args
   'pullrequest
   "Summarize the changes for a GitHub pull request description."))

(defgptelfn gptel-pinely-merge ()
  "Generate a GitHub pull request description from staged changes."
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
      (message "%s failed with message: %s"
               'gptel-pinely-merge
               (plist-get *gptel-request-info* :status))
    (with-current-buffer
        (get-buffer-create "*gptel-pinely-merge*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (pop-to-buffer (current-buffer))
      (clipboard+kill-ring-save (point-min) (point-max))
      (message "pinely merge body in kill ring")))
  :extra-args
  (make-gptel-system-prompt-args
   'pullrequest
   "Summarize the changes for a GitHub pull request description."))

(defvar gptel-document-symbol-at-point--history nil)

(defgptelfn gptel-document-symbol-at-point (sym)
  "Generate documentation for the symbol at point."
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
  (make-gptel-system-prompt-args 'documentation "Prefer making a docstring"))

(defvar gptel-tests-for-symbol-at-point nil
  "History for `gptel-tests-for-symbol-at-point`.")

(defgptelfn gptel-tests-for-symbol-at-point (sym)
  :command
  (list
   (read-string "Make tests for: "
                (symbol-name (symbol-at-point))
                gptel-document-symbol-at-point--history))
  :prompt
  (progn
    (format
     "Add test cases for %s defined below:\n%s"
     sym
     (buffer-string)))
  :body
  (if (not *gptel-response*)
      (message "%s failed with message: %s"
               'gptel-document-symbol-at-point
               (plist-get *gptel-response-info* :status))
    (with-current-buffer (get-buffer-create "*gptel-tests-for-symbol-at-point*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert *gptel-response*))
      (special-mode)
      (display-buffer (current-buffer))))
  :extra-args
  (make-gptel-system-prompt-args
   'testing
   "Generate test cases using the idiomatic language features and libraries
for the code provided"))

(provide 'gptel-tweaks)

;;; gptel-tweaks.el ends here
