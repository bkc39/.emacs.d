;;; local-config.el --- Untracked per-machine settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Values that belong to one machine, one employer or one person do not
;; belong in this public repository.  They live in an untracked
;; s-expression file, `local.eld', next to init.el.
;;
;; The file holds a single nested alist.  It is parsed with `read' and is
;; never passed to `load' or `eval', so it is data and cannot run code.
;; A missing file, a malformed file or a missing key yields the caller's
;; default, so startup never fails because of it.
;;
;; local.example.eld documents every supported key.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar local-config-file
  (or (getenv "EMACS_LOCAL_CONFIG")
      (expand-file-name "local.eld" user-emacs-directory))
  "Path of the untracked per-machine configuration file.
The environment variable EMACS_LOCAL_CONFIG overrides the default.")

(defconst local-config-known-keys
  '(env python pytest coq deny-patterns)
  "Top-level keys that `local-config-file' may contain.")

(defvar local-config--data 'unloaded
  "Parsed contents of `local-config-file', or the symbol `unloaded'.")

(defun local-config--warn (format-string &rest args)
  "Report a problem with the local configuration without failing.
FORMAT-STRING and ARGS are passed to `format-message'."
  (display-warning 'local-config
                   (apply #'format-message format-string args)
                   :warning))

(defun local-config--read-file (file)
  "Return the single s-expression stored in FILE.
Signal an error when FILE holds no form or more than one."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((form (read (current-buffer))))
      (when (condition-case nil
                (progn (read (current-buffer)) t)
              (end-of-file nil))
        (error "Expected exactly one form"))
      form)))

(defun local-config--alist-p (object)
  "Return non-nil when OBJECT is a list of conses with symbol keys."
  (and (proper-list-p object)
       (cl-every (lambda (cell)
                   (and (consp cell) (symbolp (car cell))))
                 object)))

(defun local-config-load ()
  "Read `local-config-file' and return its contents as an alist.
A missing or malformed file yields nil after a warning."
  (setq local-config--data
        (if (not (file-readable-p local-config-file))
            nil
          (condition-case err
              (let ((form (local-config--read-file local-config-file)))
                (unless (local-config--alist-p form)
                  (error "Top level must be an alist with symbol keys"))
                (dolist (cell form)
                  (unless (memq (car cell) local-config-known-keys)
                    (local-config--warn "%s: unknown key `%s'"
                                        local-config-file (car cell))))
                form)
            (error
             (local-config--warn "%s ignored: %s"
                                 local-config-file
                                 (error-message-string err))
             nil)))))

(defun local-config-get (path &optional default)
  "Return the value at PATH in the local configuration, or DEFAULT.
PATH is a key symbol or a list of key symbols naming nested alists,
for example \\='(python interpreter).  A key that is absent, or whose
value is nil, yields DEFAULT."
  (when (eq local-config--data 'unloaded)
    (local-config-load))
  (let ((value local-config--data)
        (keys (if (listp path) path (list path))))
    (while (and keys value)
      (setq value (and (local-config--alist-p value)
                       (alist-get (car keys) value))
            keys (cdr keys)))
    (if (or keys (null value)) default value)))

(defun local-config-apply-env ()
  "Set each environment variable listed under `env' unless already set."
  (dolist (cell (local-config-get 'env))
    (when (and (consp cell) (stringp (car cell)) (stringp (cdr cell))
               (not (getenv (car cell))))
      (setenv (car cell) (cdr cell)))))

;;;; Commit guard

(defun local-config--staged-added-lines ()
  "Return the lines added by the staged diff as (FILE . LINE) pairs."
  (let (file result)
    (with-temp-buffer
      (unless (zerop (process-file "git" nil t nil
                                   "diff" "--cached" "--no-color" "-U0"))
        (error "Could not read the staged diff"))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "+++ " line)
            (setq file (string-remove-prefix "b/" (substring line 4))))
           ((and (string-prefix-p "+" line) file)
            (push (cons file (substring line 1)) result))))
        (forward-line 1)))
    (nreverse result)))

(defun local-config-check-staged ()
  "Exit non-zero when a staged line matches an entry of `deny-patterns'.
Meant for `emacs --batch' from hooks/pre-commit.  The patterns are
Emacs regexps kept in the untracked file, so the denylist itself is
never published."
  (let ((patterns (cl-remove-if-not #'stringp
                                    (local-config-get 'deny-patterns)))
        (case-fold-search t)
        (hits 0))
    (when patterns
      (dolist (entry (local-config--staged-added-lines))
        (dolist (pattern patterns)
          (when (string-match-p pattern (cdr entry))
            (setq hits (1+ hits))
            (message "%s: staged line matches a deny pattern: %s"
                     (car entry) (string-trim (cdr entry)))))))
    (when noninteractive
      (kill-emacs (if (zerop hits) 0 1)))
    hits))

(provide 'local-config)

;;; local-config.el ends here
