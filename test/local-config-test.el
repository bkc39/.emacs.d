;;; local-config-test.el --- Tests for local-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -Q -L elisp -l test/local-config-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'local-config)

(defmacro local-config-test--with-file (contents &rest body)
  "Run BODY with `local-config-file' holding CONTENTS.
When CONTENTS is nil the file does not exist."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "local-config-test" t))
          (local-config-file (expand-file-name "local.eld" dir))
          (local-config--data 'unloaded)
          (warning-minimum-log-level :emergency)
          (warning-minimum-level :emergency))
     (unwind-protect
         (progn
           (when ,contents
             (with-temp-file local-config-file (insert ,contents)))
           ,@body)
       (delete-directory dir t))))

(ert-deftest local-config-missing-file-yields-defaults ()
  (local-config-test--with-file nil
    (should (null (local-config-get 'env)))
    (should (equal "python3"
                   (local-config-get '(python interpreter) "python3")))))

(ert-deftest local-config-nested-lookup ()
  (local-config-test--with-file
      "((python . ((interpreter . \"py\") (flag . nil)))
        (pytest . ((test-directories . (\"a/\" \"b/\")))))"
    (should (equal "py" (local-config-get '(python interpreter))))
    (should (equal '("a/" "b/")
                   (local-config-get '(pytest test-directories))))
    (should (equal 'fallback (local-config-get '(python flag) 'fallback)))
    (should (equal 'fallback (local-config-get '(python absent) 'fallback)))
    (should (equal 'fallback
                   (local-config-get '(python interpreter deeper) 'fallback)))
    (should (equal 'fallback (local-config-get 'coq 'fallback)))))

(ert-deftest local-config-malformed-file-is-ignored ()
  (dolist (contents '("((python . " "not-a-list" "(1 2 3)" "" "(()) (())"))
    (local-config-test--with-file contents
      (should (null (local-config-load)))
      (should (equal 'd (local-config-get '(python interpreter) 'd))))))

(ert-deftest local-config-file-is-data-not-code ()
  "Reading the file must not evaluate anything inside it."
  (local-config-test--with-file
      "((env . ((\"X\" . \"1\")))
        (python . ((interpreter . (shell-command-to-string \"echo pwned\")))))"
    (should (equal '(shell-command-to-string "echo pwned")
                   (local-config-get '(python interpreter))))))

(ert-deftest local-config-unknown-key-warns-but-loads ()
  (local-config-test--with-file "((surprise . 1) (coq . ((cpdt-src . \"/x\"))))"
    (let (warned)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (local-config-load))
      (should warned)
      (should (equal "/x" (local-config-get '(coq cpdt-src)))))))

(ert-deftest local-config-apply-env-respects-existing ()
  (local-config-test--with-file
      "((env . ((\"LOCAL_CONFIG_TEST_NEW\" . \"from-file\")
                (\"LOCAL_CONFIG_TEST_SET\" . \"from-file\")
                (bad . entry))))"
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "LOCAL_CONFIG_TEST_NEW" nil)
      (setenv "LOCAL_CONFIG_TEST_SET" "already")
      (local-config-apply-env)
      (should (equal "from-file" (getenv "LOCAL_CONFIG_TEST_NEW")))
      (should (equal "already" (getenv "LOCAL_CONFIG_TEST_SET"))))))

(ert-deftest local-config-apply-identity-sets-only-what-is-given ()
  (local-config-test--with-file "((identity . ((name . \"A Name\"))))"
    (let ((user-full-name "before")
          (user-mail-address "before@example.com"))
      (local-config-apply-identity)
      (should (equal "A Name" user-full-name))
      (should (equal "before@example.com" user-mail-address))))
  (local-config-test--with-file
      "((identity . ((name . 42) (email . \"me@example.com\"))))"
    (let ((user-full-name "before")
          (user-mail-address "before@example.com"))
      (local-config-apply-identity)
      (should (equal "before" user-full-name))
      (should (equal "me@example.com" user-mail-address)))))

(defun local-config-test--git (&rest args)
  "Run git with ARGS in `default-directory' and require success."
  (should (zerop (apply #'process-file "git" nil nil nil args))))

(ert-deftest local-config-check-staged-finds-denied-lines ()
  (skip-unless (executable-find "git"))
  (local-config-test--with-file
      "((deny-patterns . (\"secret-project\" \"host-[0-9]+\")))"
    (let* ((repo (make-temp-file "local-config-repo" t))
           (default-directory (file-name-as-directory repo))
           (noninteractive nil))
      (unwind-protect
          (progn
            (local-config-test--git "init" "-q")
            (with-temp-file (expand-file-name "clean.el" repo)
              (insert "(message \"nothing to see\")\n"))
            (local-config-test--git "add" "clean.el")
            (should (= 0 (local-config-check-staged)))
            (with-temp-file (expand-file-name "leak.el" repo)
              (insert "(setq root \"Secret-Project\")\n(setq h \"host-42\")\n"))
            (local-config-test--git "add" "leak.el")
            (should (= 2 (local-config-check-staged))))
        (delete-directory repo t)))))

(ert-deftest local-config-check-staged-without-patterns ()
  (local-config-test--with-file nil
    (let ((noninteractive nil))
      (should (= 0 (local-config-check-staged))))))

(provide 'local-config-test)

;;; local-config-test.el ends here
