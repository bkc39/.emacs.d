;; local.example.eld -- template for the untracked per-machine file.
;;
;; Copy this to `local.eld' next to init.el and edit it.  `local.eld' is
;; listed in .gitignore and must never be committed.  The file is one
;; s-expression.  It is read as data and is never evaluated.  Every key
;; is optional, and a missing key falls back to a neutral default.

(;; Environment variables to set at startup unless already set.
 (env . (("SOME_PROJECT_ROOT" . "/path/to/project")))

 (python
  . (;; Interpreter and arguments for `run-python'.
     (interpreter      . "python3")
     (interpreter-args . "-i")
     ;; Formatter wrapper.  When absent, blacken runs with line length 80.
     (blacken-executable . "/path/to/black-wrapper.sh")
     ;; When set, format on save only in files whose file-local variable
     ;; `owner' equals this string.  When absent, always format on save.
     (format-on-save-owner . "someone")
     ;; `python-file-to-import-path' strips this root and adds the prefix.
     ;; `import-root-env' names an environment variable to consult first.
     (import-root-env . "SOME_PROJECT_ROOT")
     (import-root     . "/path/to/project")
     (import-prefix   . "package.")))

 ;; `run-project-pytest'.  `root-env', when set, names an environment
 ;; variable that is set to `root' for the pytest process.
 (pytest
  . ((root             . "/path/to/project/")
     (root-env         . "SOME_PROJECT_ROOT")
     (test-directories . ("tests/unit/" "tests/integration/"))))

 ;; Source directory for the CPDT Coq library.  The environment variable
 ;; CPDT_SRC_DIR takes precedence.
 (coq . ((cpdt-src . "/path/to/cpdt/src")))

 ;; Emacs regexps.  hooks/pre-commit rejects a commit whose staged lines
 ;; match one of them.  Put employer names, hostnames and internal
 ;; project names here, so that the denylist itself is never published.
 (deny-patterns . ("some-internal-name" "some-hostname")))
