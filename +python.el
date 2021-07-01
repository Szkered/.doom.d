;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

(defcustom python-autoflake-path (executable-find "autoflake")
  "autoflake executable path."
  :group 'python
  :type 'string)


(defun python-autoflake ()
  "Automatically clean up python codes
$ autoflake --in-place --remove-unused-variables --remove-all-unused-imports --remove-duplicate-keys --expand-star-imports <filename>"
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command
     (format
      "%s --in-place --remove-unused-variables --remove-all-unused-imports --remove-duplicate-keys --expand-star-imports %s"
      python-autoflake-path
      (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(add-hook 'python-mode-hook
          (function (lambda ()
                      (add-hook 'before-save-hook
                                'py-yapf-buffer))))

(defun python-toggle-breakpoint ()
  "Add an ipdb break point, highlight it."
  (interactive)
  (let ((trace (cond (t "breakpoint()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        ;; (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

(defun spacemacs/python-start-or-switch-repl ()
  "Start and/or switch to the REPL."
  (interactive)
  (let ((shell-process
         (or (python-shell-get-process)
             ;; `run-python' has different return values and different
             ;; errors in different emacs versions. In 24.4, it throws an
             ;; error when the process didn't start, but in 25.1 it
             ;; doesn't throw an error, so we demote errors here and
             ;; check the process later
             (with-demoted-errors "Error: %S"
               ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
               ;; shell process
               (call-interactively #'run-python)
               (python-shell-get-process)))))
    (unless shell-process
      (error "Failed to start python shell properly"))
    (pop-to-buffer (process-buffer shell-process))
    (evil-insert-state)))


(defun spacemacs//python-setup-shell (&rest args)
  (if (spacemacs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "[\r\n|\n]$" "" (shell-command-to-string (format "\"%s\" --version" (string-trim (spacemacs/pyenv-executable-find "ipython"))))) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))

(defun spacemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil. This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command)))
              (pyenv-version-names (split-string (string-trim (shell-command-to-string "pyenv version-name")) ":"))
              (executable nil)
              (i 0))
          (if (not (string-match "not found" pyenv-string))
              (while (and (not executable)
                          (< i (length pyenv-version-names)))
                (if (string-match (elt pyenv-version-names i) (string-trim pyenv-string))
                    (setq executable (string-trim pyenv-string)))
                (if (string-match (elt pyenv-version-names i) "system")
                    (setq executable (string-trim (executable-find command))))
                (setq i (1+ i))))
          executable))
    (executable-find command)))

(spacemacs//python-setup-shell)

(set-popup-rule! "*Python*" :size 0.5 :side 'right :slot 1 :ttl nil :select nil :modeline nil :quit nil)

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

;; extra checkers after lsp
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint python-mypy python-flake8)))))))))


;; extra KDB / auto activate conda env
(add-hook! python-mode
  (conda-env-activate "py39")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "'" 'spacemacs/python-start-or-switch-repl
    "l" 'lsp-execute-code-action
    "sb" 'python-shell-send-buffer
    "sr" 'python-shell-send-region
    "db" 'python-toggle-breakpoint
    "ri" 'py-isort-buffer
    "ru" 'python-autoflake))


(setq ein:output-area-inlined-images t)
(setq ein:use-auto-complete t)
(setq ein:use-smartrep t)

