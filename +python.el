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


(defun python-toggle-breakpoint ()
  "Add an ipdb break point, highlight it."
  (interactive)
  (let ((trace (cond (t "import ipdb; ipdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

;; (defun spacemacs/python-start-or-switch-repl ()
;;   "Start and/or switch to the REPL."
;;   (interactive)
;;   (let ((shell-process
;;          (or (python-shell-get-process)
;;              ;; `run-python' has different return values and different
;;              ;; errors in different emacs versions. In 24.4, it throws an
;;              ;; error when the process didn't start, but in 25.1 it
;;              ;; doesn't throw an error, so we demote errors here and
;;              ;; check the process later
;;              (with-demoted-errors "Error: %S"
;;                ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
;;                ;; shell process
;;                (call-interactively #'run-python)
;;                (python-shell-get-process)))))
;;     (unless shell-process
;;       (error "Failed to start python shell properly"))
;;     (pop-to-buffer (process-buffer shell-process) )
;;     (evil-insert-state)))


(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(add-hook! python-mode
  (conda-env-activate "tf2")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "'" '+python/open-ipython-repl
    "sb" 'python-shell-send-buffer
    "sr" 'python-shell-send-region
    "db" 'python-toggle-breakpoint
    "ri" 'py-isort-buffer
    "ru" 'python-autoflake
    )
  )

(setq ein:output-area-inlined-images t)
