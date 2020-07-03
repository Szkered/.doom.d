;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-


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
