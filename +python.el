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
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (eq major-mode 'python-mode) (+format/buffer))))))


(set-formatter! 'yapf  "yapf " :modes '(python-mode))

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

;; (after! lsp-python-ms
;;   (set-lsp-priority! 'mspyls 1))

;; extra checkers after lsp
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint python-mypy python-flake8))))))
              (setq flycheck-pylintrc "~/.config/pylintrc"))))

;; (executable-find "pyright")

(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

;; extra KDB / auto activate conda env
(add-hook! python-mode
 ;;(conda-env-activate "py38")
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


;; remote lsp
;; (require 'lsp-mode)


;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
;;                   :major-modes '(python-mode)
;;                  :remote? t
;;                  :server-id 'pyright-remote))


;; (push "/mnt/home/shizk/.linuxbrew/bin/pyright" tramp-remote-path)

;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-tramp-connection "langserver.index.js")
;;   :major-modes '(python-mode)
;;   :multi-root lsp-pyright-multi-root
;;   :remote? t
;;   :server-id 'pyright-remote
;;   :multi-root lsp-pyright-multi-root
;;   :priority 3
;;   :initialized-fn (lambda (workspace)
;;                     (with-lsp-workspace workspace
;;                       ;; we send empty settings initially, LSP server will ask for the
;;                       ;; configuration of each workspace folder later separately
;;                       (lsp--set-configuration
;;                        (make-hash-table :test 'equal))))
;;   :download-server-fn (lambda (_client callback error-callback _update?)
;;                         (lsp-package-ensure 'pyright callback error-callback))
;;   :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;                                  ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;                                  ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))

;; (setq enable-remote-dir-locals t)

(defun my-pdf-view-double-scroll-up-or-next-page (&optional arg)
  "Scroll page up ARG lines if possible, else go to the next page.

When `pdf-view-continuous' is non-nil, scrolling upward at the
bottom edge of the page moves to the next page.  Otherwise, go to
next page only on typing SPC (ARG is nil)."
  (interactive "P")
  (if (or pdf-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
            (cur-page (pdf-view-current-page)))
        (when (or (= (window-vscroll) (image-scroll-up arg))
                  ;; Workaround rounding/off-by-one issues.
                  (memq pdf-view-display-size
                        '(fit-height fit-page)))
          (pdf-view-next-page 2)
          (when (/= cur-page (pdf-view-current-page))
            (image-bob)
            (image-bol 1))
          (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-up arg)))

(defun my-pdf-view-double-scroll-horizontal-view ()
  (interactive)
  (my-pdf-view-double-scroll-up-or-next-page)
  (other-window 1)
  (my-pdf-view-double-scroll-up-or-next-page)
  (other-window 1))
