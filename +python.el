;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

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
