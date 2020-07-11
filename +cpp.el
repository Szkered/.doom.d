;;; ~/.doom.d/+cpp.el -*- lexical-binding: t; -*-


(after! lsp-clients
  (set-lsp-priority! 'clangd 1)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
  )


(add-hook! c++-mode-hook (lsp))
