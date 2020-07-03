;;; ~/.doom.d/+cpp.el -*- lexical-binding: t; -*-


(after! lsp-clients
  (set-lsp-priority! 'clangd 1)
  )

(add-hook! c++-mode
  (flycheck-disable-checker 'c/c++-clang)
  (flycheck-disable-checker 'c/c++-gcc)
  )
