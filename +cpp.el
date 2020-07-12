;;; ~/.doom.d/+cpp.el -*- lexical-binding: t; -*-


(after! lsp-clients (set-lsp-priority! 'clangd 1))

(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

(add-hook! c++-mode-hook (lsp))
