;;; ~/.doom.d/+cpp.el -*- lexical-binding: t; -*-


;; (after! lsp-clients (set-lsp-priority! 'clangd 1))

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

(add-hook! c++-mode-hook (lsp))

(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (add-hook 'before-save-hook
                                '+format/buffer))))
