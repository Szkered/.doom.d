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

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-cppcheck
                                '(warning . c/c++-googlelint))))

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'c++-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (c/c++-googlelint)))))))))
