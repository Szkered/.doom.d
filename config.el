;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Zekun Shi"
      user-mail-address "zekun@neuri.ai")

;; themes
(setq doom-font (font-spec :family "Iosevka Term SS04" :size 20 :weight 'medium))
(setq doom-variable-pitch-font (font-spec :family "Iosevka SS04" :size 16))
(setq doom-theme 'doom-horizon)
(after! solaire-mode (solaire-mode-swap-bg)) ;; some themes need to swap bg
(custom-set-faces! '(vterm-color-black :background "#839496")) ;; make auto-complete visable

(setq display-line-numbers-type nil)

;; to make kdb closer to spacemace
(load! "~/.doom.d/modules/spacemacs/+spacemacs")

;; modes
(add-to-list 'auto-mode-alist '("\\.gin\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)[Dd]ockerfile" . dockerfile-mode))

;; treemacs
(after! treemacs (setq treemacs-sorting 'mod-time-desc))

;; make flyspell faster
(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1))

;; info mode colors
(use-package! info-colors
  :commands (info-colors-fontify-node))
(add-hook 'Info-selection-hook 'info-colors-fontify-node)
(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(setq TeX-engine 'xetex)
(when EMACS28+
  (add-hook 'latex-mode-hook #'TeX-latex-mode))


(load! "+org")
(load! "+python")
(load! "+cpp")
(load! "+bindings")
