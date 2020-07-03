;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zekun Shi"
      user-mail-address "zekun@neuri.ai")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; (setq doom-font "Fira Mono for Powerline")

(setq doom-font (font-spec :family "Iosevka Term SS04" :size 20 :weight 'medium))

;; (setq doom-variable-pitch-font (font-spec :family "Overpass" :size 20))
;; (setq doom-variable-pitch-font (font-spec :family "Fira Sans Book" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Iosevka SS04" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-solarized-light)
(setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-laserwave)
;; (setq doom-theme 'zaiste)

;; some themes need to swap bg
(after! solaire-mode (solaire-mode-swap-bg))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; LSP
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(after! lsp-clients
  (set-lsp-priority! 'clangd 1)
  (flycheck-disable-checker 'c/c++-clang)
  (flycheck-disable-checker 'c/c++-gcc)
  )


;; to make kdb closer to spacemace
(load! "~/.doom.d/modules/spacemacs/+spacemacs")

;; python stuff

;; autoflake
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

(add-to-list 'auto-mode-alist '("\\.gin\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)[Dd]ockerfile" . dockerfile-mode))


(after! treemacs
  (setq treemacs-sorting 'mod-time-desc)
  )

(map!
 :v "s" #'evil-surround-region
 :o "S" #'evil-surround-edit
 :n  "]e"    #'move-text-line-down
 :n  "[e"    #'move-text-line-up

 (:when (featurep! :editor multiple-cursors)
  :nv "C-n" #'evil-mc-make-and-goto-next-match
  :nv "C-p" #'evil-mc-make-and-goto-prev-match
  )
 )

;; search
(define-key global-map (kbd "C-s") 'swiper)
(define-key global-map (kbd "C-c s") 'counsel-rg)

(spacemacs/set-leader-keys "Ts" 'load-theme)

(spacemacs/set-leader-keys "'" 'spacemacs/shell-pop-multi-vterm)

(spacemacs/set-leader-keys "o" 'spacemacs/workspaces-transient-state/body)

(spacemacs/set-leader-keys "gs" 'magit-status)

(spacemacs/set-leader-keys "0" 'treemacs-select-window)

(spacemacs/set-leader-keys "1" 'winum-select-window-1)
(spacemacs/set-leader-keys "2" 'winum-select-window-2)
(spacemacs/set-leader-keys "3" 'winum-select-window-3)
(spacemacs/set-leader-keys "4" 'winum-select-window-4)
(spacemacs/set-leader-keys "5" 'winum-select-window-5)
(spacemacs/set-leader-keys "8" 'winum-select-window-8)
(spacemacs/set-leader-keys "9" 'winum-select-window-9)

(spacemacs/set-leader-keys "d" 'evil-goto-definition)

(spacemacs/set-leader-keys "es" 'flycheck-list-errors)

(spacemacs/set-leader-keys "ji" 'imenu)

(spacemacs/set-leader-keys "fb" 'bookmark-jump)

(custom-set-faces '(vterm-color-black ((t (:background "#839496"))))) ;; make auto-complete visable
(use-package! multi-vterm
  :config
  (spacemacs/set-leader-keys "mc" 'multi-vterm)
  (spacemacs/set-leader-keys "mp" 'multi-vterm-prev)
  (spacemacs/set-leader-keys "mn" 'multi-vterm-next)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("25f1b2ace87d23d803b42267fafdc38b31472e444c2aaa9069aa2c06be8955b2" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" default))
 '(package-selected-packages '(csv-mode csv vterm shell-pop projectile)))
