;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!

 :v "s" #'evil-surround-region
 :o "S" #'evil-surround-edit
 :n  "]e"    #'move-text-line-down
 :n  "[e"    #'move-text-line-up

 ;; searching
 "C-s" 'swiper
 "C-c s" 'counsel-rg

 (:when (featurep! :editor multiple-cursors)
  :nv "C-n" #'evil-mc-make-and-goto-next-match
  :nv "C-p" #'evil-mc-make-and-goto-prev-match
  )

 (:leader

  ;; workspace / window management
  :n "o" 'spacemacs/workspaces-transient-state/body
  :n "0" 'treemacs-select-window
  :n "1" 'winum-select-window-1
  :n "2" 'winum-select-window-2
  :n "3" 'winum-select-window-3
  :n "4" 'winum-select-window-4
  :n "5" 'winum-select-window-5
  :n "6" 'winum-select-window-6
  :n "7" 'winum-select-window-7
  :n "8" 'winum-select-window-8
  :n "9" 'winum-select-window-9
  :n "'" 'spacemacs/shell-pop-multi-vterm
  :n "es" 'flycheck-list-errors
  :n "gs" 'magit-status

  :n "Ts" 'load-theme

  ;; jumps
  :n "ji" 'imenu
  :n "d" 'evil-goto-definition
  :n "fb" 'bookmark-jump
  :n "bb" 'switch-to-buffer
  )

 (:after multi-vterm
  (:map multi-vterm-mode-map
   :leader
   :n "mc" 'multi-vterm
   :n "mp" 'multi-vterm-prev
   :n "mn" 'multi-vterm-next
   )
  )

 ;; (:when (:featurep! :lang python)
 ;;  (:map python-mod-map
 ;;   :localleader
 ;;   :n "'"  #'+python/open-ipython-repl
 ;;   :n "sb" #'python-shell-send-buffer
 ;;   :n "sr" #'python-shell-send-region
 ;;   :n "db" #'python-toggle-breakpoint
 ;;   :n "ri" #'py-isort-buffer
 ;;   :n "ru" #'python-autoflake)
 ;;  )

 )

(after! evil (evil-escape-mode nil))

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
