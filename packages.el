;; -*- no-byte-compile: t; -*-
;;; $doomdir/packages.el

;; to install a package with doom you must declare them here and run 'doom sync'
;; on the command line, then restart emacs for the changes to take effect -- or
;; use 'm-x doom/reload'.


;; to install some-package from melpa, elpa or emacsmirror:
                                        ;(package! some-package)

;; to install a package directly from a remote git repo, you must specify a
;; `:recipe'. you'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; if the package you are trying to install does not contain a packagename.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; if you'd like to disable a package included with doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; you can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. these will inherit the rest of its recipe
;; from doom or melpa/elpa/emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; specify a `:branch' to install a package from a particular branch or tag.
;; this is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; doom's packages are pinned to a specific commit and updated from release to
;; release. the `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...or *all* packages (not recommended; will likely break things)
                                        ;(unpin! t)

(package! nov)                          ;; for epub layer
(package! auto-highlight-symbol)        ;; for spacemacs-navigation
(package! eyebrowse)                    ;; for spacemacs-layouts
(package! bm)                           ;; for bm layer
(package! evil-iedit-state)             ;; for spacemacs-navigation
(package! bind-map)
(package! symbol-overlay)
(package! move-text)                    ;; for spacemacs-editing
(package! string-inflection)            ;; for spacemacs-editing
(package! forge)                        ;; for github layer
(package! evil-terminal-cursor-changer) ;; for spacemacs evil
(package! eshell-prompt-extras)         ;; for shell layer
(package! multi-vterm)                  ;; for multiple vterms
(package! lsp-python-ms)
(package! shell-pop)
(package! dired-toggle-sudo)
(package! csv-mode)
;; (package! org-download)
(package! systemd)
(package! info-colors)
(package! evil-tex)
;; (package! evil-tex
;;   :hook (LaTeX-mode . evil-tex-mode))
(package! powerthesaurus)
(package! smartrep)
(package! ob-browser)
(package! tldr)
(package! eglot-jl)
(package! auto-yasnippet)
(package! org-pomodoro)
(package! aas)
(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
;; (package! auto-latex-snippeto
;;   :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
;; (package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
;;   :pin "cc02f25337...")
(package! vlf)
(package! org-superstar)
(unpin! doom-themes)                    ;; to get latest themes
;; (package! org-roam :recipe (:host github :repo "org-roam/org-roam"))
;; (package! org-roam-server)
(package! org-timeline)
(package! org-gcal)
(package! protobuf-mode)
(package! bazel)
(package! py-yapf)
(package! flycheck-google-cpplint)
(package! gitconfig-mode
  :recipe (:host github :repo "magit/git-modes" :files ("gitconfig-mode.el")))
(package! dired-narrow)
(package! docker-tramp)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
