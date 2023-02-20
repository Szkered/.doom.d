;;; ~~/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-directory "~/Dropbox/notes")
;; (setq org-agenda-files (directory-files-recursively "~/Dropbox/notes/" "\\.org$"))

;; appearances
(setq org-hide-emphasis-markers nil)
(setq org-superstar-headline-bullets-list '("⁖"))
(setq org-ellipsis " ... ")

;; deps
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

;; kbd
(add-hook! org-mode
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "I" 'org-clock-in
    "O" 'org-clock-out
    "R" 'org-refile)

  (org-superstar-mode)
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/notes/" "\\.org$")))


(setq org-todo-keywords
      '((sequence "TODO(t)" "HANGER(h)" "RUNWAY(r)" "AIRBORNE(a)" "|" "DONE(d)" "FAIL(f)" "CANCELLED(c)" "DELEGATED(g)")))

;; priorities
(setq org-default-priority ?C)
(setq org-lowest-priority ?D)
(setq org-highest-priority ?A)

;; org file location
(defun filter-org-file (file)
  (equal (car (last (split-string file "\\."))) "org"))


(setq all-org-files
      (seq-filter 'filter-org-file (directory-files-recursively "~/Dropbox/notes/" ".*")))

;; refile
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)
        (all-org-files :maxlevel . 3)))
;; (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;; (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; agenda / clock
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
     PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun chanining/archive-when-done ()
  "Archive current entry if it is marked as DONE"
  (when (org-entry-is-done-p)
    (org-toggle-archive-tag)))

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 3 :fileskip0 t :narrow 80 :formula %)))
(setq org-agenda-log-mode-items '(closed state clock))
(setq org-clock-idle-time 5)
(setq org-agenda-block-separator (string-to-char " "))
;; (setq my-org-agenda-prefix-format " %-2i %(concat \"[\" (car (last(split-string (or (org-agenda-get-category) \"\") \"-\"))) \"]\") %b %t%s")
(setq my-org-agenda-prefix-format " %-2i %b %t%s")
(setq org-agenda-custom-commands
      '(
        ("d" "Daily agenda and all TODOs"
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-prefix-format my-org-agenda-prefix-format)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-remove-tags t)
                 (org-agenda-overriding-header "\n\n⚡ Imminent:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))

          (tags "TODO=\"AIRBORNE\""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                (air-org-skip-subtree-if-priority ?A)
                                                (air-org-skip-subtree-if-priority ?B)
                                                (air-org-skip-subtree-if-priority ?D)))
                 ;; (org-agenda-prefix-format " %-2:i")
                 (org-agenda-prefix-format my-org-agenda-prefix-format)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-remove-tags t)
                 (org-agenda-overriding-header "⚡ Airborne:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))

          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
                                                (org-agenda-skip-entry-if 'todo '("RUNWAY" "HANGER"))))
                 ;; (org-agenda-prefix-format " %-2:i")
                 (org-agenda-prefix-format my-org-agenda-prefix-format)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-remove-tags t)
                 (org-agenda-overriding-header "⚡ Projects:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")))

          (tags "PRIORITY=\"D\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
                                                (org-agenda-skip-entry-if 'todo '("RUNWAY" "HANGER"))))
                 ;; (org-agenda-prefix-format " %-2:i")
                 (org-agenda-prefix-format my-org-agenda-prefix-format)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-remove-tags t)
                 (org-agenda-overriding-header "⚡ Deferrible:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))

          (agenda "" (
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 2)
                      (org-agenda-overriding-header "⚡ Schedule:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      ;; (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format   "  %-2i %t %s  ")
                      ;; (org-agenda-prefix-format " %-2i %b %t%s")
                      ;; (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-time-grid (quote ((daily today remove-match)
                                                    (0900 1200 1500 1800 2100)
                                                    "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))

          (tags "TODO=\"RUNWAY\""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                (air-org-skip-subtree-if-priority ?A)))
                 ;; (org-agenda-prefix-format " %-2:i")
                 (org-agenda-prefix-format my-org-agenda-prefix-format)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-remove-tags t)
                 (org-agenda-overriding-header "⚡ Runway:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))

          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))
                                                   (org-agenda-skip-entry-if 'todo '("AIRBORNE" "RUNWAY"))))
                    ;; (org-agenda-prefix-format " %-2:i")
                    (org-agenda-prefix-format my-org-agenda-prefix-format)
                    (org-agenda-todo-keyword-format "")
                    (org-agenda-remove-tags t)
                    (org-agenda-overriding-header "⚡ Hanger:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"))))


         ((org-agenda-compact-blocks nil)
          (org-agenda-archives-mode t)
          ;; (org-agenda-start-with-log-mode '(closed))
          (org-agenda-start-with-log-mode t)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-start-on-weekday 1)))

        ("w" "Weekly review"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-log-mode t)
          (org-agenda-start-with-clockreport-mode t)
          (org-agenda-archives-mode t)
          (org-agenda-prefix-format " %-2i %t %s %b ")
          (org-agenda-remove-tags t)))))

(setq org-agenda-breadcrumbs-separator " 🡒 ")

(setq org-agenda-category-icon-alist
      `((".*org.*" ,(list (all-the-icons-faicon "pencil")) nil nil :ascent center)
        (".*gtd.*" ,(list (all-the-icons-faicon "pencil")) nil nil :ascent center)
        (".*research.*" ,(list (all-the-icons-faicon "pencil")) nil nil :ascent center)
        ;; ("neuri" ,(list (all-the-icons-faicon "black-tie" :height 0.9)) nil nil :ascent center)
        (".*sail.*" ,(list (all-the-icons-octicon "briefcase")) nil nil :ascent center)
        (".*game_ai.*" ,(list (all-the-icons-octicon "briefcase")) nil nil :ascent center)
        (".*neuri.*" ,(list (all-the-icons-octicon "briefcase")) nil nil :ascent center)
        (".*career.*" ,(list (all-the-icons-octicon "briefcase")) nil nil :ascent center)
        ;; ("math" ,(list (all-the-icons-faicon "graduation-cap" :height 0.65)) nil nil :ascent center)
        (".*math.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*ai4sci.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*phd_application.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*reinforcement_learning.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*cvx.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*geometry.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*quantum.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*nus.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*MA.*" ,(list (all-the-icons-octicon "mortar-board")) nil nil :ascent center)
        (".*music.*" ,(list (all-the-icons-faicon "music")) nil nil :ascent center)
        (".*health.*" ,(list (all-the-icons-octicon "pulse")) nil nil :ascent center)
        (".*habit.*" ,(list (all-the-icons-octicon "pulse")) nil nil :ascent center)
        (".*workouts.*" ,(list (all-the-icons-octicon "pulse")) nil nil :ascent center)
        (".*nutrition.*" ,(list (all-the-icons-octicon "pulse")) nil nil :ascent center)
        (".*mindfulness.*" ,(list (all-the-icons-octicon "pulse")) nil nil :ascent center)
        ;; ("my_fin" ,(list (all-the-icons-faicon "usd")) nil nil :ascent center)
        (".*fin.*" ,(list (all-the-icons-faicon "line-chart" :height 0.68)) nil nil :ascent center)
        (".*ml.*" ,(list (all-the-icons-octicon "hubot")) nil nil :ascent center)
        (".*cs.*" ,(list (all-the-icons-faicon "terminal")) nil nil :ascent center)
        (".*cpp.*" ,(list (all-the-icons-faicon "terminal")) nil nil :ascent center)
        ;; (".*schedule.*" ,(list (all-the-icons-faicon "commenting")) nil nil :ascent center)
        (".*schedule.*" ,(list (all-the-icons-octicon "calendar")) nil nil :ascent center)
        (".*blockchain.*" ,(list (all-the-icons-faicon "lock")) nil nil :ascent center)
        ("vocab" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        (".*read.*" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
        (".*cooking.*" ,(list (all-the-icons-faicon "fire")) nil nil :ascent center)))


;; TO DISPLAY ALL AVAILABLE ICONS
;; (all-the-icons-insert-icons-for 'octicon 10)
;; (all-the-icons-insert-icons-for 'alltheicon)
;; (all-the-icons-insert-icons-for 'faicon 1 0.5)

(defun my-org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let ((colors (list "IndianRed" "SeaGreen4" "sienna3" "DarkSlateGray4"))
          pos
          duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "#FFFFFF"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

;; (let ((colors (list "IndianRed" "SeaGreen4" "sienna3" "DarkSlateGray4"))) (nth 2 colors))

(add-hook 'org-agenda-finalize-hook #'set-window-clean)

;; use percentage to calculate left/right margin instead of the default 80 char line width
(setq writeroom-width 0.8)

(defun set-window-clean ()
  "clean buffer for org agenda"
  (interactive)
  (setq mode-line-format nil)
  (writeroom-mode)
  (text-scale-decrease 2)
  (my-org-agenda-time-grid-spacing))


;; persistent org agenda buffer
(setq org-agenda-sticky t)


;; org journal
(setq org-journal-file-type 'monthly)
(setq org-journal-enable-agenda-integration t)
(setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

;; deft
;; (use-package deft
;;   :config
;;   (setq deft-extensions '("txt" "tex" "org"))
;;   (setq deft-directory "~/Dropbox/notes")
;;   (setq deft-recursive t))

;; Automatically add an appointment when clocking in a task
(defvar bzg-org-clock-in-appt-delay 100
  "Number of minutes for setting an appointment by clocking-in")

(defun bzg-org-clock-in-add-appt (&optional n)
  "Add an appointment for the Org entry at point in N minutes."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (looking-at org-complex-heading-regexp)
    (let* ((msg (match-string-no-properties 4))
           (ct-time (decode-time))
           (appt-min (+ (cadr ct-time)
                        (or n bzg-org-clock-in-appt-delay)))
           (appt-time ; define the time for the appointment
            (progn (setf (cadr ct-time) appt-min) ct-time)))
      (appt-add (format-time-string
                 "%H:%M" (apply 'encode-time appt-time)) msg)
      (if (interactive-p) (message "New appointment for %s" msg)))))

;; (defadvice org-clock-in (after org-clock-in-add-appt activate)
;;   "Add an appointment when clocking a task in."
;;   (bzg-org-clock-in-add-appt))

;; babel
(defun my-org-python ()
  (if (eq major-mode 'python-mode)
      (progn (anaconda-mode t)
             (company-mode t))))

(add-hook 'org-src-mode-hook 'my-org-python)

;; pomodoro
(setq org-pomodoro-length 50)
(setq org-pomodoro-short-break-length 10)
(setq org-pomodoro-long-break-length 30)

;; roam
(setq org-roam-directory "~/Dropbox/notes/roam")

;; (use-package org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8123
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

;; (defun org-roam-server-open ()
;;   "Ensure the server is active, then open the roam graph."
;;   (interactive)
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode 1)
;;   (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
;;   (smartparens-global-mode 1))

;; ;; automatically enable server-mode
;; (after! org-roam
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode)
;;   (smartparens-global-mode 1))

;; google calendar integration
(require 'org-gcal)
(setq org-gcal-client-id my-gcal-client-id
      org-gcal-client-secret my-gcal-client-secret
      org-gcal-fetch-file-alist '(("derek.zekun.shi%40gmail.com" .  "~/Dropbox/notes/roam/schedule.org")))

;; auto export org beamer file
(defun my-org-beamer-sync-hook ()
  (when (eq major-mode 'org-mode)
    (if (and (boundp 'org-latex-classes)
             (equal (car org-file-tags) #("presentation" 0 12 (inherited t))))
        (org-beamer-export-to-latex))))

(add-hook 'before-save-hook #'my-org-beamer-sync-hook)
