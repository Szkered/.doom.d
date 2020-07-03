;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-directory "~/Dropbox/org/")

;; looks
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("⁖"))
(setq org-ellipsis " ... ")

;; deps
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

;; kbd
(add-hook! org-mode
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "I" 'org-clock-in
    "O" 'org-clock-out
    "R" 'org-refile
    )
  )

(setq org-todo-keywords
      '((sequence "TODO(t)" "HANGER(h)" "RUNWAY(r)" "AIRBORNE(a)" "|" "DONE(d)" "FAIL(f)" "CANCELLED(c)")))

;; priorities
(setq org-default-priority ?C)
(setq org-lowest-priority ?D)
(setq org-highest-priority ?A)

;; agenda / clock
(defun filter-org-file (file)
  (equal (car (last (split-string file "\\."))) "org")
  )

(setq all-org-files
      (mapcar (lambda (x) (concat "~/Dropbox/org/" x))
              (seq-filter 'filter-org-file (directory-files "~/Dropbox/org/"))))

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
(setq org-agenda-custom-commands
      '(
        ("d" "Daily agenda and all TODOs"
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Doing it Now:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
                                                (org-agenda-skip-entry-if 'todo '("RUNWAY" "HANGER"))))
                 (org-agenda-overriding-header "Long Term:")))
          (tags "PRIORITY=\"D\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
                                                (org-agenda-skip-entry-if 'todo '("RUNWAY" "HANGER"))))
                 (org-agenda-overriding-header "Learning:")))
          (tags "TODO=\"AIRBORNE\""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                (air-org-skip-subtree-if-priority ?A)
                                                (air-org-skip-subtree-if-priority ?B)
                                                (air-org-skip-subtree-if-priority ?D)))
                 (org-agenda-overriding-header "AIRBORNE projects:")))
          (agenda "" ((org-agenda-ndays 1)))
          (tags "TODO=\"RUNWAY\""
                ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                (air-org-skip-subtree-if-priority ?A)))
                 (org-agenda-overriding-header "Projects on the RUNWAY:")))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))
                                                   (org-agenda-skip-entry-if 'todo '("AIRBORNE" "RUNWAY"))))
                    (org-agenda-overriding-header "Projects / Tasks in the HANGER:")))
          )
         ((org-agenda-compact-blocks nil)
          (org-agenda-archives-mode t)
          (org-agenda-start-on-weekday 1)
          ))
        ("w" "Weekly review"
         agenda ""
         ((org-agenda-span 'week)
          (org-agenda-start-on-weekday 1)
          (org-agenda-start-with-log-mode t)
          (org-agenda-archives-mode t)
          )
         )
        )
      )
