(setq dds-org-packages
      '(
        org
        org-bullets
        org-journal
        ))

(defun dds-org/post-init-org ()
  (use-package org
    :hook ((org-mode . turn-on-visual-line-mode)
           (org-mode . turn-on-flyspell))
    :bind (
           :map org-agenda-mode-map ("!" . dds-org/org-agenda-toggle-deadlines-and-habits)
           )
    :config
    (setq
     org-directory '("~/plan/")
     org-deadline-warning-days 20
     org-default-notes-file "~/plan/planner.org"
     org-tags-column -90
     org-enforce-todo-dependencies t             ;; block closing on children
     org-refile-targets '((nil :maxlevel . 4))   ;; refile in-file to 4 levels
     org-adapt-indentation nil                   ;; indent text to match heading
     org-columns-default-format "%50ITEM(Task) %17Effort(Estimated Effort){:} %10CLOCKSUM %16TIMESTAMP_IA"
     org-log-into-drawer t
     org-pretty-entities t
     org-pretty-entities-include-sub-superscripts nil
     org-use-sub-superscripts '{}
     org-export-with-sub-superscripts '{}
     )
    (setq org-todo-keywords
          '((sequence "TODO(t)"
                      "WAIT(w@/!)"
                      "DELE(d@/!)"
                      "|" "DONE(x!)" "FAIL(c@)")
            (type "MEET")))
    (let ((fh-odnf '(file+headline org-default-notes-file "Notes")))
      (setq org-capture-templates
            `(
              ("t" "Todo" entry (file org-default-notes-file)
               (file "~/plan/templates/todo.template.org"))
              ("n" "Note" entry ,fh-odnf
               (file "~/plan/templates/note.template.org"))
              ("m" "Meeting" entry ,fh-odnf
               (file "~/plan/templates/meeting.template.org")
               :clock-in t)
              ("D" "Daily Review" entry
               (file+datetree "~/plan/notes/reviews.org")
               (file "~/plan/templates/dailyreview.template.org")
               :tree-type week
               :jump-to-captured t)
              ("W" "Weekly Review" entry
               (file+datetree "~/plan/notes/reviews.org")
               (file "~/plan/templates/weeklyreview.template.org")
               :tree-type week
               :jump-to-captured t)
              ("M" "Monthly Review" entry
               (file+headline "~/plan/notes/reviews.org" "Monthly Reviews")
               (file "~/plan/templates/monthlyreview.template.org")
               :jump-to-captured t)
              ("A" "Annual Review" entry
               (file+headline "~/plan/notes/reviews.org" "Annual Reviews")
               (file "~/plan/templates/yearlyreview.template.org")
               :jump-to-captured t)
              ("S" "Protocol" entry ,fh-odnf
               (file "~/plan/templates/protocol.quote.template.org")
               :empty-lines 1)
              ("L" "Protocol Link" entry ,fh-odnf
               (file "~/plan/templates/protocol.link.template.org")
               :immediate-finish 1
               :empty-lines 1)
              )))
    (add-to-list 'org-refile-targets
                 '(("~/plan/notes/notes.org" "~/plan/notes/bib.org")
                   :maxlevel . 3))
    ;; Faces
    (dds-org/post-init-org/config-faces)
    ;; Archives
    (dds-org/post-init-org/config-archives)
    ;; Agenda configuration
    (require 'org-habit)
    (dds-org/post-init-org/config-agenda)
    ;; Seamless cryptography
    (require 'org-crypt)
    (dds-org/post-init-org/config-crypt)
    ;; Time tracking
    (dds-org/post-init-org/config-clock)
    ;; Exporting
    (dds-org/post-init-org/config-export)
    ;; Habit tracking
    (dds-org/post-init-org/config-habit)
    ;; Working with source-code
    (dds-org/post-init-org/config-src)
    (dds-org/post-init-org/config-ob)
    ;; Make Emacs handle org-protocol:// URLs, for browser integration
    (require 'org-protocol)
    ;; Exporting to ical for google calendar
    (dds-org/post-init-org/config-ical)
    ))

(defun dds-org/post-init-org/config-archives ()
  (setq
   org-archive-location "~/plan/archive/datetree.org::datetree/"
   org-archive-subtree-add-inherited-tags t
   org-datetree-add-timestamp 'inactive
   ))

(defun dds-org/post-init-org/config-agenda ()
  (setq
   org-agenda-files org-directory
   org-agenda-span 'day
   org-agenda-text-search-extra-files '("~/plan/archive/datetree.org" "~/plan/archive/archive.org")
   org-agenda-use-time-grid nil
   org-agenda-sticky nil
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-skip-scheduled-if-deadline-is-shown nil
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-scheduled-if-done t
   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :step day :scope agenda-with-archives)
   org-stuck-projects '("BLOCKED=\"t\"" nil nil "SCHEDULED:\\|DEADLINE:")
   )
  (setq org-agenda-custom-commands
   '(("r" . "Weekly Review")
     ("r1" "1. Get Clear: Collect loose materials, process Inbox"
      tags "REFILE|LEVEL=1+TODO={TODO}"
      ((org-agenda-overriding-header "1. Get Clear: Process Inbox")
       (org-agenda-prefix-format "")
       (org-agenda-skip-function 'my/org-skip-subtree-if-habit)))
     ("r2" "2. Get Current: Review the time clock. Find errors."
      agenda "" ((org-agenda-overriding-header "2. Get Current: Review the timelog")
                 (org-agenda-sorting-strategy '(time-up))
                 (org-agenda-archives-mode t)
                 (org-agenda-log-mode-items '(clocked closed state))
                 (org-agenda-skip-deadline-if-done nil)
                 (org-agenda-skip-scheduled-if-done nil)
                 (org-agenda-start-with-log-mode 'clockcheck)
                 (org-agenda-start-with-clockreport-mode t)
                 (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :step week :scope agenda))
                 (org-agenda-span 'week)
                 ))
     ("r3" "3. Get Current: Review last week's work"
      agenda "" ((org-agenda-overriding-header "3. Get Current: Review completed tasks")
                 ;; (org-agenda-entry-types '(:timestamp :sexp))
                 (org-agenda-archives-mode t)
                 (org-agenda-sorting-strategy '(time-up))
                 (org-agenda-log-mode-items '(closed))
                 (org-agenda-start-with-log-mode t)
                 (org-agenda-skip-deadline-if-done nil)
                 (org-agenda-skip-scheduled-if-done nil)
                 (org-agenda-span 'week)))
     ("r4" "4. Get Current: Identify stuck and unscheduled work"
      ((stuck ""
              ((org-agenda-overriding-header "4.1. Get Current: Stuck")))
       (todo "WAIT|DELE"
             ((org-agenda-overriding-header "4.2. Get Current: Waiting and delegated")))
       (tags-todo "-BLOCKED=\"t\"/!TODO"   ;; this matches all TODOs except those that are blocked
                  ((org-agenda-overriding-header "4.3. Get Current: Unblocked and unscheduled")
                   (org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-todo-ignore-deadlines 'near)
                   (org-agenda-tags-todo-honor-ignore-options t)
                   )))
      ((org-agenda-archives-mode nil)
       (org-agenda-dim-blocked-tasks nil)))))
  (add-to-list 'org-refile-targets '(org-agenda-files :maxlevel . 3)))

(defun dds-org/post-init-org/config-crypt ()
  (org-crypt-use-before-save-magic)
  (setq
   org-crypt-disable-auto-save t
   org-crypt-key user-mail-address
   ))

(defun dds-org/post-init-org/config-clock ()
  (org-clock-persistence-insinuate)
  (setq
   org-clock-persist t
   ))

(defun dds-org/post-init-org/config-export ()
  (setq
   org-export-with-planning t
   ))

(defun dds-org/post-init-org/config-habit ()
  (setq
   org-habit-following-days 7
   org-habit-graph-column 50
   org-habit-preceding-days 14
   ))

(defun dds-org/post-init-org/config-src ()
  (setq
   org-src-preserve-indentation t
   ))

(defun dds-org/post-init-org/config-ob ()
  (setq
   ))

(defun dds-org/post-init-org/config-faces ()
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(defun dds-org/post-init-org/config-ical ()
  (setq
   org-icalendar-store-UID t
   ;; Google calendar doesn't support included TOODs
   org-icalendar-include-todo nil
   org-icalendar-combined-agenda-file "~/plan/ical"
   ))

(defun dds-org/pre-init-org-journal ()
  (use-package org-journal
    :hook ((org-journal-mode . (lambda () (visual-fill-column-mode 1))))
    :init
    (setq-default
     org-journal-dir "~/plan/journal/"
     org-journal-file-format "%Y%m%d.org"
     org-journal-enable-agenda-integration t
     )))

(defun dds-org/post-init-org-bullets ()
  (use-package org-bullets
    :config
    (setq
     org-bullets-bullet-list
       '(
         "◉"
         "○"
         "✸"
         "●"
         "►"
         "◆"
         "▸"
         ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ► ▶ • ★ ▸
        ))))


