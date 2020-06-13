;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(when window-system (set-frame-size (selected-frame) 120 50))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "David D. Smith")

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
;; (setq doom-font (font-spec :family "Source Code Pro" :size 12))
(setq doom-font "Source Code Pro-13")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/plan/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; (defun dds//projectilize (f &rest args)
;;   (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
;;     (call-interactively f)))
;; (advice-add 'spacemacs/python-start-or-switch-repl :around 'dds//projectilize)
;;

;; Add homebrew emacs lisp to load path
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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
    ;; Make Emacs handle org-protocol:// URLs, for browser integration
    (require 'org-protocol)
    ;; Exporting to ical for google calendar
    (dds-org/post-init-org/config-ical)
    ))


(after! org-mode
  (dds-org/post-init-org))

(defun org-journal//safe-is-journal ()
  "Determine if file is a journal file."
  (when (buffer-file-name)
    (string-match (org-journal-dir-and-file-format->pattern) (buffer-file-name))))

(use-package org-journal
  :hook ((org-journal-mode . (lambda () (visual-fill-column-mode 1))))
  :init
  (defalias 'org-journal-is-journal 'org-journal//safe-is-journal)
  (setq-default
   org-journal-dir "~/plan/journal/"
   org-journal-file-format "%Y%m%d.org"
   org-journal-enable-agenda-integration t))

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

(after! go-mode
  (set-lookup-handlers! 'go-mode
    :definition #'godef-jump
    :references #'go-guru-referrers
    :documentation #'godef-describe))

(use-package! mu4e
  :hook ((mu4e-compose-mode . turn-on-flyspell)
         (mu4e-compose-mode . turn-on-visual-line-mode)
         (mu4e-compose-pre . mu4e//set-from-address)
         ;; (mu4e-compose-mode . (lambda () (org-mu4e-compose-org-mode)))
         (mu4e-view-mode . turn-on-visual-line-mode))
  :config
  (map! :map mu4e-headers-mode-map
        :nv "#" 'mu4e//mark-spam-and-next)
  (map! :map mu4e-headers-mode-map
        :nv "T" 'mu4e-headers-mark-thread)
  (map! :map mu4e-view-mode-map
        :nv "#" 'mu4e//view-mark-spam-and-next)
  (map! :map mu4e-view-mode-map
        :nv "T" 'mu4e-view-mark-thread)
  (require 'mu4e-query-fragments)
  (setq
   mu4e-maildir "~/mail"
   mu4e-confirm-quit nil
   mu4e-use-fancy-chars nil
   mu4e-attachment-dir "/tmp"
   mu4e-headers-include-related nil
   mu4e-bookmarks
   '(
     ("not (flag:sent or maildir:/.*\/archive/ or maildir:/.*\/sent/ or %hidden)" "inbox" ?i)
     ("flag:unread and not %hidden" "unread" ?u)
     ("flag:flagged" "flagged" ?f)
     ("%recent and not %hidden" "all mail" ?a)
     )
   mu4e-compose-dont-reply-to-self t
   mu4e-change-filenames-when-moving t
   mu4e-user-mail-address-list
   '(
     "davidsmith@acm.org"
     "david.daniel.smith@gmail.com"
     "dds@bosabosa.org"
     "dsmith@nerdwallet.com"
     )
   mu4e-refile-folder (lambda (msg) (mu4e//maildir-subfolder "archive" msg))
   mu4e-trash-folder (lambda (msg) (mu4e//maildir-subfolder "trash" msg))
   mu4e-drafts-folder "/drafts"
   mu4e-sent-messages-behavior 'delete
   mu4e-view-show-addresses t
   mu4e-view-show-images nil
   mu4e-headers-sort-direction 'descending
   mu4e-context-policy 'pick-first
   ;; contexts, i.e. mail accounts
   mu4e-contexts
   `(,(make-mu4e-context
       :name "nw"
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/nerdwallet" (mu4e-message-field msg :maildir))))
       :vars `(( mu4e-mail-from-address . "dsmith@nerdwallet.com" )
               ( message-signature-file . "~/.signature.nerdwallet" )
               ( mu4e-compose-signature . t )))
     ,(make-mu4e-context
       :name "gmail"
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
       :vars `(( mu4e-mail-from-address . "davidsmith@acm.org" )
               ( message-signature-file . "~/.signature" )
               ( mu4e-compose-signature . t)))
     ,(make-mu4e-context
       :name "bosabosa"
       :match-func (lambda (msg)
                     (or (when msg
                           (mu4e-message-contact-field-matches msg :to "bosabosa.org"))
                         (ignore-errors
                           (string-match-p "bosabosa.org" (message-sendmail-envelope-from)))))
       :vars '(( mu4e-mail-from-address . "dds@bosabosa.org" )
               ( message-signature-file . "~/.signature" )
               ( mu4e-compose-signature . t)))
     )
   mml-enable-flowed nil
   message-citation-line-format "On %a, %b %e, %Y at %k:%M %z, %f wrote:\n"
   message-citation-line-function 'message-insert-formatted-citation-line
   message-confirm-send t
   message-fill-column nil
   message-interactive t
   message-kill-buffer-on-exit t
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header
   send-mail-function 'sendmail-send-it
   mail-specify-envelope-from t
   sendmail-program "msmtpq"
   )
  )

(use-package! mu4e-query-fragments
  :bind (
         :map mu4e-main-mode-map
         ("s" . mu4e-query-fragments-search)
         :map mu4e-headers-mode-map
         ("s" . mu4e-query-fragments-search)
         )
  :config
  (setq! mu4e-query-fragments-list
         `(("%recent" . "date:18m..")
           ("%spam" . "maildir:/.*\/spam/")
           ("%hidden" . "(maildir:/ripple\/.*/ or %spam or flag:draft or flag:trashed)")
           ("%family" . ,(mapconcat (lambda (c) (format "contact:\"%s\"" c))
                                    '("propilotmag.com" "wargolem" "ajsmith" "joelleegger")
                                    " or ")))
         mu4e-query-fragments-append " and %recent and not %hidden"))

(defun mu4e//maildir-subfolder (subfolder msg)
  (if msg
      (let ((maildir (mu4e-message-field msg :maildir)))
        (replace-regexp-in-string "^\\(/\[^/]+\\)/\\(.*\\)$" (concat "\\1/" subfolder) maildir ))))

(defun mu4e//set-from-address ()
  (let* ((msg mu4e-compose-parent-message)
         (from (or (when (and msg
                              (mu4e-message-contact-field-matches msg :to "bosabosa.org"))
                     (cdar (mu4e-message-field-raw msg :to)))
                   (or mu4e-mail-from-address user-mail-address))))
    (set (make-local-variable 'user-mail-address) from)))

(defun mu4e//mark-spam-and-next ()
  "Move the message at point to the correct spam folder, then move to the next message."
  (interactive)
  (let* ((original-maildir (mu4e-message-field (mu4e-message-at-point) :maildir))
         (spam-maildir (replace-regexp-in-string
                        "^\\(/\[^/]+\\)/\\(.*\\)$" "\\1/spam"
                        original-maildir)))
    (mu4e-mark-set 'move spam-maildir)
    (when mu4e-headers-advance-after-mark (mu4e-headers-next))))

(defun mu4e//view-mark-spam-and-next ()
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e//mark-spam-and-next)))

(after! python
  (add-hook 'python-mode-local-vars-hook 'pyvenv//build/venv)
  (advice-add 'pyenv-mode-set :after 'pyenv-venv-wrapper-act)
  (advice-add 'pyenv-mode-unset :after 'pyenv-venv-wrapper-deact))

(defun pyenv-venv-wrapper-act ()
  (setenv "VIRTUAL_ENV" (shell-command-to-string "_pyenv_virtualenv_hook; echo -n $VIRTUAL_ENV")))

(defun pyenv-venv-wrapper-deact ()
  (setenv "VIRTUAL_ENV"))

;; https://github.com/jorgenschaefer/pyvenv/issues/51#issuecomment-474785730
(defun pyvenv//build/venv ()
  "Automatically activates pyvenv version if build/venv/* directory exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((venv-path (car (f-glob (f-join (f-expand "build/venv" path) "*")))))
       (if (and venv-path (f-exists? venv-path))
           (progn
             (pyvenv-activate venv-path)
             t)
         nil)))))
