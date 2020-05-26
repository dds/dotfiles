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
(setq doom-font (font-spec :family "Source Code Pro" :size 12))

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
  (map! :map mu4e-view-mode-map
        :nv "#" 'mu4e//view-mark-spam-and-next)
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
