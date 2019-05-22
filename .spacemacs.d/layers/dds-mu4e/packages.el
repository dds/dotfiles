(setq dds-mu4e-packages
      '(
        mu4e
        mu4e-query-fragments
        ))

(defun dds-mu4e/post-init-mu4e ()
  (use-package mu4e
    :hook ((mu4e-compose-mode . turn-on-flyspell)
           (mu4e-compose-mode . turn-on-visual-line-mode)
           (mu4e-compose-pre . dds-mu4e//set-from-address)
           (mu4e-compose-mode . (lambda () (org-mu4e-compose-org-mode)))
           (mu4e-view-mode . turn-on-visual-line-mode)
           (mu4e-view-mode . emojify-mode))
    :bind (
           :map mu4e-headers-mode-map
             ("z" . dds-mu4e/mark-spam-and-next)
           :map mu4e-view-mode-map
             ("z" . dds-mu4e/view-mark-spam-and-next)
           )
    :config
    (add-to-list 'Info-directory-list "~/src/mu/mu4e")
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
     mu4e-user-mail-address-list '(
                                   "davidsmith@acm.org"
                                   "david.daniel.smith@gmail.com"
                                   "dds@bosabosa.org"
                                   "david.smith@gatehub.net"
                                   "david@cad.xyz"
                                   "dsmith@nerdwallet.com"
                                   )
     mu4e-refile-folder (lambda (msg) (dds-mu4e//maildir-subfolder "archive" msg))
     mu4e-trash-folder (lambda (msg) (dds-mu4e//maildir-subfolder "trash" msg))
     mu4e-drafts-folder "/drafts"
     mu4e-sent-messages-behavior 'delete
     mu4e-view-show-addresses t
     mu4e-view-show-images nil
     mu4e-headers-sort-direction 'ascending
     )
    ;; contexts, i.e. mail accounts
    (dds-mu4e/post-init-mu4e/config-contexts)
    ;; include query fragments to avoid repetition
    (require 'mu4e-query-fragments)
    ;; message composition
    (dds-mu4e/post-init-mu4e/config-compose)
    ;; sending mail
    (dds-mu4e/post-init-mu4e/config-sendmail)
    ))

(defun dds-mu4e/post-init-mu4e/config-contexts ()
  (setq
   mu4e-context-policy 'pick-first
   mu4e-contexts
   `(,(make-mu4e-context
       :name "nw"
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/nerdwallet" (mu4e-message-field msg :maildir))))
       :vars `(( mu4e-mail-from-address . "dsmith@nerdwallet.com" )
               ( mu4e-compose-signature .
                 (concat
                  "David D. Smith\n"
                  "\n"
                  "dsmith@nerdwallet.com\n"
                  "415-779-5893\n"
                  "875 Stevenson St. - 5th Floor\n"
                  "San Francisco, CA 94103\n\n"
                  "<http://www.nerdwallet.com/>\n"
                  "Providing clarity for all of life's financial decisions\n"
                  ))))
     ,(make-mu4e-context
       :name "gmail"
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
       :vars `(( mu4e-mail-from-address . "davidsmith@acm.org" )
               ( mu4e-compose-signature . t)))
     ,(make-mu4e-context
       :name "bosabosa"
       :match-func (lambda (msg)
                     (or (when msg
                           (mu4e-message-contact-field-matches msg :to "bosabosa.org"))
                         (ignore-errors
                           (string-match-p "bosabosa.org" (message-sendmail-envelope-from)))))
       :vars '(( mu4e-mail-from-address . "dds@bosabosa.org" )
               ( mu4e-compose-signature . t)))
     ,(make-mu4e-context
       :name "cad"
       :match-func (lambda (msg)
                     (when msg
                       (string-prefix-p "/cad" (mu4e-message-field msg :maildir))))
       :vars `(( mu4e-mail-from-address . "david@cad.xyz" )
               ( mu4e-compose-signature . t)))
     )))

(defun dds-mu4e/init-mu4e-query-fragments ())
(defun dds-mu4e/post-init-mu4e-query-fragments ()
  (use-package mu4e-query-fragments
    :bind (
           :map mu4e-main-mode-map
              ("s" . mu4e-query-fragments-search)
           :map mu4e-headers-mode-map
           ("s" . mu4e-query-fragments-search)
           )
    :config
    (setq mu4e-query-fragments-list
          `(("%recent" . "date:18m..")
            ("%spam" . "maildir:/.*\/spam/")
            ("%hidden" . "(maildir:/ripple\/.*/ or %spam or flag:draft or flag:trashed)")
            ("%family" . ,(mapconcat (lambda (c) (format "contact:\"%s\"" c))
                                     '("propilotmag.com" "wargolem" "ajsmith" "joelleegger")
                                     " or ")))
          mu4e-query-fragments-append " and %recent and not %hidden")))

(defun dds-mu4e/post-init-mu4e/config-compose ()
  (with-eval-after-load 'message
    (setq mml-enable-flowed nil
          message-citation-line-format "On %a, %b %e, %Y at %k:%M %z, %f wrote:\n"
          message-citation-line-function 'message-insert-formatted-citation-line
          message-confirm-send t
          message-fill-column nil
          message-interactive t
          message-kill-buffer-on-exit t
          message-send-mail-function 'message-send-mail-with-sendmail
          message-sendmail-envelope-from 'header)))

(defun dds-mu4e/post-init-mu4e/config-sendmail ()
  (use-package sendmail
    :config
    (setq send-mail-function 'sendmail-send-it)
    (setq mail-specify-envelope-from t)
    (setq sendmail-program "msmtpq")))
