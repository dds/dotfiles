(defun dds-mu4e//maildir-subfolder (subfolder msg)
  (if msg
      (let ((maildir (mu4e-message-field msg :maildir)))
        (replace-regexp-in-string "^\\(/\[^/]+\\)/\\(.*\\)$" (concat "\\1/" subfolder) maildir ))))

(defun dds-mu4e//set-from-address ()
  (let* ((msg mu4e-compose-parent-message)
         (from (or (when (and msg
                              (mu4e-message-contact-field-matches msg :to "bosabosa.org"))
                     (cdar (mu4e-message-field-raw msg :to)))
                   (or mu4e-mail-from-address user-mail-address))))
    (set (make-local-variable 'user-mail-address) from)))

(defun dds-mu4e/mark-spam-and-next ()
  "Move the message at point to the correct spam folder, then move to the next message."
  (interactive)
  (let* ((original-maildir (mu4e-message-field (mu4e-message-at-point) :maildir))
         (spam-maildir (replace-regexp-in-string
                        "^\\(/\[^/]+\\)/\\(.*\\)$" "\\1/spam"
                        original-maildir)))
    (mu4e-mark-set 'move spam-maildir)
    (when mu4e-headers-advance-after-mark (mu4e-headers-next))))

(defun dds-mu4e/view-mark-spam-and-next ()
  (interactive)
  (mu4e~view-in-headers-context
   (dds-mu4e/mark-spam-and-next)))
