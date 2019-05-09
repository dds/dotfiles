(defun dds-org/org-agenda-toggle-deadlines-and-habits ()
  (interactive)
  (org-agenda-toggle-deadlines)
  (org-habit-toggle-habits))
