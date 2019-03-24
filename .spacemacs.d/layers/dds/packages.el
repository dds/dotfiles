(defconst dds-packages
  '(
    secrets
    ))

(defconst auth-sources
  '(
    "secrets:session"
    "secrets:Login"
    "~/.authinfo.gpg"
    ))

(defun dds/init-secrets ()
  (use-package secrets :defer t))

(defun dds/post-init-ghub ()
  (require 'secrets))
