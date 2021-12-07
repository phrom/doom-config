;;; lisp/set-working-directory.el -*- lexical-binding: t; -*-

(after! zzz-finished
  (dolist (buffer (buffer-list))
    (when (not (buffer-file-name buffer))
      (with-current-buffer buffer
        (cd "~")))))
