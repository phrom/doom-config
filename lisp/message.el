;;; lisp/message.el -*- lexical-binding: t; -*-

; Taken from https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun phr/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun phr/add-timestamp-to-message (format-string &rest args)
  "Advice to run before `message' that prepends a timestamp to each message."
  (unless (string-equal format-string "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (phr/current-time-microseconds) " ")))))

(advice-add 'message :before 'phr/add-timestamp-to-message)
