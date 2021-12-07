;;; lisp/steam.el -*- lexical-binding: t; -*-

(defun phr/steam-start (id)
  (phr/wsl-call-process (format "steam://run/%s/" id)))

(after! ol
  (org-link-set-parameters "steam" :follow #'phr/steam-start))
