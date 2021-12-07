;;; lisp/vlc.el -*- lexical-binding: t; -*-

(setq vlc-program "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")

(defun phr/vlc-open-file (filename)
  (phr/wsl-call-process vlc-program filename))

(after! ol
  (org-link-set-parameters "vlc" :follow #'phr/vlc-open-file))
