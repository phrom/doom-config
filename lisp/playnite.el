;;; lisp/playnite.el -*- lexical-binding: t; -*-

(setq playnite-program "C:\\Users\\Pedro Romano\\AppData\\Local\\Playnite\\Playnite.DesktopApp.exe")

(defun phr/playnite-start (id)
  (phr/wsl-call-process playnite-program "--start" id))

(after! ol
  (org-link-set-parameters "playnite" :follow #'phr/playnite-start))
