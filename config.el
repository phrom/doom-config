;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Pedro Henrique Romano"
      user-mail-address "mail@pedroromano.org")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

(setq doom-font "Noto Mono-12")
(setq doom-variable-pitch-font "Noto Sans CJK JP-12")
(+global-word-wrap-mode +1)

(setq load-prefer-newer t)
(setq confirm-kill-processes nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(use-package! el-patch)

(defun phr/clean-history ()
  (interactive)
  (recentf-cleanup)
  (clean-buffer-list))

(defun phr/goto-random-line ()
  (interactive)
  (let* ((line-count (count-lines (point-min) (point-max)))
         (random-line (random line-count)))
    (goto-char (point-min))
    (forward-line random-line)))

(defun phr/ignore-warnings (orig-fun &rest args)
  "Run function while ignoring warning messages, to be used with advice-add :around"
  (let ((warning-minimum-log-level :error))
    (apply orig-fun args)))

(defun phr/load-files (path)
  (dolist (file (remove-if (lambda (x) (member x '("." "..")))
                         (directory-files path)))
    (load! file path)))

(phr/load-files (expand-file-name "lisp" doom-private-dir))

(defun phr/org-roam-get-first-image (filename) filename)

(defun phr/image-dired-create-thumb-for-org-file (args)
  (cons (phr/org-roam-get-first-image (car args)) (cdr args)))

(advice-add 'image-dired-create-thumb-1 :filter-args #'phr/image-dired-create-thumb-for-org-file)

