;;; lisp/ahk.el -*- lexical-binding: t; -*-

(use-package! ahk-mode
  :config
  (defun ahk-run-script ()
    (interactive)
    (phr/wsl-call-process (buffer-file-name))))
