;;; lisp/powershell.el -*- lexical-binding: t; -*-

(use-package! powershell
  :config
  (defun powershell ()
    (interactive)
    (ansi-term "pwsh.exe")))
