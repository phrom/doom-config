;;; lisp/deft.el -*- lexical-binding: t; -*-

(after! deft
  (setq deft-directory org-directory)
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-file-limit 100))
