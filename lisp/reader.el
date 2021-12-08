;;; lisp/reader.el -*- lexical-binding: t; -*-

(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-text-width 120))
