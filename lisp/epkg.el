;;; lisp/epkg.el -*- lexical-binding: t; -*-

(use-package! epkg
  :config
  (setq epkg-repository (expand-file-name "epkg" doom-etc-dir)))
