;;; lisp/server.el -*- lexical-binding: t; -*-

(use-package! server
  :config
  (unless (server-running-p) (server-start)))
