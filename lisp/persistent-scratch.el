;;; lisp/persistent-scratch.el -*- lexical-binding: t; -*-

(use-package! persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (setq persistent-scratch-save-file (expand-file-name ".persistent-scratch" doom-etc-dir))
  (setq persistent-scratch-autosave-interval 60)
  (defun phr/persistent-scratch-buffer-p ()
    (or (persistent-scratch-default-scratch-buffer-p)
        (and (not (buffer-file-name))
             (not (string-match-p " *\\*" (buffer-name)))
             (eq major-mode 'org-mode))))
  (setq-default major-mode 'org-mode)
  (setq persistent-scratch-scratch-buffer-p-function 'phr/persistent-scratch-buffer-p))
