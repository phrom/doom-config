;;; lisp/theme.el -*- lexical-binding: t; -*-

(use-package! time
  :config
  (setq display-time-format "%Y-%m-%d %H:%M:%S")
  (setq display-time-load-average-threshold 0)
  (setq display-time-interval 1)
  (display-time-mode))

(use-package! nyan-mode
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  (nyan-mode t))

(use-package vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

(after! uniquify
  (setq uniquify-buffer-name-style 'forward))
