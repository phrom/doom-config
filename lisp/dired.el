;;; lisp/dired.el -*- lexical-binding: t; -*-

(after! dired
  ;; -A: do not include . and ..
  ;; -l: show file details
  ;; -u: show access time
  ;; -v: sort numbers within text
  ;; -h: print file sizes like 1K 234M 5G
  (setq dired-listing-switches "-aAluvh --group-directories-first")

  ;; broken
  (require 'dired-aux)
  (defun phr/dired-hook-auto-open-on-image-dired ()
    (when (eq major-mode 'dired-mode)
      (let ((all-images nil))
        (catch 'early-exit
          (dired-map-dired-file-lines
           (lambda (path)
             (if (member (downcase (file-name-extension path))
                         '("jpg" "jpeg" "png" "gif"))
                 (setq all-images t)
               (setq all-images nil)
               (throw 'early-exit nil)))))
        (when all-images
          (image-dired default-directory)
          (image-dired-display-thumbnail-original-image)
          (delete-other-windows)
          (switch-to-buffer image-dired-thumbnail-buffer)
          (split-window-right)
          (other-window 1)
          (switch-to-buffer image-dired-display-image-buffer)
          (other-window 1)
          (throw 'intentionally-unhandled "This is fine!")))))

  (add-hook 'dired-after-readin-hook 'phr/dired-hook-auto-open-on-image-dired)

  (setq image-dired-cmd-create-temp-image-options '("-size" "%wx%h" "%f[0]" "-resize" "%wx%h" "-strip" "jpeg:%t")))

(after! image-dired
  (setq image-dired-thumb-height 400)
  (setq image-dired-thumb-width 400)
  (setq image-dired-thumb-margin 10)
  (setq image-dired-show-all-from-dir-max-files 150))

(after! image-mode
  (setq image-auto-resize 'fit-height)
  (evil-define-key 'normal image-mode-map (kbd "n") 'image-next-file)
  (evil-define-key 'normal image-mode-map (kbd "p") 'image-previous-file))
