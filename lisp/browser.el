;;; lisp/browser.el -*- lexical-binding: t; -*-

(after! browse-url
  (defun browse-url-msedge (url &optional _new-window)
    "Ask the Microsoft Edge browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (phr/wsl-call-process browse-url-msedge-program url))

  (setq browse-url-msedge-program "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe")

  (setq browse-url-browser-function #'browse-url-msedge)

  (defun phr/browse-url-nyaa (url &optional new-window)
    (browse-url-msedge
     (replace-regexp-in-string "nyaa.si/download/\\([0-9]+\\).torrent" "nyaa.si/view/\\1" url)
     new-window))

  (add-to-list 'browse-url-handlers '("nyaa.si" . phr/browse-url-nyaa)))

(use-package! eww
  :config
  (setq shr-width 120)
  (setq url-cookie-file "/tmp/emacs-url-cookies")

  (defun phr/eww-readable (url)
    "Open URL in eww, with eww-readable enabled and proportional fonts disabled
  Based on https://emacs.stackexchange.com/questions/36284/how-to-open-eww-in-readable-mode"
    (interactive "sEnter URL: ")
    (let ((buffer (get-buffer-create "*eww*")))
      (with-current-buffer buffer
        (autoload 'eww-setup-buffer "eww")
        (eww-setup-buffer)
        (setq-local shr-use-fonts nil)
        (url-retrieve url
                      (lambda (status url buffer)
                        (eww-render status url nil buffer)
                        (switch-to-buffer buffer)
                        (eww-readable))
                      (list url buffer))))))
