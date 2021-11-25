;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Pedro Henrique Romano"
      user-mail-address "mail@pedroromano.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

(setq doom-font "Noto Mono-12")
(setq doom-variable-pitch-font "Noto Sans CJK JP-12")
(+global-word-wrap-mode +1)

(defun phr/wsl-call-process (command &rest parameters)
  (call-process "/mnt/c/Users/Pedro Romano/CLionProjects/fork/cmake-build-debug/fork.exe"
                nil nil nil
                command
                (string-join parameters " ")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory org-directory)
(setq deft-directory org-directory)

(after! browse-url
  (defun browse-url-msedge (url &optional _new-window)
    "Ask the Microsoft Edge browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (phr/wsl-call-process browse-url-msedge-program url))

  (setq browse-url-msedge-program "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe")

  (setq browse-url-browser-function #'browse-url-msedge))

(setq playnite-program "C:\\Users\\Pedro Romano\\AppData\\Local\\Playnite\\Playnite.DesktopApp.exe")

(defun phr/org-playnite-start (id)
  (phr/wsl-call-process playnite-program "--start" id))

(setq vlc-program "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")

(defun phr/vlc-open-file (filename)
  (phr/wsl-call-process vlc-program filename))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun phr/org-roam-capture-move-point ()
  (org-up-heading-safe)
  (forward-word)
  (backward-word))

(after! org
  (setq org-startup-with-inline-images t)
  (setq org-attach-auto-tag nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-element-use-cache nil)
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "* ${title}\n")
           :unnarrowed t
           :immediate-finish t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "%?" :target
          (file+head "%<%Y-%m-%d>.org" "* %<%Y-%m-%d>\n"))))
  (advice-add 'org-roam-node-find :after #'phr/org-roam-capture-move-point)
  (org-link-set-parameters "playnite" :follow #'phr/org-playnite-start)
  (org-link-set-parameters "exe" :follow #'phr/wsl-call-process)
  (org-link-set-parameters "vlc" :follow #'phr/vlc-open-file)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)
  (use-package! org-edna)
  (org-edna-mode))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! ddskk
  (setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
  (setq skk-tut-file "./.emacs.d/.local/straight/repos/ddskk/etc/SKK.tut"))

(use-package! vulpea)

(setq org-agenda-prefix-format
      '((agenda . " %i %(vulpea-agenda-category 16) %?-16t% s")
        (todo . " %i %(vulpea-agenda-category 16) ")
        (tags . " %i %(vulpea-agenda-category 16) ")
        (search . " %i %(vaulpea-agenda-category 16) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(defun phr/org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun phr/org-roam-project-files ()
  "Return a list of note files containing 'project' tag."
  (seq-map
   #'car
   (org-roam-db-query
    [:select :distinct [nodes:file]
     :from nodes
     :join files :on (= nodes:file files:file)
     :where (not (is todo))])))

(defun phr/org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (phr/org-roam-project-files)))

(advice-add 'org-agenda :before #'phr/org-agenda-files-update)

(defun phr/org-roam-get-first-image (filename) filename)

(defun phr/image-dired-create-thumb-for-org-file (args)
  (cons (phr/org-roam-get-first-image (car args)) (cdr args)))

(advice-add 'image-dired-create-thumb-1 :filter-args #'phr/image-dired-create-thumb-for-org-file)

(defun phr/org-roam-toggle-buffer (&rest _)
  (cond
   ((and (eq (org-roam-buffer--visibility) 'visible) (not (phr/org-roam-note-p)))
    (org-roam-buffer-toggle))
   ((and (not (eq (org-roam-buffer--visibility) 'visible)) (phr/org-roam-note-p))
    (org-roam-buffer-toggle))))

(advice-add 'switch-to-buffer :after #'phr/org-roam-toggle-buffer)

(after! elfeed
  (setq elfeed-search-filter "+unread")

  (defun phr/elfeed-save-after-every-update (_url)
    (elfeed-db-save-safe))

  (add-hook 'elfeed-update-hooks #'phr/elfeed-save-after-every-update)

  (defun phr/elfeed-update-helper (feeds)
    (when feeds
      (elfeed-update-feed (car feeds))
      (run-at-time 2 0 #'phr/elfeed-update-helper (cdr feeds))))

  (defun phr/elfeed-update ()
    (interactive)
    (phr/elfeed-update-helper (elfeed--shuffle (elfeed-feed-list)))))
