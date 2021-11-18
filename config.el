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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory org-directory)

(after! browse-url
  (defun browse-url-msedge (url &optional _new-window)
    "Ask the Microsoft Edge browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (call-process browse-url-msedge-program nil nil nil url))

  (setq browse-url-msedge-program "/mnt/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")

  (setq browse-url-browser-function #'browse-url-msedge))

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

(after! org
  (setq org-startup-with-inline-images t)
  (setq org-attach-auto-tag nil)
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-element-use-cache nil)
  (setq org-log-done t)
  (setq org-log-into-drawer t)
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

(after! org
  (add-to-list 'org-tags-exclude-from-inheritance "include-in-agenda"))

(defun phr/org-roam-has-todo-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))                     ; (3)

(defvar phr/org-roam-taggers (list))

(defun phr/org-roam-tag-include-in-agenda (tags)
  (if (phr/org-roam-has-todo-p)
      (cons "include-in-agenda" tags)
    (remove "include-in-agenda" tags)))

(add-to-list 'phr/org-roam-taggers #'phr/org-roam-tag-include-in-agenda)

(defun phr/org-roam-tag-by-directory (tags)
  (append tags (butlast (f-split (file-relative-name (buffer-file-name) org-roam-directory)))))

(add-to-list 'phr/org-roam-taggers #'phr/org-roam-tag-by-directory)

(defun phr/org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun phr/org-roam-update-filetags ()
      "Update PROJECT tag in the current buffer."
      (when (and (not (active-minibuffer-window))
                 (phr/org-roam-note-p))
        (save-excursion
          (goto-char (point-min))
          (let* ((tags (vulpea-buffer-tags-get))
                 (original-tags tags))
            (dolist (tagger phr/org-roam-taggers)
              (setq tags (funcall tagger tags)))

            ;; cleanup duplicates
            (setq tags (seq-uniq tags))

            ;; update tags if changed
            (when (or (seq-difference tags original-tags)
                      (seq-difference original-tags tags))
              (apply #'vulpea-buffer-tags-set tags))))))

(add-hook 'find-file-hook #'phr/org-roam-update-filetags)
(add-hook 'before-save-hook #'phr/org-roam-update-filetags)

(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"include-in-agenda\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)

(defun phr/org-roam-tagger-update-all-files ()
  (interactive)
  (dolist (file (org-roam-list-files))
  (message "processing %s" file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (phr/org-roam-update-filetags)
    (save-buffer))))

(defun phr/org-roam-get-first-image (filename) filename)

(defun phr/image-dired-create-thumb-for-org-file (args)
  (cons (phr/org-roam-get-first-image (car args)) (cdr args)))

(advice-add 'image-dired-create-thumb-1 :filter-args #'phr/image-dired-create-thumb-for-org-file)
