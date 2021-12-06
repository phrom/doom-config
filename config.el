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

(setq load-prefer-newer t)
(setq confirm-kill-processes nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

; Taken from https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun phr/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun phr/add-timestamp-to-message (format-string &rest args)
  "Advice to run before `message' that prepends a timestamp to each message."
  (unless (string-equal format-string "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (phr/current-time-microseconds) " ")))))

(advice-add 'message :before 'phr/add-timestamp-to-message)

(dolist (buffer (buffer-list))
  (when (not (buffer-file-name buffer))
    (with-current-buffer buffer
      (cd "~"))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(defun phr/goto-random-line ()
  (interactive)
  (let* ((line-count (count-lines (point-min) (point-max)))
         (random-line (random line-count)))
    (goto-char (point-min))
    (forward-line random-line)))

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
(use-package! el-patch)

(defun phr/wsl-call-process (command &rest parameters)
  (call-process "/mnt/c/Users/Pedro Romano/CLionProjects/fork/cmake-build-debug/fork.exe"
                nil nil nil
                command
                (string-join parameters " ")))

(setq playnite-program "C:\\Users\\Pedro Romano\\AppData\\Local\\Playnite\\Playnite.DesktopApp.exe")

(defun phr/playnite-start (id)
  (phr/wsl-call-process playnite-program "--start" id))

(defun phr/steam-start (id)
  (phr/wsl-call-process (format "steam://run/%s/" id)))

(setq vlc-program "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")

(defun phr/vlc-open-file (filename)
  (phr/wsl-call-process vlc-program filename))

(setq org-directory "~/org/")

(after! org
  (setq org-startup-with-inline-images t)
  (setq org-element-use-cache nil)
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c!)")))
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-into-drawer "TIMEBOOK")

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-id)

  (add-hook 'kill-emacs-hook #'org-save-all-org-buffers)

  (defun phr/org-insert-sequence (prompt start end)
    (interactive "sPrompt:\nnStart:\nnEnd:\n")
    (dotimes (i (1+ (- end start)))
      (if (= i 0)
          (org-insert-subheading '(4))
        (org-insert-heading-after-current))
      (insert (format "%s %d" prompt (+ start i)))))

  (defun phr/org-handle-reset-property ()
    (let ((date (org-get-scheduled-time (point)))
          (reset (org-entry-get (point) "RESET")))
      (when (and date reset (string= org-state "DONE"))
        (org-todo 1)
        (org-schedule nil (concat (format-time-string "%Y-%m-%d " date) reset)))))

  (add-hook 'org-after-todo-state-change-hook #'phr/org-handle-reset-property))

(after! org-attach
  (setq org-attach-auto-tag nil)

  (setq phr/org-attach-dir-from-filename t)

  (defun phr/org-attach-dir-advice (oldfun &rest args)
    (if phr/org-attach-dir-from-filename
        (let* ((file-name (file-name-base (buffer-file-name)))
               (hashed-name (md5 file-name)))
          (file-name-concat org-attach-id-dir
                            (substring hashed-name 0 2)
                            (substring hashed-name 2)
                            file-name))
      (apply oldfun args)))

  (advice-add 'org-attach-dir :around #'phr/org-attach-dir-advice))

(after! org-src
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t))

(after! ol
  (org-link-set-parameters "playnite" :follow #'phr/playnite-start)
  (org-link-set-parameters "exe" :follow #'phr/wsl-call-process)
  (org-link-set-parameters "vlc" :follow #'phr/vlc-open-file)
  (org-link-set-parameters "steam" :follow #'phr/steam-start))

(after! org-roam
  (setq org-roam-directory org-directory)
  (setq org-roam-node-display-template "${title:*} ${tags:10}")

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "* ${title}\n")
           :unnarrowed t
           :immediate-finish t)))

  (defun phr/org-roam-capture-move-point ()
    (org-up-heading-safe)
    (forward-word)
    (backward-word))

  (advice-add 'org-roam-node-find :after #'phr/org-roam-capture-move-point)

  (defun phr/org-roam-toggle-buffer ()
    (cond
     ((and (eq (org-roam-buffer--visibility) 'visible)
           (not (phr/org-roam-note-p))
           (not (one-window-p)))
      (org-roam-buffer-toggle))
     ((and (not (eq (org-roam-buffer--visibility) 'visible))
           (phr/org-roam-note-p))
      (org-roam-buffer-toggle))))

  (add-hook 'doom-switch-buffer-hook #'phr/org-roam-toggle-buffer))

(use-package! org-fc
  :config
  (defun phr-org-fc-card-p ()
    (org-entry-get nil "FC_TYPE"))
  (setq org-fc-directories (list org-roam-directory))
  (setq org-fc-review-history-file (expand-file-name "org-fc-reviews.tsv" doom-etc-dir))
  (require 'org-fc-keymap-hint)

  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "RET") 'org-fc-review-flip
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)

  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit))

(after! (org-fc org-roam)
  (setq org-roam-db-node-include-function (lambda () (not (phr-org-fc-card-p)))))

(after! org-habit
  (setq org-habit-preceding-days 30)
  (setq org-habit-graph-column 70))

(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

(after! org-agenda
  (setq org-agenda-span 'day)
  (setq org-agenda-start-day nil)
  (setq org-agenda-start-with-log-mode '(closed clock state))
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-deadline-warning-days 999)

  (setq org-agenda-sorting-strategy
        '((agenda time-up user-defined-up priority-down category-keep)
          (todo priority-down category-keep tag-up)
          (tags priority-down tag-up category-keep)
          (search category-keep alpha-up)))

  (setq org-agenda-prefix-format
        '((agenda . " %i %-20:(phr/org-show-breadcrumbs 16)%?-12t %s")
          (todo . " %i %-16:c %-34:(phr/org-show-breadcrumbs 30)")
          (tags . " %i %-16:c")
          (search . " %i %-16:c")))
  (setq org-agenda-tags-column 100)

  (defun phr/org-show-breadcrumbs (width)
    (let ((breadcrumbs (org-format-outline-path (reverse (org-get-outline-path)) width)))
      (if (= (length breadcrumbs) 0)
          (concat "Category: "(org-get-category))
        breadcrumbs)))

  (setq org-agenda-time-grid
        `((daily today require-timed)
          ,(cl-loop for i from 600 to 2300 by 100 collect i)
          "......" "----------------"))

  (defsubst phr/org-time-grid-p (a)
    (text-property-any 0 (length a) 'face 'org-time-grid a))

  (defun phr/org-agenda-cmp-time-grid (a b)
    (cond ((phr/org-time-grid-p a) -1)
          ((phr/org-time-grid-p b) +1)))

  (setq org-agenda-cmp-user-defined #'phr/org-agenda-cmp-time-grid)

  (defvar-local phr/org-agenda-bulk-function-schedule-one-per-day-interval 0)

  (defun phr/org-agenda-bulk-function-schedule-one-per-day-hook-helper ()
    (setq-local phr/org-agenda-bulk-function-schedule-one-per-day-interval 0)
    (remove-hook 'post-command-hook 'phr/org-agenda-bulk-function-schedule-one-per-day-hook-helper t))

  (defun phr/org-agenda-bulk-function-schedule-one-per-day ()
    (interactive "P")
    (add-hook 'post-command-hook 'phr/org-agenda-bulk-function-schedule-one-per-day-hook-helper)
    (let ((marker (or (org-get-at-bol 'org-hd-marker)
                      (org-agenda-error))))
      (org-with-point-at marker
        (org-back-to-heading t)
        (org-schedule nil (format "+%dd" (cl-incf phr/org-agenda-bulk-function-schedule-one-per-day-interval))))))

  (add-to-list 'org-agenda-bulk-custom-functions '(?O phr/org-agenda-bulk-function-schedule-one-per-day))

  ;; Patch to avoid hiding DEADLINE entries in org-agenda when dependencies are enforced
  ;; (for example, when the DEADLINE is on a parent task representing a whole project,
  ;; and its subtasks are not done)
  (el-patch-defun org-agenda--mark-blocked-entry (entry)
    "If ENTRY is blocked, mark it for fontification or invisibility.

If the header at `org-hd-marker' is blocked according to
`org-entry-blocked-p', then if `org-agenda-dim-blocked-tasks' is
'invisible and the header is not blocked by checkboxes, set the
text property `org-todo-blocked' to `invisible', otherwise set it
to t."
    (when (get-text-property 0 'todo-state entry)
      (let ((entry-marker (get-text-property 0 'org-hd-marker entry))
            (org-blocked-by-checkboxes nil)
	    ;; Necessary so that `org-entry-blocked-p' does not change
	    ;; the buffer.
            (org-depend-tag-blocked nil))
        (when entry-marker
	  (let ((blocked
	         (with-current-buffer (marker-buffer entry-marker)
		   (save-excursion
		     (goto-char entry-marker)
		     (el-patch-swap
                       (org-entry-blocked-p)
                       (and (org-entry-blocked-p) (not (org-get-deadline-time (point)))))))))
	    (when blocked
	      (let ((really-invisible
		     (and (not org-blocked-by-checkboxes)
			  (eq org-agenda-dim-blocked-tasks 'invisible))))
	        (put-text-property
	         0 (length entry) 'org-todo-blocked
	         (if really-invisible 'invisible t)
	         entry)
	        (put-text-property
	         0 (length entry) 'org-filter-type 'todo-blocked entry)))))))
    entry)

  ;; Patch org-agenda-goto to replace current window instead of splitting.
  ;; My tasks are on org-roam files; default org-agenda-goto leads to a 3-way
  ;; horizontal split, including the org-roam backlinks buffer, which is too much.
  (el-patch-defun org-agenda-goto (&optional highlight)
    "Go to the entry at point in the corresponding Org file."
    (interactive)
    (let* ((marker (or (org-get-at-bol 'org-marker)
		       (org-agenda-error)))
	   (buffer (marker-buffer marker))
	   (pos (marker-position marker)))
      ;; FIXME: use `org-switch-to-buffer-other-window'?
      ((el-patch-swap switch-to-buffer-other-window switch-to-buffer) buffer)
      (widen)
      (push-mark)
      (goto-char pos)
      (when (derived-mode-p 'org-mode)
        (org-show-context 'agenda)
        (recenter (/ (window-height) 2))
        (org-back-to-heading t)
        (let ((case-fold-search nil))
	  (when (re-search-forward org-complex-heading-regexp nil t)
	    (goto-char (match-beginning 4)))))
      (run-hooks 'org-agenda-after-show-hook)
      (and highlight (org-highlight (point-at-bol) (point-at-eol))))))

(use-package! org-edna
  :after org
  :config
  (org-edna-mode))

(use-package! org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-groups
        '((:name "Deadline" :and (:deadline t :not (:log closed) :not (:log clocked)) :order 1)
          (:name "Special" :category ("holiday" "birthday") :order 1)
          (:name "Habits Done" :log state :order 8)
          (:name "Tasks Finished" :log closed :order 9)
          (:name "Time Log" :log clocked :order 10)
          (:name "Habits" :and (:habit t :not (:time-grid)) :order 4)
          (:name "Overdue Tasks" :and (:todo t :scheduled past :not (:habit t)) :order 2)
          (:name "Scheduled" :time-grid t :order 5)
          (:name "Unscheduled for Today" :todo t :order 3)
          (:discard (:anything t))))
  (org-super-agenda-mode 1))

(after! evil-org
  (define-key evil-org-mode-map (kbd "<normal-state> RET") nil)
  (define-key evil-org-mode-map (kbd "<normal-state> <return>") nil)
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(after! ddskk
  (setq skk-user-directory (expand-file-name "skk" doom-etc-dir))
  (setq skk-get-jisyo-directory (expand-file-name "skk-get-jisyo" skk-user-directory))
  (setq skk-get-jisyo-directory (expand-file-name "skk-get-jisyo" doom-etc-dir))
  (setq skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))
  (setq skk-tut-file (file-name-concat doom-local-dir "straight/repos/ddskk/etc/SKK.tut")))

(defun phr/org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun phr/org-roam-project-files ()
  "Return a list of note files containing nodes with categories 'task' or 'project'"
  (seq-map
   #'car
   (org-roam-db-query
    [:select :distinct [nodes:file]
     :from nodes
     :join files :on (= nodes:file files:file)
     :where (not (is todo))
     :and (or (like properties '"%(\"CATEGORY\" . \"task\")%")
              (like properties '"%(\"CATEGORY\" . \"project\")%"))])))

(defun phr/org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (interactive)
  (setq org-agenda-files (phr/org-roam-project-files)))

(advice-add 'org-agenda :before #'phr/org-agenda-files-update)

(defun phr/org-roam-get-first-image (filename) filename)

(defun phr/image-dired-create-thumb-for-org-file (args)
  (cons (phr/org-roam-get-first-image (car args)) (cdr args)))

(advice-add 'image-dired-create-thumb-1 :filter-args #'phr/image-dired-create-thumb-for-org-file)

(defun phr/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?a)
  (org-sort-entries nil ?s)
  (org-sort-entries nil ?p)
  (org-sort-entries nil ?o))

(defun phr/clean-history ()
  (interactive)
  (recentf-cleanup)
  (clean-buffer-list))

(after! elfeed
  (setq elfeed-search-filter "+unread")
  (setq elfeed-search-title-max-width 120)
  (setq elfeed-search-date-format '("%Y-%m-%d %H:%M" 16 :left))
  (setq elfeed-show-entry-switch #'switch-to-buffer)
  (setq elfeed-curl-extra-arguments '("-k"))

  (defun phr/recenter ()
    (recenter-top-bottom '(4)))

  (advice-add 'elfeed-next-line :after #'phr/recenter)
  (advice-add 'elfeed-previous-line :after #'phr/recenter)
  (advice-add 'elfeed-search-untag-all-unread :after #'phr/recenter)

  (defun phr/elfeed-save-after-update (&rest _)
    (elfeed-db-save-safe))

  (add-hook 'elfeed-update-hooks #'phr/elfeed-save-after-update)

  (defvar phr/elfeed-feeds-to-update)
  (defvar phr/elfeed-max-concurrent-updates 1)

  (defun phr/elfeed-update ()
    (interactive)
    (setq phr/elfeed-feeds-to-update (elfeed--shuffle (elfeed-feed-list)))
    (add-hook 'elfeed-update-hooks #'phr/elfeed-update-helper)
    (dotimes (_ phr/elfeed-max-concurrent-updates)
      (phr/elfeed-update-helper nil)))

  (defun phr/elfeed-update-helper (_url)
    (if phr/elfeed-feeds-to-update
      (elfeed-update-feed (pop phr/elfeed-feeds-to-update))
      (remove-hook 'elfeed-update-hooks #'phr/elfeed-update-helper)))

  (defvar phr/elfeed-filters
    '((always "+unread +always")
      (torrent "+unread +torrent")
      (youtube "+unread +youtube")
      (games "+unread +games")
      (japan "+unread +japan")
      (misc "+unread +misc")
      (blog "+unread +blog")
      (tech "+unread +tech")
      (reddit "+unread +reddit")
      (podcast "+unread +podcast")
      (mods "+unread +mods")
      (instagram "+unread +instagram")
      (twitter "+unread +twitter")))

  (defun phr/elfeed-filter-set-filter ()
    (interactive)
    (let* ((filter-name (completing-read "Filter: " phr/elfeed-filters))
           (filter (cl-find-if
                    (lambda (x) (equal filter-name (symbol-name (car x))))
                    phr/elfeed-filters)))
      (elfeed-search-set-filter (cadr filter))))

  (defun phr/elfeed-filter-next ()
    (interactive)
    (let ((current-filter
           (cl-member-if (lambda (x) (equal elfeed-search-filter (cadr x)))
                         phr/elfeed-filters)))
      (elfeed-search-set-filter
       (if current-filter
           (cadadr current-filter)
         (cadar phr/elfeed-filters)))
      (if (or elfeed-search-entries
              (equal elfeed-search-filter "+unread"))
          (goto-char (point-min))
        (phr/elfeed-filter-next))))

  (evil-define-key 'normal elfeed-search-mode-map (kbd "S") 'phr/elfeed-filter-set-filter)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "n") 'phr/elfeed-filter-next)

  (advice-add 'phr/elfeed-filter-set-filter :after #'phr/elfeed-save-after-update)

  (defun phr/elfeed-with-eww-readable ()
    (interactive)
    (phr/eww-readable (elfeed-entry-link elfeed-show-entry)))

  (evil-define-key 'normal elfeed-search-mode-map (kbd "B") 'phr/elfeed-eww-readable)

  (defun phr/clamp (min value max)
    (let ((result value))
      (when min (setq result (max min result)))
      (when max (setq result (min max result)))
      result))

  (defvar phr/elfeed-search-feed-min-width nil)
  (defvar phr/elfeed-search-feed-max-width 30)
  (defvar phr/elfeed-search-tags-min-width nil)
  (defvar phr/elfeed-search-tags-max-width nil)

  (el-patch-defun elfeed-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (el-patch-add
           (feed-title (when feed-title
                         (elfeed-format-column
                          feed-title (phr/clamp
                                      phr/elfeed-search-feed-min-width
                                      (length feed-title)
                                      phr/elfeed-search-feed-max-width)
                          :left))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (el-patch-add
           (tags-str (when tags-str
                       (elfeed-format-column
                        tags-str (phr/clamp
                                  phr/elfeed-search-tags-min-width
                                  (length tags-str)
                                  phr/elfeed-search-tags-max-width)))))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")")))))

(use-package! elfeed-score
  :after elfeed

  :config
  (setq elfeed-score-serde-score-file (expand-file-name "elfeed.score" doom-etc-dir))
  (setq elfeed-score-rule-stats-file (expand-file-name "elfeed.stats" doom-etc-dir))
  (elfeed-score-enable)

  (defun phr/elfeed-sort (a b)
    "Sort on elfeed-score, then feed title, then publish date"
    (let ((a-score (elfeed-score-scoring-get-score-from-entry a))
          (b-score (elfeed-score-scoring-get-score-from-entry b))
          (a-feed-title (elfeed-feed-title (elfeed-entry-feed a)))
          (b-feed-title (elfeed-feed-title (elfeed-entry-feed b)))
          (a-date (elfeed-entry-date a))
          (b-date (elfeed-entry-date b)))
      (or (> a-score b-score)
          (and (= a-score b-score) (string< a-feed-title b-feed-title))
          (and (= a-score b-score) (string= a-feed-title b-feed-title) (< a-date b-date)))))

  (setq elfeed-search-sort-function #'phr/elfeed-sort)

  (defun phr/elfeed-score-save-after-update (_url)
    (elfeed-score-serde-write-score-file elfeed-score-serde-score-file))

  (add-hook 'elfeed-update-hooks #'phr/elfeed-score-save-after-update))

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

(after! deft
  (setq deft-directory org-directory)
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-file-limit 100))

(defun phr/ignore-warnings (orig-fun &rest args)
  "Run function while ignoring warning messages, to be used with advice-add :around"
  (let ((warning-minimum-log-level :error))
    (apply orig-fun args)))

(after! (org-refile org-roam)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((phr/org-roam-project-files . (:maxlevel . 2))))
  (org-refile-get-targets))

(after! uniquify
  (setq uniquify-buffer-name-style 'forward))

(after! calendar
  (setq calendar-christian-all-holidays-flag t)
  (calendar-set-date-style 'iso))

(use-package! server
  :config
  (unless (server-running-p) (server-start)))

(use-package! pinentry
  :config
  (pinentry-start))

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

(use-package! nyan-mode
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  (nyan-mode t))

(use-package! time
  :config
  (setq display-time-format "%Y-%m-%d %H:%M:%S")
  (setq display-time-load-average-threshold 0)
  (setq display-time-interval 1)
  (display-time-mode))
