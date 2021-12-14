;;; lisp/org.el -*- lexical-binding: t; -*-

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

  (add-hook 'org-after-todo-state-change-hook #'phr/org-handle-reset-property)

  (defun phr/org-sort-entries ()
    (interactive)
    (org-sort-entries nil ?a)
    (org-sort-entries nil ?s)
    (org-sort-entries nil ?p)
    (org-sort-entries nil ?o))

  (defun phr/org-save-all-and-kill-buffers ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'org-mode)
          (save-buffer)
          (kill-buffer)))))

  (defun phr/org-autosave ()
    (run-at-time nil 60 #'org-save-all-org-buffers))

  (phr/org-autosave))

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

(after! org-roam
  (setq org-roam-directory org-directory)

  (cl-defmethod org-roam-node-category ((node org-roam-node))
    (format "[%s]" (alist-get "CATEGORY" (org-roam-node-properties node) nil nil #'equal)))

  (setq org-roam-node-display-template "${category:16} ${olp} > ${title:*}")

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

  (add-hook 'doom-switch-buffer-hook #'phr/org-roam-toggle-buffer)

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
                (like properties '"%(\"CATEGORY\" . \"project\")%"))]))))

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

(after! (org-roam org-agenda)
  (defun phr/org-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (interactive)
    (setq org-agenda-files (phr/org-roam-project-files)))

  (advice-add 'org-agenda :before #'phr/org-agenda-files-update))

(use-package! org-edna
  :after org
  :config
  (org-edna-mode))

(use-package! org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-groups
        '((:name "Deadline" :and (:deadline t
                                  :todo "TODO"
                                  :not (:time-grid t)
                                  :not (:log closed)
                                  :not (:log clocked))
           :order 1)
          (:name "Special" :category ("holiday" "birthday") :order 1)
          (:name "Habits Done" :and (:habit t :log state) :order 8)
          (:discard (:log state))
          (:name "Time Log" :log clocked :order 10)
          (:name "Tasks Finished" :todo "DONE" :log closed :order 9)
          (:name "Habits" :and (:habit t :not (:time-grid t)) :order 4)
          (:name "Overdue Tasks" :and (:todo t :scheduled past :not (:habit t)) :order 2)
          (:name "Scheduled" :time-grid t :order 5)
          (:name "Unscheduled for Today" :todo t :order 3)
          (:discard (:anything t))))
  (org-super-agenda-mode 1))

(after! (org-refile org-roam)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets '((phr/org-roam-project-files . (:maxlevel . 2))))
  (org-refile-get-targets))

(after! evil-org
  (define-key evil-org-mode-map (kbd "<normal-state> RET") nil)
  (define-key evil-org-mode-map (kbd "<normal-state> <return>") nil)
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
