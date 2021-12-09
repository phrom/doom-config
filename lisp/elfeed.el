;;; lisp/elfeed.el -*- lexical-binding: t; -*-

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

  (defvar phr/elfeed-feeds-to-update nil)
  (defvar phr/elfeed-max-concurrent-updates 16)
  (defvar phr/elfeed-update-finished-hooks nil)

  (defun phr/elfeed-update ()
    (interactive)
    (setq phr/elfeed-feeds-to-update (elfeed--shuffle (elfeed-feed-list)))
    (add-hook 'elfeed-update-hooks #'phr/elfeed-update-helper)
    (dotimes (_ phr/elfeed-max-concurrent-updates)
      (phr/elfeed-update-helper nil)))

  (defun phr/elfeed-update-helper (_url)
    (if phr/elfeed-feeds-to-update
        (elfeed-update-feed (pop phr/elfeed-feeds-to-update))
      (remove-hook 'elfeed-update-hooks #'phr/elfeed-update-helper)
      (run-hooks phr/elfeed-update-finished-hooks)))

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

  (defun phr/elfeed-score-save-after-update (&rest _url)
    (elfeed-score-rule-stats-write elfeed-score-rule-stats-file))

  (add-hook 'phr/elfeed-update-finished-hooks #'phr/elfeed-score-save-after-update))

(map! :leader
      (:prefix ("e" . "elfeed")
       :desc "Open elfeed" "e" #'elfeed
       :desc "Update all feeds" "u" #'phr/elfeed-update
       :desc "Update a single feed" "U" #'elfeed-update-feed
       :desc "Save database" "s" #'phr/elfeed-save-after-update
       :desc "Compact database" "c" #'elfeed-db-compact))
