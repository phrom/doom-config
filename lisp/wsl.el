;;; lisp/wsl.el -*- lexical-binding: t; -*-

(defun phr/wsl-convert-path (path)
  (cond
   ((not path) path)
   ((string-prefix-p "/mnt/" path)
      (let* ((path (replace-regexp-in-string
                    "/mnt/[a-z]/"
                    (lambda (substr)
                      (concat (upcase (substring substr 5 6)) ":/"))
                    path))
             (path (replace-regexp-in-string "/" "\\\\" path)))
        path))
   ((file-exists-p path) (concat "\\\\wsl$\\Debian" (replace-regexp-in-string "/" "\\\\" path)))
   (t path)))

(defun phr/wsl-call-process (command &rest parameters)
  (call-process "/mnt/c/Users/Pedro Romano/CLionProjects/fork/cmake-build-debug/fork.exe"
                nil nil nil
                (phr/wsl-convert-path command)
                (string-join (mapcar #'phr/wsl-convert-path parameters) " ")))

(after! ol
  (org-link-set-parameters "exe" :follow #'phr/wsl-call-process))
