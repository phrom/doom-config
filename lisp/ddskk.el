;;; lisp/ddskk.el -*- lexical-binding: t; -*-

(after! ddskk
  (setq skk-user-directory (expand-file-name "skk" doom-etc-dir))
  (setq skk-get-jisyo-directory (expand-file-name "skk-get-jisyo" skk-user-directory))
  (setq skk-get-jisyo-directory (expand-file-name "skk-get-jisyo" doom-etc-dir))
  (setq skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))
  (setq skk-tut-file (file-name-concat doom-local-dir "straight/repos/ddskk/etc/SKK.tut")))
