;;; lisp/calendar.el -*- lexical-binding: t; -*-

(after! calendar
  (setq calendar-christian-all-holidays-flag t)
  (calendar-set-date-style 'iso))
