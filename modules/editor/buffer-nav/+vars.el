;; +vars.el -*- lexical-binding: t; -*-

(setq avy-all-windows t

      ;; Scrolling
      hscroll-margin 2
      hscroll-step 1
      auto-hscroll-mode 'current-line
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil

      ;; Cursor
      ;; blink-cursor-mode -1
      blink-matching-paren nil
      x-stretch-cursor nil
      )


;;-- lines
(setq so-long-threshold 5000

      ;; Lines
      display-line-numbers             t
      display-line-numbers-major-tick  20
      display-line-numbers-width 4
      display-line-numbers-type t
      overflow-newline-into-fringe t

      line-move-ignore-invisible t

      fringes-outside-margins t

      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      )
;;-- end lines
