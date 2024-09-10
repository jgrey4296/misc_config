;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(dlog! "Setting up Misc Bindings")

;; Add in self-insert-command
;; (let* ((self-insert-vec (make-vector 2 nil))
;;        (make-vec-f (lambda (from to)
;;             (while (< from to)
;;               (aset self-insert-vec 0 from)
;;               (define-key global-map self-insert-vec #'self-insert-command)
;;               (setq from (1+ from))))))
;;   (funcall make-vec-f #o040 #o0177)
;;   (funcall make-vec-f #o0240 #o0400)
;;   )

(map! :map global-map
      "M-c" #'ignore
      [menu-bar] nil
      "RET" #'ignore
      "<xterm-paste>" #'xterm-paste
      "DEL" #'ignore
      )

(map! :map global-map ;; Fn Disable
      "<f1>" #'ignore
      "<f2>" #'ignore
      "<f3>" #'ignore
      "<f4>" #'ignore
      "<f5>" #'ignore
      "<f6>" #'ignore
      "<f9>" #'ignore
      "<f10>" #'ignore
      "<f11>" #'ignore
      "<f12>" #'ignore
      )

(map! :map global-map ;; Mouse Disable
      "<pinch>" nil
      "<touch-end>" nil
      "<mouse-movement>"   nil
      "C-<down-mouse-1>"   nil
      "C-<down-mouse-2>"   nil
      "C-<mouse-1>"        nil
      "C-<mouse-2>"        nil
      "C-<mouse-3>"        nil
      "C-<mouse-4>"        nil
      "C-<mouse-5>"        nil
      "C-<wheel-down>"     nil
      "C-<wheel-up>"       nil

      "C-M-<down-mouse-1>" nil
      "C-M-<down-mouse-2>" nil
      "C-M-<mouse-1>"      nil
      "C-M-<mouse-2>"      nil
      "C-M-<mouse-3>"      nil
      "C-M-<mouse-4>"      nil
      "C-M-<mouse-5>"      nil
      )

(map! :map read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element
  )

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

(map! :map special-mode-map
      :n "q" #'quit-window
      )

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

;;-- evil overrides/intercept
(evil-make-overriding-map messages-buffer-mode-map)
(evil-make-intercept-map read-expression-map)

;;-- end evil overrides/intercept

(setq esc-map (make-keymap)
      lisp-mode-shared-map (make-sparse-keymap)
      )
;; (use-global-map global-map)

(keymap-global-set "C-c u" #'universal-argument)
(provide 'jg-global-bindings)
