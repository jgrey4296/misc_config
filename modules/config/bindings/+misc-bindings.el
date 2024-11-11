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
      "<xterm-paste>" #'xterm-paste
      "M-c" #'ignore
      [menu-bar] nil
      "RET"                #'ignore
      "DEL"                #'ignore
      "C-w"                #'ignore
      "C-t"                #'ignore
      "C-c &"              #'ignore
      "C-x v"              #'ignore
      "C-c !"              #'ignore
      "C-x SPC"            #'ignore
      "C-c C-x"            #'ignore
      "C-x w"              #'ignore
      "C-c p"              #'ignore
      "C-M-i"              #'ignore
      "C-M-q"              #'ignore
      "C-M-x"              #'ignore
      "C-x C-a"            #'ignore
      "C-@"                #'ignore
      "C-a"                #'ignore
      "C-d"                #'ignore
      "C-e"                #'ignore
      "C-g"                #'ignore
      "C-k"                #'ignore
      "C-o"                #'ignore
      "C-l"                #'ignore
      "C-q"                #'ignore
      "C-s"                #'ignore
      "C-v"                #'ignore
      "C-y"                #'ignore
      "C-_"                #'ignore
      "C-SPC"              #'ignore
      "C-?"                #'ignore
      "C-M"                #'ignore
      "C-<backspace>"      #'ignore
      "C-<delete>"         #'ignore
      "C-<down>"           #'ignore
      "C-<end>"            #'ignore
      "C-<home>"           #'ignore
      "C-<left>"           #'ignore
      "C-<next>"           #'ignore
      "C-h"                #'ignore
      "C-x"                #'ignore
      "C-M"                #'ignore
      "ESC C-<backspace>"  nil
      "ESC C-<backspace>"  nil
      "ESC C-<delete>"     nil
      "ESC C-<down>"       nil
      "ESC C-<end>"        nil
      "ESC C-<home>"       nil
      "ESC C-<left>"       nil
      "ESC C-<right>"      nil
      "ESC C-<up>"         nil
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
