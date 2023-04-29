;;; +vars.el -*- lexical-binding: t; -*-

(defvar +eval-popup-min-lines 4
  "The output height threshold (inclusive) before output is displayed in a popup
buffer rather than an overlay on the line at point or the minibuffer.")

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(spec-handling-add! popup nil
                    '(eval
                      ((lambda (bufname _) (when (boundp '+eval-repl-mode) (buffer-local-value '+eval-repl-mode (get-buffer bufname))))
                       :ttl (lambda (buf) (unless (plist-get +eval-repl-plist :persist)
                                            (when-let (process (get-buffer-process buf))
                                              (set-process-query-on-exit-flag process nil)
                                              (kill-process process)
                                              (kill-buffer buf))))
                       :size 0.25 :quit nil)
                      )
                    '(quickrun
                      ("^\\*quickrun" :size 0.3 :ttl 0)
                      )
                    )
