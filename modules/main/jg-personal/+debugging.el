;;; main/personal/+debugging.el -*- lexical-binding: t; -*-

;; Debugging
(defadvice message (before who-said-that activate)
  "Find out who said that thing and say so"
  ;;from emacswiki.org/emacs/DebugMessages:
  (let ((trace nil) (n 1) (frame nil))
    (while (setq frame (backtrace-frame n))
      (setq n (1+ n)
            trace (cons (cadr frame) trace)) )
    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 9)))
    (ad-set-args 1 (cons trace (ad-get-args 1))) ))
;;deactivate the above:
(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)
