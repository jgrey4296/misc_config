(require 'subr-x)

;; from: http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs#1242366
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun face-under-cursor-customize (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (customize-face face) (message "No face at %d" pos))))

;;from emacswiki.org/emacs/DebugMessages:
(defadvice message (before who-said-that activate)
  "Find out who said that thing and say so"
  (let ((trace nil) (n 1) (frame nil))
    (while (setq frame (backtrace-frame n))
      (setq n (1+ n)
            trace (cons (cadr frame) trace)) )
    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 9)))
    (ad-set-args 1 (cons trace (ad-get-args 1))) ))

;;deactivate the above:
(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)



