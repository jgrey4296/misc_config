;;; util/jg-misc/+ivy_actions.el -*- lexical-binding: t; -*-

(defun +jg-misc-ivy-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :action #'+jg-misc-ivy-open-as-popup
            :matcher #'ivy--switch-buffer-matcher
            :call 'jg-misc-ivy-popup-buffer
            )
  )

(defun +jg-misc-ivy-open-as-popup (buff)
  (let ((display-buffer-alist +popup--display-buffer-alist))
    (if (not (display-buffer-assq-regexp buff display-buffer-alist nil))
        (push (+popup-make-rule "." jg-misc-popup-persist-default)
              display-buffer-alist))
    (bury-buffer buff)
    (pop-to-buffer buff))
  )


(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-misc-ivy-open-as-popup "Popup")))
