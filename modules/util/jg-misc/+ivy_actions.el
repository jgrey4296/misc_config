;;; util/jg-misc/+ivy_actions.el -*- lexical-binding: t; -*-

(defun +jg-misc-ivy-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate #'+jg-misc-ivy-predicate
            :action #'+jg-misc-ivy-open-as-popup
            :matcher #'ivy--switch-buffer-matcher
            :call 'jg-misc-ivy-popup-buffer
            )
  )

(defun +jg-misc-ivy-open-as-popup (buff)
  (let ((curr-rule (display-buffer-assq-regexp buff display-buffer-alist nil))
        (curr-window (selected-window))
        )
    ;; Add rule if necessary:
    (if (not curr-rule)
        (progn (message "Adding temp rule")
               (setq curr-rule (+popup-make-rule buff jg-misc-popup-persist-default))
               (push curr-rule display-buffer-alist))
      )
    (bury-buffer buff)
    (pop-to-buffer buff)
    (if (not (alist-get 'select (alist-get 'window-parameters curr-rule)))
        (select-window curr-window)
      )
    )
  )


(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-misc-ivy-open-as-popup "Popup")))
