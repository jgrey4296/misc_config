;;; util/window-control/+ivy_actions.el -*- lexical-binding: t; -*-

(defun +jg-ui-ivy-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate       #'+jg-ui-ivy-predicate
            :action          #'+jg-ui-ivy-open-as-popup
            :matcher         #'ivy--switch-buffer-matcher
            :caller 'window-control-ivy-popup-buffer
            )
  )

(defun +jg-ui-ivy-open-as-popup (buff)
  (interactive)
  (let ((curr-rule (display-buffer-assq-regexp buff display-buffer-alist nil))
        (curr-window (selected-window))
        )
    ;; Add rule if necessary:
    (if (not curr-rule)
        (progn (message "Adding temp rule")
               (setq curr-rule (+popup-make-rule buff window-control-popup-persist-default))
               (push curr-rule display-buffer-alist))
      )
    (bury-buffer buff)
    (pop-to-buffer buff)
    (if (not (alist-get 'select (alist-get 'window-parameters curr-rule)))
        (select-window curr-window)
      )
    )
  )

(defun +jg-ui-ivy-reset-popup-rules ()
  (interactive)
  (+jg-ui-setup-popup-rules-hook)
  )


(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-ui-ivy-open-as-popup "Popup")))

(ivy-set-actions '+jg-ui-ivy-popup-buffer
                 '(("p" +jg-ui-ivy-reset-popup-rules "Clear Popup")))
