;;; util/window-control/+ivy_actions.el -*- lexical-binding: t; -*-

(defun +window-control-ivy-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate #'+window-control-ivy-predicate
            :action #'+window-control-ivy-open-as-popup
            :matcher #'ivy--switch-buffer-matcher
            :caller 'window-control-ivy-popup-buffer
            )
  )

(defun +window-control-ivy-open-as-popup (buff)
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

(defun +window-control-ivy-reset-popup-rules ()
  (interactive)
  (+window-control-setup-popup-rules-hook)
  )


(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +window-control-ivy-open-as-popup "Popup")))

(ivy-set-actions '+window-control-ivy-popup-buffer
                 '(("p" +window-control-ivy-reset-popup-rules "Clear Popup")))
