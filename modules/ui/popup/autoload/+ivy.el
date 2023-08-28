;;; +ivy.el -*- lexical-binding: t; -*-

(defun +jg-popup-ivy-predicate (x)
  ;; return nil for cruft buffers
  (not (string-match jg-popup-ivy-predicate-patterns (car x)))
  )

;;;###autoload
(defun +jg-popup-ivy-open (buff)
  (interactive)
  (let ((curr-rule (display-buffer-assq-regexp buff display-buffer-alist nil))
        (curr-window (selected-window))
        )
    ;; Add rule if necessary:
    (if (not curr-rule)
        (progn (message "Adding temp rule")
               (setq curr-rule (cons buff window-control-popup-persist-default-alist))
               (push curr-rule display-buffer-alist))
      )
    (bury-buffer buff)
    (pop-to-buffer buff)
    (if (not (alist-get 'select (alist-get 'window-parameters curr-rule)))
        (select-window curr-window)
      )
    )
  )

;;;###autoload
(defun +jg-popup-ivy-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate       #'+jg-popup-ivy-predicate
            :action          #'+jg-popup-ivy-open
            :matcher         #'ivy--switch-buffer-matcher
            :sort t
            :caller 'window-control-ivy-popup-buffer
            )
  )
