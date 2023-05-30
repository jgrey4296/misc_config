;;; -*- lexical-binding: t; -*-

;;;###autoload (autoload 'evil-conflict-merge-state "emacs/version-control/autoload/conflict-merge-state" nil t)
 (evil-define-state conflict-merge
    "Merge Conflict State."
    :tag "<MC>"
    :message "-- MERGE CONFLICT --"
    ;; :enable (motion)
    ;; :input-method t
    :suppress-keymap t
    )

(evil-make-intercept-map evil-conflict-merge-state-map)

(provide 'conflict-merge-state)
