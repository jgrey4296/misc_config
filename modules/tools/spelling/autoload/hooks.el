;;; hooks.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +spell-remove-run-together-switch-for-aspell-h ()
  (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))
  )

;;;###autoload
(defun +spell-init-excluded-faces-h ()
               "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
               (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
                 (setq-local spell-fu-faces-exclude excluded))
               )

;;;###autoload
(defun +spell-inhibit-duplicate-detection-maybe-h ()
               "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
               (and (or (and (bound-and-true-p flycheck-mode)
                             (executable-find "proselint"))
                        (featurep 'langtool))
                    (setq-local flyspell-mark-duplications-flag nil))
               )
