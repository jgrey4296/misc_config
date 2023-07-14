;;; lang/ocaml/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ocaml/comment-indent-new-line (&optional _)
  "Break line at point and indent, continuing comment if within one."
  (interactive)
  (comment-indent-new-line)
  (when (eq (char-before) ?*)
    (just-one-space))
  (unless (eq (char-after) 32)
    (save-excursion (insert " "))))

;;;###autoload
(defun +ocaml-init-ocamlformat-h ()
  ;; TODO Fix region-based formatting support
    (setq +format-with 'ocp-indent)
    (when (and (executable-find "ocamlformat")
               (locate-dominating-file default-directory ".ocamlformat"))
      (when buffer-file-name
        (let ((ext (file-name-extension buffer-file-name t)))
          (cond ((equal ext ".eliom")
                 (setq-local ocamlformat-file-kind 'implementation))
                ((equal ext ".eliomi")
                 (setq-local ocamlformat-file-kind 'interface)))))
      (setq +format-with 'ocamlformat)))

;;;###autoload
(defun +ocaml-init-ocp-indent-h ()
    "Run `ocp-setup-indent', so long as the ocp-indent binary exists."
    (when (executable-find "ocp-indent")
      (ocp-setup-indent)))

;;;###autoload
(defun +ocaml-init-utop-h ()
      (when (executable-find "utop")
        (utop-minor-mode))
      )

;;;###autoload
(defun +ocaml-init-merlin-h ()
    "Activate `merlin-mode' if the ocamlmerlin executable exists."
    (when (executable-find "ocamlmerlin")
      (merlin-mode)))

;;;###autoload
(defun +ocaml-init-flycheck-h ()
    "Activate `flycheck-ocaml`"
    ;; Disable Merlin's own error checking
    (setq merlin-error-after-save nil)
    ;; Enable Flycheck checker
    (flycheck-ocaml-setup))
