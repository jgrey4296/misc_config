;;; commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-dot-get-commands (&optional dir)
  (interactive)
  (-when-let* ((filename (buffer-file-name))
               (is-dot (f-ext? filename "dot"))
               )
    (+jg-eval--pair-cmds
     `("dot compile" ,(graphviz-compile-command filename))
     (when (f-exists? (graphviz-output-file-name filename))
       `("dot open" ,(format "eog %s" (graphviz-output-file-name filename)))
       )
     )
    )
  )

;;;###autoload
(defun +jg-dot-compile-and-view ()
  (interactive)
  (when (f-exists? (graphviz-output-file-name (buffer-file-name)))
    (+jg-dired-async-trash (graphviz-output-file-name (buffer-file-name))))
  (let* ((outf (graphviz-output-file-name (buffer-file-name)))
         (comp-buffer (compile (graphviz-compile-command (buffer-file-name))))
         (sentinel (-partial #'(lambda (outf proc status)
                                 (message "Outf: %s" outf)
                                 (when (and (not (process-live-p proc))
                                            (f-exists? outf))
                                   (browse-url outf)
                                   ))
                             outf))
         )
    (set-process-sentinel
     (get-buffer-process comp-buffer)
     sentinel)
    )
  )

;;;###autoload
(defun +jg-dot-set-ext ()
  (interactive)
  (setq graphviz-dot-preview-extension (ivy-read "Graphviz Output Format: "
                                                 '("gif" "png" "jpg" "svg")
                                                 :require-match t
                                                 ))
  )
