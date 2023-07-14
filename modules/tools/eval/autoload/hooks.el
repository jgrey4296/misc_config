;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eval-quickrun-shrink-window-h ()
      "Shrink the quickrun output window once code evaluation is complete."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window (get-buffer-window quickrun--buffer-name)
          (let ((ignore-window-parameters t))
            (shrink-window-if-larger-than-buffer)))))

;;;###autoload
(defun +eval-quickrun-scroll-to-bof-h ()
      "Ensures window is scrolled to BOF on invocation."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window win
          (goto-char (point-min)))))
