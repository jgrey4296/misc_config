;; workspace.el -*- lexical-binding: t; -*-
(require 'macro-tools--related)
(require 'carousel-minor-mode)

;;;###autoload (autoload 'related-files:jg-python-project "lang-weakly-typed/python/autoload/workspace.el" nil t)
(make-related! jg-python-project
  :files ((:impl (f-join root fparent2 (s-replace "test_" "" fname)) :when is-test)
          (:test (f-join fparent "__tests" (concat "test_" fname))   :when (not is-test))

          (:initpy (f-join fparent "__init__.py"))
          (:doot "doot.toml")
          (:errors (f-join src "errors")    :when (f-exists? (f-join root src "errors")))
          (:errors (f-join src "errors.py") :when (f-exists? (f-join root src "errors.py")))
          ;; TODO (:docs)
          (:log log-file                    :when (f-exists? log-file))
          )
  :tests ((is-test      (s-matches? "^test_" fname))
          (is-doot-toml (s-matches? "doot.toml" fname))
          )
  :binds ((src (f-filename root))
          (log-file (f-join root (concat "log." fname)))
          )
  )

;;;###autoload
(defun +jg-python-carousel-window-fn (buff)
  (unless (and (persp-parameter 'carousel)
               (not (ring-empty-p (persp-parameter 'carousel-actual))))
    (select-window (split-window-below)))
  (find-file buff)
  )

;;;###autoload
(defun +jg-python-popup-related-test ()
  (interactive)
  (-when-let* ((testfile (projectile--find-related-file (buffer-file-name) :test))
               (exists-p (f-exists? testfile))
               (buff (find-file-noselect testfile nil nil))
               )
    (+popup-buffer buff '((:side . right) (:select true)))
    )
  )
