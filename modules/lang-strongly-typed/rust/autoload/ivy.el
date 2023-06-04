;;; +ivy.el -*- lexical-binding: t; -*-
(require 'ivy)

;; TODO add action for inserting new entries into dependency list

(defvar jg-rust-dependency-loc (doom-module-expand-path :lang-strongly-typed 'rust  "_data/dependencies.ivy"))
(defvar jg-rust-dependency-collection nil)


;;;###autoload
(defun +jg-rust-dependency-ivy ()
  (interactive)
  ;; Read dependencies.ivy
  (unless jg-rust-dependency-collection
    (setq jg-rust-dependency-collection
          (with-temp-buffer (insert-file jg-rust-dependency-loc)
                            (split-string (buffer-string) "\n" t))))

  ;; provide ivy for them
  (insert (ivy-read "Add Dependency: " jg-rust-dependency-collection))
  )
