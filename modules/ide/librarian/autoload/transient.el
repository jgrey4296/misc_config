;; transient.el -*- lexical-binding: t; -*-
(require 'librarian)
(require 'librarian-browser)
(unless (boundp 'librarian-browser-variants) (defvar librarian-browser-variants nil))
(unless (boundp 'librarian-default-browser) (defvar librarian-default-browser "firefox"))

;; TODO librarian-browser-select to transient-toggles
;;TODO apply choice
(transient-define-argument browse-selector ()
  :class 'transient-switches
  :argument-format "-browser=%s"
  :argument-regexp "-browser=%s"
  :choices librarian-browser-variants
  )

(transient-make-mode-toggle! librarian-mode "Librarian" "!")
(progn
    (transient-make-call! librarian-browser-select "b" (format "Browser: %10s" librarian-default-browser)
                        :transient nil
                        (librarian-browser-select)
                        )
    (transient-make-call! librarian-rebuild-database "r" "Rebuild Database"
                          :transient nil
                          (librarian-tagging-mode-rebuild-tag-database)
                          )
  )

(transient-make-subgroup! librarian-settings "b"
                          "For controlling librarian"
                          :desc "|| Librarian  ||"
                          [[
                            (transient-macro-toggle-librarian-mode)
                            (transient-macro-call-librarian-browser-select)
                            ]
                           [
                            ""
                            (transient-macro-call-librarian-rebuild-database)

                            ]]
                          )


;;;###autoload
(defun +jg-librarian-add-librarian-transient ()
  (transient-append-suffix 'jg-toggle-main "w" librarian-settings)
  )
