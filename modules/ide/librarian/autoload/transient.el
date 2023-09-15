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

(transient-make-mode-toggle! librarian-mode "Librarian" "b")
(progn
    (transient-make-call! librarian-browser-select "b" (format "Browser: %10s" librarian-default-browser)
                        :transient nil
                        (librarian-browser-select)
                        )
  )

(transient-make-subgroup! librarian-settings "B"
                          "For controlling librarian"
                          :desc "|| Librarian ||"
                          [[
                            (transient-macro-call-librarian-browser-select)


                            ]]
                          )


;;;###autoload
(defun +jg-librarian-add-librarian-transient ()
  (transient-append-suffix 'jg-toggle-main "w" librarian-settings)
  )
