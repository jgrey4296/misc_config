;; transient.el -*- lexical-binding: t; -*-
(require 'librarian)
(require 'librarian--browse)
(unless (boundp 'librarian--browse-variants) (defvar librarian--browse-variants nil))
(unless (boundp 'librarian--browse-default) (defvar librarian--browse-default "firefox"))

;; TODO librarian-browser-select to transient-toggles
;;TODO apply choice
(transient-define-argument browse-selector ()
  :class 'transient-switches
  :argument-format "-browser=%s"
  :argument-regexp "-browser=%s"
  :choices librarian--browse-variants
  )

(transient-toggle-mode! librarian-mode ()
  "Librarian Mode"
  :desc (propertize "Librarian" 'face 'transient-heading)
  :key "!"
  )
(transient-call! librarian-browser-select ()
  "Browser Select"
  :key "b"
  :desc (format "Browser: %10s" librarian--browse-default)
  :transient nil
  (librarian-browser-select)
  )
(transient-call! librarian-rebuild-database ()
  "Rebuild Database"
  :key "r"
  :transient nil
  (librarian-tag-mode-rebuild-tag-database)
  )

(transient-subgroup! librarian-settings ()
  "For controlling librarian"
  :key "b"
  :desc "|| Librarian  ||"
  ["Global"
   (transient-macro-toggle-librarian-mode)
   ]
  ["Settings"
   (transient-macro-call-librarian-browser-select)
   ]
  ["Triggers"
   (transient-macro-call-librarian-rebuild-database)
   ]
  )


;;;###autoload
(defun +jg-librarian-add-librarian-transient ()
  (transient-append-suffix 'jg-toggle-main '(1 1 0) librarian-settings)
  )
