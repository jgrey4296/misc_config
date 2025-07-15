;; transient.el -*- lexical-binding: t; -*-
(require 'librarian)
(require 'librarian--browse)
(require 'macro-tools--transient)

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
  "Rebuild Tag Database"
  :key "r"
  :transient nil
  (librarian-tag-mode-rebuild-tag-database)
  )
(transient-call! general-insert-rebuild-cache ()
  "Clears and rebuilds the general insert cache"
  :key "@"
  :desc (propertize "Clear General-Insert Cache" 'face 'transient-heading)
  (librarian-insert-clear-caches)
  (message "Cache Rebuilt")
  )

(transient-subgroup! librarian-settings ()
  "For controlling librarian"
  :key "b"
  :desc "|| Librarian  ||"
  :rows t
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
(defun +jg-librarian-build-librarian-transient ()
  (transient-guarded-insert-subgroup! 'jg-toggle-main librarian-settings (1 -1))
  (transient-append-suffix 'jg-toggle-main
    '(0 0 -1)
    '("@" transient-macro-call-general-insert-rebuild-cache)
    )
  )
