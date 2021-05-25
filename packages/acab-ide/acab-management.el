;; -*- mode: elisp; lexical-binding: t; -*-
;;
(provide 'acab-ide-management)

;;Registered databases for lookup and tracking
(defvar acab-ide/dialects '()
  "All defined sub-dsls")
(defvar acab-ide/current-priors '()
  "Available Priors for the current rule")
(defvar acab-ide/current-post '()
  "Available Posts for the current rule")


(defvar acab-ide/rules nil
  "Defined rules")
(defvar acab-ide/types nil
  "Loaded and Defined types")
(defvar acab-ide/crosscuts nil
  "Loaded and defined crosscuts")
(defvar acab-ide/patterns nil
  "Loaded and defined patterns")
(defvar acab-ide/tests nil
  "Defined tests")

(defvar acab-ide/sentences nil)
(defvar acab-ide/tags nil
  "Tag references to other objects")
(defvar acab-ide/channels nil
  "Specified channels between layers")


(defun acab-ide/init-data ()
  (setq acab-ide/rules (make-hash-table :test 'equal)
        acab-ide/types (make-hash-table :test 'equal)
        acab-ide/crosscuts (make-hash-table :test 'equal)
        acab-ide/patterns (make-hash-table :test 'equal)
        acab-ide/tests (make-hash-table :test 'equal)
        acab-ide/tags (make-hash-table :test 'equal)
        acab-ide/channels (make-hash-table :test 'equal)

        acab-ide/current-priors '()
        acab-ide/current-post '()
        )
  )

;; Retrieve


;; Insert


;;DELETION
(defun acab-ide/delete-rule ()
  (interactive)
  ;;TODO delete rule
  ;;remove from hashmap
  ;;delete file
  ;;remove from runtime
  )
(defun acab-ide/delete-type ()
  (interactive)
  ;;TODO delete type
  )
(defun acab-ide/delete-crosscut ()
  (interactive)
  ;;TODO delete crosscut
  )
(defun acab-ide/delete-sequence ()
  (interactive)
  ;;TODO delete sequence
  )

(defun acab-ide/show-analysis-results ()
  ;;TODO show analysis results
  )

;;UPDATE
(defun acab-ide/update-buffer-contents ()
  (interactive)
  ;;TODO update buffer contents
  )
(defun acab-ide/sort-conditions-and-actions ()
  ;;TODO sort conditions and actions
  )

;;REMOVAL
(defun acab-ide/remove-component ()
  ;;TODO remove component
  )

;;Analysis
(defun acab-ide/analyse-data ()
  ;;TODO analyse data
  )
