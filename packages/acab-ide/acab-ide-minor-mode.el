;; -*- mode: elisp; lexical-binding: t; -*-

;;based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'acab-comint)
(require 'acab-face)
(require 'acab-hydras)
(require 'acab-log-mode)
(require 'acab-inst-mode)
(require 'acab-rule-mode)
(require 'acab-sequence-mode)

(require 'trie-explore-mode)
(require 'trie-management)
(require 'trie-minor-mode)
(require 'trie-mode)
(require 'trie-passive-mode)
(require 'trie-tree)
(require 'trie-company)

(defgroup acab-ide '() "Acab Mode Customizations")
;;--------------------
;; Mode Variables
;;--------------------
(defcustom acab-ide-minor-mode-hook nil "Basic Hook For Acab Mode" :type '(hook))
;;--------------------
;;Utilities
;;--------------------

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar acab-ide-minor-mode-map
  (make-sparse-keymap)
  "Keymap for Acab ide mode mode")

;;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.acab\\'" . acab-ide-minor-mode))

;; --------------------
;;Entry Function
;;--------------------
(define-minor-mode acab-ide-minor-mode
  "Major Mode for creating rules using Acab"
  nil
  :lighter "Acab-IDE"
  :keymap acab-ide-minor-mode-map
  :global t
  (message "Enabling Acab IDE")
  ;; TODO enable acab-window-manager
  ;; setup default windows
  ;; start up acab-comint



  )


(provide 'acab-ide-minor-mode)
