;; -*- mode: elisp; lexical-binding: t; -*-

;;based On https://www.emacswiki.org/emacs/ModeTutorial
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
(require 'trie-data)
(require 'trie-company)

(provide 'acab-ide-minor-mode)

(defgroup acab-ide '() "Acab Mode Customizations")
;;--------------------
;; Mode Variables
;;--------------------
(defcustom acab-ide-minor-mode-hook nil "Basic Hook For Acab Mode" :type '(hook))
(defvar acab-ide/ide-data-loc nil)

;;--------------------
;;Utilities
;;--------------------
;;Utilities
(defun acab-ide/trie-ide-running-p ()
  " Tests whether the ide is running or not "
  acab-ide/trie-ide-is-running
  )
(defun acab-ide/no-op ()
  (interactive)
  )

;;Startup and Cleanup
(defun acab-ide/start-trie-ide ()
  " Start the trie ide, setting up windows, etc "
  (interactive)
  ;; Get the directory to work with
  (let ((location (read-file-name "Institution Location:"))
        (windows (acab-ide/build-ide-window-layout))
        inst-name
        )

    ;;if a dir chosen, get a name for the
    ;;inst, create a stub, create a data dir
    (if (f-dir? location)
        (progn
          ;;get name for inst
          (setq inst-name (read-string "Institution Name: ")
                acab-ide/ide-data-loc (f-join location (format "%s-data" inst-name)))
          )
      ;;else if an org chosen, load it and its data dir
      (progn
        (assert (equal (f-ext location) "org"))
        (setq inst-name (f-base location)
              location (f-parent location)
              acab-ide/ide-data-loc (f-join location (format "%s-data" inst-name))
              )
        ))

    (setq acab-ide/ide-pipeline-spec-buffer (format "%s.org" inst-name))
    (acab-ide/maybe-build-data-loc)
    (acab-ide/init-ide-buffers-contents location inst-name windows)
    ;;Save the window configuration
    (setq acab-ide/window-configuration windows)

    ;;start python server
    (acab-ide/run-python-server location)
    (acab-ide/load-directory-and-pipeline acab-ide/ide-data-loc)

    )

  (setq acab-ide/trie-ide-is-running t)
  )
(defun acab-ide/stop-trie-ide ()
  (interactive)
  ;;Clear windows, unload data
  (message "Shutting down Trie IDE")
  (acab-ide/dump-to-files)

  (if (and acab-ide/python-process
           (processp acab-ide/python-process)
           (process-live-p acab-ide/python-process))
      (progn
        (message "Closing Python Server")
        (quit-process acab-ide/python-process)
        (kill-buffer acab-ide/python-process-buffer-name)
        (setq acab-ide/python-process nil)
        )
    )
  (setq acab-ide/trie-ide-is-running nil)
  (assert (not (acab-ide/trie-ide-running-p)))
  )

;;Directory and buffer initialisation
(defun acab-ide/maybe-build-data-loc ( )
  (if (not (f-exists? acab-ide/ide-data-loc))
      (progn (mkdir acab-ide/ide-data-loc)
             (mapc (lambda (x) (mkdir (f-join acab-ide/ide-data-loc x)))
                   acab-ide/data-loc-subdirs)
             )
    )
  )
(defun acab-ide/init-ide-buffers-contents (location inst-name windows)
  ;;create inst stub
  (if (not (f-exists? (f-join location acab-ide/ide-pipeline-spec-buffer)))
      (progn
        (with-current-buffer (get-buffer-create acab-ide/ide-pipeline-spec-buffer)
          ;; insert default institution contents
          (trie-mode)
          (org-mode)
          (yas-expand-snippet (yas-lookup-snippet "pipeline" 'trie-mode))
          (write-file (f-join location acab-ide/ide-pipeline-spec-buffer))
          )
        )
    )


  (window--display-buffer (find-file (f-join location acab-ide/ide-pipeline-spec-buffer)) (plist-get windows :miscL) 'window)
  (window--display-buffer (generate-new-buffer "rule_stub")  (plist-get windows :rule) 'window)
  (window--display-buffer (get-buffer-create acab-ide/inputs-buffer-name)  (plist-get windows :prior)'window)
  (window--display-buffer (get-buffer-create acab-ide/outputs-buffer-name)  (plist-get windows :post) 'window)
  (window--display-buffer (get-buffer-create acab-ide/logging-buffer-name)  (plist-get windows :miscR) 'window)
  (window--display-buffer (get-buffer-create acab-ide/working-group-buffer-name)  (plist-get windows :miscC) 'window)

  (acab-ide/build-working-group-buffer)
  (with-current-buffer "rule_stub"
    (trie-mode)
    (yas-expand-snippet (yas-lookup-snippet "rule" 'trie-mode) (point-min))
    )

  (with-current-buffer acab-ide/inputs-buffer-name
    (insert "AVAILABLE INPUTS Layer 0:\n--------------------\n\n")
    )
  (with-current-buffer acab-ide/outputs-buffer-name
    (insert "AVAILABLE OUTPUTS Layer 1:\n--------------------\n\n")
    )
  (with-current-buffer acab-ide/logging-buffer-name
    (insert "LOGGING:\n--------------------\n\n")
    )
  )
(defun acab-ide/build-working-group-buffer ()
  (with-current-buffer acab-ide/working-group-buffer-name
    (org-mode)
    (insert "* Working Group\n")
    (mapc (lambda (x) (insert "** " x ":\n")) acab-ide/working-group-buffer-headings)
    )
  )

;;Loading and saving files
(defun acab-ide/load-directory-and-pipeline (loc)
  " Given a location, load into ide "
  ;;Initialise data
  (trie/init-data)
  ;; TODO load directory and setup pipeline
  ;;command python


  )
(defun acab-ide/dump-to-files ()
  (interactive)
  ;;Get all trie-* mode buffers, and the pipeline spec
  ;;and write to subdirs of acab-ide/ide-data-loc
  (let ((buffers (buffer-list))
        (curr-buff (current-buffer))
        (special-buffers (list acab-ide/inputs-buffer-name
                               acab-ide/outputs-buffer-name
                               acab-ide/working-group-buffer-name
                               acab-ide/logging-buffer-name))
        )
    (mapc (lambda (x)
            (cond ((and (buffer-file-name x) (f-ancestor-of? acab-ide/ide-data-loc (buffer-file-name x)))
                   (progn (save-buffer)
                          (if (not (equal curr-buff x))
                              (kill-buffer x))))
                  ((-contains? special-buffers (buffer-name x))
                   (kill-buffer x))))
          buffers)
    )
  )

;;CLEANUP
(defun acab-ide/cleanup ()
  ;;TODO cleanup
  )
(defun acab-ide/write-rules-to-files ()
  ;;TODO write rules to files
  )


;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar acab-ide-minor-mode-map
  (make-sparse-keymap)
  "Keymap for Acab ide mode")

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
  ;; TODO if vars are missing, ask for them

  ;; Init management variables
  ;;
  ;; start acab-window-manager
  (acab-wm/init)
  ;; start up acab-comint
  (acab-comint/init)
  ;; Setup trie-company
  ;; Setup acab-company


  )


;;;--------------------
;;Autoloading
;;--------------------
(add-to-list 'auto-mode-alist '("\\.acab\\'" . acab-ide-minor-mode))
