;; trie funcs.el
;; loaded third.

;;Utilities
(defun acab-ide/trie-ide-running-p ()
  " Tests whether the ide is running or not "
  acab-ide/trie-ide-is-running
  )
(defun acab-ide/no-op ()
  (interactive)
  )

;;Startup and Cleanup
(defun acab-ide/toggle-trie-ide ()
  (interactive)
  (if (acab-ide/trie-ide-running-p)
      (acab-ide/stop-trie-ide)
    (acab-ide/start-trie-ide))
  )
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

;;Window setup
(defun acab-ide/reset-windows ()
  (interactive)
  (if (and (acab-ide/trie-ide-running-p) (window-configuration-p acab-ide/window-configuration))
      (progn (setq acab-ide/window-configuration (acab-ide/build-ide-window-layout))
             (acab-ide/init-ide-buffers-contents location inst-name acab-ide/window-configuration)
             )
    )
  )
(cl-defun acab-ide/build-ide-window-layout ()
  """ Setup rule editing windows """
  ;; (terminals - ) priors - rule - posts (terminals)
  ;;                       defeaters
  ;;       upstream stats  - alts - downstream stats
  (interactive)
  (let (prior post rule miscL miscC miscR)
    (delete-other-windows)
    ;; split in half
    (setq prior (selected-window))
    (setq miscL (split-window-below))
    ;;Top half:
    ;; Split into three: priors, rule, posts
    (setq rule (split-window-right))
    (select-window rule)
    (setq post (split-window-right))
    ;;Bottom Half
    ;; Split into three: upstream, alts, downstream
    (select-window miscL)
    (setq miscC (select-window (split-window-right)))
    (setq miscR (split-window-right))

    (list :prior prior :post post :rule rule :miscL miscL :miscC miscC :miscR miscR)
    )
  )
(defun acab-ide/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )

(defun acab-ide/redisplay-io-window (side content)
  ;;delete all content in buffer
  ;;insert content
  ;;TODO redisplay io window
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

;;Python subprocess
(defun acab-ide/run-python-server (loc)
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test"
  (message "Initializing Python Server")
  ;;start python process
  (setq trie/python-process (make-process :name "Rule IDE Process"
                                          :buffer acab-ide/python-process-buffer-name
                                          :command (list acab-ide/process-python-command
                                                         acab-ide/process-python-args)
                                          :connection-type 'pipe
                                          :filter 'acab-ide/python-filter
                                          :sentinel 'acab-ide/python-sentinel
                                          )
        )
  ;;initialize
  (process-send-string trie/python-process (format "load {}\n" loc))
  ;;populate emacs side data with loaded+parsed info

  )
(defun acab-ide/python-filter (proc x)
  ;; TODO Filter to parse and handle python responses


  )
(defun acab-ide/python-sentinel (proc x)
  ;; TODO Sentinel to handle python state changes

  )

;;Testing
(defun acab-ide/trigger-tests ()
  " TODO Trigger a Bank of tests "
  (interactive)
  ;;with buffer rule logs
  ;;clear?
  ;;get tests for working group
  ;;run tests
  ;;print results


  )


;;Python process functions

(defun acab-ide/python-server-load ()
  ;; TODO python server load
  )
(defun acab-ide/python-server-query ()
  ;; TODO python server query
  )
(defun acab-ide/python-server-inspect ()
  ;; TODO python server inspect
  )
(defun acab-ide/python-server-test ()
  ;; TODO python server test
  )
(defun acab-ide/python-server-quit ()
  ;; TODO python server quit
  )
(defun acab-ide/python-server-replace ()
  ;; TODO python server replace
  )
(defun acab-ide/python-server-report ()
  ;; TODO Python server report
  )
