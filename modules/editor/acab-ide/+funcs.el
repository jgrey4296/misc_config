;; trie funcs.el
;; loaded third.

;;Utilities
(defun jg-trie-layer/trie-ide-running-p ()
  " Tests whether the ide is running or not "
  jg-trie-layer/trie-ide-is-running
  )
(defun jg-trie-layer/no-op ()
  (interactive)
  )

;;Startup and Cleanup
(defun jg-trie-layer/toggle-trie-ide ()
  (interactive)
  (if (jg-trie-layer/trie-ide-running-p)
      (jg-trie-layer/stop-trie-ide)
    (jg-trie-layer/start-trie-ide))
  )
(defun jg-trie-layer/start-trie-ide ()
  " Start the trie ide, setting up windows, etc "
  (interactive)
  ;; Get the directory to work with
  (let ((location (read-file-name "Institution Location:"))
        (windows (jg-trie-layer/build-ide-window-layout))
        inst-name
        )

    ;;if a dir chosen, get a name for the
    ;;inst, create a stub, create a data dir
    (if (f-dir? location)
        (progn
          ;;get name for inst
          (setq inst-name (read-string "Institution Name: ")
                jg-trie-layer/ide-data-loc (f-join location (format "%s-data" inst-name)))
          )
      ;;else if an org chosen, load it and its data dir
      (progn
        (assert (equal (f-ext location) "org"))
        (setq inst-name (f-base location)
              location (f-parent location)
              jg-trie-layer/ide-data-loc (f-join location (format "%s-data" inst-name))
              )
        ))

    (setq jg-trie-layer/ide-pipeline-spec-buffer (format "%s.org" inst-name))
    (jg-trie-layer/maybe-build-data-loc)
    (jg-trie-layer/init-ide-buffers-contents location inst-name windows)
    ;;Save the window configuration
    (setq jg-trie-layer/window-configuration windows)

    ;;start python server
    (jg-trie-layer/run-python-server location)
    (jg-trie-layer/load-directory-and-pipeline jg-trie-layer/ide-data-loc)

    )

  (setq jg-trie-layer/trie-ide-is-running t)
  )
(defun jg-trie-layer/stop-trie-ide ()
  (interactive)
  ;;Clear windows, unload data
  (message "Shutting down Trie IDE")
  (jg-trie-layer/dump-to-files)

  (if (and jg-trie-layer/python-process
           (processp jg-trie-layer/python-process)
           (process-live-p jg-trie-layer/python-process))
      (progn
        (message "Closing Python Server")
        (quit-process jg-trie-layer/python-process)
        (kill-buffer jg-trie-layer/python-process-buffer-name)
        (setq jg-trie-layer/python-process nil)
        )
    )
  (setq jg-trie-layer/trie-ide-is-running nil)
  (assert (not (jg-trie-layer/trie-ide-running-p)))
  )

;;Directory and buffer initialisation
(defun jg-trie-layer/maybe-build-data-loc ( )
  (if (not (f-exists? jg-trie-layer/ide-data-loc))
      (progn (mkdir jg-trie-layer/ide-data-loc)
             (mapc (lambda (x) (mkdir (f-join jg-trie-layer/ide-data-loc x)))
                   jg-trie-layer/data-loc-subdirs)
             )
    )
  )
(defun jg-trie-layer/init-ide-buffers-contents (location inst-name windows)
  ;;create inst stub
  (if (not (f-exists? (f-join location jg-trie-layer/ide-pipeline-spec-buffer)))
      (progn
        (with-current-buffer (get-buffer-create jg-trie-layer/ide-pipeline-spec-buffer)
          ;; insert default institution contents
          (trie-mode)
          (org-mode)
          (yas-expand-snippet (yas-lookup-snippet "pipeline" 'trie-mode))
          (write-file (f-join location jg-trie-layer/ide-pipeline-spec-buffer))
          )
        )
    )


  (window--display-buffer (find-file (f-join location jg-trie-layer/ide-pipeline-spec-buffer)) (plist-get windows :miscL) 'window)
  (window--display-buffer (generate-new-buffer "rule_stub")  (plist-get windows :rule) 'window)
  (window--display-buffer (get-buffer-create jg-trie-layer/inputs-buffer-name)  (plist-get windows :prior)'window)
  (window--display-buffer (get-buffer-create jg-trie-layer/outputs-buffer-name)  (plist-get windows :post) 'window)
  (window--display-buffer (get-buffer-create jg-trie-layer/logging-buffer-name)  (plist-get windows :miscR) 'window)
  (window--display-buffer (get-buffer-create jg-trie-layer/working-group-buffer-name)  (plist-get windows :miscC) 'window)

  (jg-trie-layer/build-working-group-buffer)
  (with-current-buffer "rule_stub"
    (trie-mode)
    (yas-expand-snippet (yas-lookup-snippet "rule" 'trie-mode) (point-min))
    )

  (with-current-buffer jg-trie-layer/inputs-buffer-name
    (insert "AVAILABLE INPUTS Layer 0:\n--------------------\n\n")
    )
  (with-current-buffer jg-trie-layer/outputs-buffer-name
    (insert "AVAILABLE OUTPUTS Layer 1:\n--------------------\n\n")
    )
  (with-current-buffer jg-trie-layer/logging-buffer-name
    (insert "LOGGING:\n--------------------\n\n")
    )
  )
(defun jg-trie-layer/build-working-group-buffer ()
  (with-current-buffer jg-trie-layer/working-group-buffer-name
    (org-mode)
    (insert "* Working Group\n")
    (mapc (lambda (x) (insert "** " x ":\n")) jg-trie-layer/working-group-buffer-headings)
    )
  )

;;Window setup
(defun jg-trie-layer/reset-windows ()
  (interactive)
  (if (and (jg-trie-layer/trie-ide-running-p) (window-configuration-p jg-trie-layer/window-configuration))
      (progn (setq jg-trie-layer/window-configuration (jg-trie-layer/build-ide-window-layout))
             (jg-trie-layer/init-ide-buffers-contents location inst-name jg-trie-layer/window-configuration)
             )
    )
  )
(cl-defun jg-trie-layer/build-ide-window-layout ()
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
(defun jg-trie-layer/show-side-window (buffer &optional left)
  (interactive)
  ;; For Terminals:
  (display-buffer-in-side-window buffer `((side . ,(if left 'left 'right))))
  )

(defun jg-trie-layer/redisplay-io-window (side content)
  ;;delete all content in buffer
  ;;insert content
  ;;TODO redisplay io window
  )

;;Loading and saving files
(defun jg-trie-layer/load-directory-and-pipeline (loc)
  " Given a location, load into ide "
  ;;Initialise data
  (trie/init-data)
  ;; TODO load directory and setup pipeline
  ;;command python


  )
(defun jg-trie-layer/dump-to-files ()
  (interactive)
  ;;Get all trie-* mode buffers, and the pipeline spec
  ;;and write to subdirs of jg-trie-layer/ide-data-loc
  (let ((buffers (buffer-list))
        (curr-buff (current-buffer))
        (special-buffers (list jg-trie-layer/inputs-buffer-name
                               jg-trie-layer/outputs-buffer-name
                               jg-trie-layer/working-group-buffer-name
                               jg-trie-layer/logging-buffer-name))
        )
    (mapc (lambda (x)
            (cond ((and (buffer-file-name x) (f-ancestor-of? jg-trie-layer/ide-data-loc (buffer-file-name x)))
                   (progn (save-buffer)
                          (if (not (equal curr-buff x))
                              (kill-buffer x))))
                  ((-contains? special-buffers (buffer-name x))
                   (kill-buffer x))))
          buffers)
    )
  )

;;Python subprocess
(defun jg-trie-layer/run-python-server (loc)
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test"
  (message "Initializing Python Server")
  ;;start python process
  (setq trie/python-process (make-process :name "Rule IDE Process"
                                          :buffer jg-trie-layer/python-process-buffer-name
                                          :command (list jg-trie-layer/process-python-command
                                                         jg-trie-layer/process-python-args)
                                          :connection-type 'pipe
                                          :filter 'jg-trie-layer/python-filter
                                          :sentinel 'jg-trie-layer/python-sentinel
                                          )
        )
  ;;initialize
  (process-send-string trie/python-process (format "load {}\n" loc))
  ;;populate emacs side data with loaded+parsed info

  )
(defun jg-trie-layer/python-filter (proc x)
  ;; TODO Filter to parse and handle python responses


  )
(defun jg-trie-layer/python-sentinel (proc x)
  ;; TODO Sentinel to handle python state changes

  )

;;Testing
(defun jg-trie-layer/trigger-tests ()
  " TODO Trigger a Bank of tests "
  (interactive)
  ;;with buffer rule logs
  ;;clear?
  ;;get tests for working group
  ;;run tests
  ;;print results


  )


;;Python process functions

(defun jg-trie-layer/python-server-load ()
  ;; TODO python server load
)
(defun jg-trie-layer/python-server-query ()
  ;; TODO python server query
)
(defun jg-trie-layer/python-server-inspect ()
  ;; TODO python server inspect
)
(defun jg-trie-layer/python-server-test ()
  ;; TODO python server test
)
(defun jg-trie-layer/python-server-quit ()
  ;; TODO python server quit
)
(defun jg-trie-layer/python-server-replace ()
  ;; TODO python server replace
)
(defun jg-trie-layer/python-server-report ()
  ;; TODO Python server report
)
