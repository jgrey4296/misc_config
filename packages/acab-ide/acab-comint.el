;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; [[file:/Volumes/documents/github/emacs_files/lisp/comint-test.el][comint]]


(provide 'acab-comint)

;; TODO For working with acab-py, through a comint

(defvar acab-comint/acab-py-loc nil)
(defvar acab-comint/python-cmd nil)
(defvar acab-comint/python-args nil)
(defvar acab-comint/python-process nil)
(defvar acab-comint/cwd nil)
(defvar acab-comint/buffer-name nil)
(defvar acab-comint/prompt-regexp "^\\(:\\)")

(defun acab-comint/init ()
  ;; Get/create comint buffer then:
  ;; (unless buffer
  ;;   (apply 'make-comint-in-buffer my-comint-buffer-name buffer prog my-comint-args)
  ;;   (my-comint-mode))))
  )
(defun acab-comint/shut-down ()

  )

(defun acab-comint/send-input ()

  )
(defun acab-comint/get-output ()

  )

(defun acab-comint/python-server-load ()
  ;; TODO python server load
  )
(defun acab-comint/python-server-query ()
  ;; TODO python server query
  )
(defun acab-comint/python-server-inspect ()
  ;; TODO python server inspect
  )
(defun acab-comint/python-server-test ()
  ;; TODO python server test
  )
(defun acab-comint/python-server-quit ()
  ;; TODO python server quit
  )
(defun acab-comint/python-server-replace ()
  ;; TODO python server replace
  )
(defun acab-comint/python-server-report ()
  ;; TODO Python server report
  )

;;Python subprocess
(defun acab-comint/run-python-server (loc)
  "Start a subprocess of python, loading the rule engine
ready to set the pipeline and rulesets, and to test"
  (message "Initializing Python Server")
  ;;start python process
  (setq trie/python-process (make-process :name "Rule IDE Process"
                                          :buffer acab-comint/python-process-buffer-name
                                          :command (list acab-comint/process-python-command
                                                         acab-comint/process-python-args)
                                          :connection-type 'pipe
                                          :filter 'acab-comint/python-filter
                                          :sentinel 'acab-comint/python-sentinel
                                          )
        )
  ;;initialize
  (process-send-string trie/python-process (format "load {}\n" loc))
  ;;populate emacs side data with loaded+parsed info

  )
(defun acab-comint/python-filter (proc x)
  ;; TODO Filter to parse and handle python responses


  )
(defun acab-comint/python-sentinel (proc x)
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

(define-derived-mode acab-comint-mode comint-mode "Acab-Comint"
  "Major Mode for Comint Interaction with Acab-Py"
  nil "Acab-Comint"
  ;;Setup:
  (setq comint-prompt-regexp acab-comint/prompt-regexp)

  ;; Set up transforms:
  ;; (setq-local comint-input-filter-functions '(nil))
  ;; (setq-local comint-input-sender nil)
  ;; (add-hook 'comint-preoutput-filter-functions nil nil t)
  )

(defun acab-comint/init-hook ()
  "Helper function to initialize My-Comint"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  )

;; this has to be done in a hook. grumble grumble.
(add-hook 'acab-comint-mode-hook 'acab-comint/init-hook)
