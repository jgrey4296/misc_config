;My own custom functions:
(require 'subr-x)

;; from: http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs#1242366
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun face-under-cursor-customize (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (customize-face face) (message "No face at %d" pos))))

;;Easily switch to scratch:
(defun scratch()
  (interactive)
  (switch-to-buffer "*scratch*")
  )

(defun flycheck-go()
  (interactive)
  (flycheck-mode 1)
  (flycheck-list-errors)
  )


(defun my-select-region-by-line-number (start end)
  (interactive "nStart Line: \nnEnd Line:")
  ;;(message "Lines: %i %i " start end)
  (goto-line start)
  ;;(message "gone to: %i" start)
  (goto-line end)
  ;;(message "gone to %i" end)
  (activate-mark)
  )

(defun my-eval-line ()
  (interactive)
  (beginning-of-line)
  (let ((m (point-marker)))
    (end-of-line)  
    (eval-region (region-beginning) (region-end))
    )
  )


;Set a window to be dedicated
(defun toggle-window-dedicated()
  (interactive)
 (message 
  (if (let (window (get-buffer-window (current-buffer)))
	(set-window-dedicated-p window 
				(not (window-dedicated-p window))))
    "Window '%s' is dedicated"
    "Window '%s' is normal")
  
  (current-buffer)))

;Set the Frame width
(defun jgsfw (arg)
  (interactive "p")
  (set-frame-width(selected-frame) arg))

;?
(defun buffer-in-window-list ()
  (let (buffers)
    (walk-windows (lambda (window) (push (window-buffer window)
  buffers)) t t)
    buffers))

;?
(defun display-all-buffers ()
  (interactive)
  (let (buffers-in-window (buffer-in-window-list))
    (dolist (buffer (buffer-list))
      (when (and (not (string-match "\\`[[:space:]]*\\*" (buffer-name
  buffer)))
                 (not (memq buffer buffers-in-window)))
        (set-window-buffer (split-window (get-largest-window))
  buffer)))
    (balance-windows)))

;
(defun file-string(file)
  "Read the contents of a file and return as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


;Date stuff:
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-author-date-time()
  (interactive)
  (insert "Author: John Grey ")
  (insert (format-time-string current-date-time-format (current-time)))
)

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

(defun insert-lparen()
  (interactive)
  (insert "(")
  )

(defun insert-rparen()
  (interactive)
  (insert ")")
  )


(defalias 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;call occur for all tags in the file:
(defun tag-occurances ()
  (interactive)
  ;;save eventually to a new buffer
  (with-output-to-temp-buffer "*tags*"
    (save-excursion ;;store where you are in the current
      (goto-char (point-min))
      ;;where to store tags:
      (let ((tag-set (make-hash-table :test 'equal)))
        ;;match all
        (while (not (eq nil (re-search-forward ":\\([[:graph:]]+\\):\\(\.\.\.\\)?\$" nil t)))
          ;;split tags into list
          (let ((tags (split-string (match-string-no-properties 0) ":" t ":")))
            ;;increment counts
            (mapc (lambda (x) (puthash x (+ 1 (gethash x tag-set 0)) tag-set)) tags)
            )
          )
        ;;now turn them into pairs
        (let ((hashPairs nil) (sorted '()))
          (maphash (lambda (k v) (push `(,k ,v) hashPairs)) tag-set)
          (setq sorted (sort hashPairs (lambda (a b) (string-lessp (car a) (car b)))))
          ;;print them all out
          (mapc (lambda (x)
                  (princ (string-join `(,(car x)
                                        ,(make-string (- 20 (length (car x))) ?\ )
                                         ": "
                                         ,(number-to-string (cadr x))
                                         ,(make-string (- 5 (length (number-to-string (cadr x)))) ?\ )
                                         " : "
                                         ,(make-string (cadr x) ?=)
                                         "\n"
                                         ))))
                  sorted)
          )
        )
      )
    )
  )

(defun list-agenda-files ()
  (interactive)
  (with-output-to-temp-buffer "*Agenda Files*"
    (mapc (lambda (x) (progn
                        (princ x)
                        (princ "\n")
                  ))
          org-agenda-files)
    )
  )


(defun my-indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-url-insert-file-contents (url)
  "Prompt for URL and insert file contents at point."
  (interactive "sURL: ")
  (url-insert-file-contents url))



;;from emacswiki.org/emacs/DebugMessages:
(defadvice message (before who-said-that activate)
  "Find out who said that thing and say so"
  (let ((trace nil) (n 1) (frame nil))
    (while (setq frame (backtrace-frame n))
      (setq n (1+ n)
            trace (cons (cadr frame) trace)) )
    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 9)))
    (ad-set-args 1 (cons trace (ad-get-args 1))) ))


;;deactivate the above:
(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)



