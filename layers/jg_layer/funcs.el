;; jg_emacs funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))


;;----------------------------------------
(defun jg_layer/twitter-extend ()
  (when (configuration-layer/package-usedp 'twittering)
    (defun jg_layer/twitter-http-chunk-cleanup-sentinel (p s c h)
      ;; cleanup / refire a chunk sending
      ;; if a failure occurs:
      ;; resend the chunk
      (let ((status (assq 'status h))
            (finalize (eq 0 (length (assq 'chunks c))))
            (command (if  finalize "FINALIZE" "APPEND"))
            (media_id_string (assq 'media_id_string h))
            (chunks (assq 'chunks c))
            (chunk (car chunks))
            (http-parameters '(("command" . ,command)
                               ("media_id" . ,media_id_string)
                               ,(if (not finalize) `("media_data" . ,(car chunk)))
                               ,(if (not finalize) `("segment_index" . ,(cadr chunk)))
                               ))
            (additional-info `((account-info ,(assq 'account-info c))
                               (host ,(assq 'host c))
                               (format-str ,(assq 'format-str c))
                               (chunks ,chunks)))
            )
	(print (format "Cleanup sentinel: %s  %s" status finalize))
	(cond
	 ;; Retry chunk
	 ((and (> status 300) (not finalize)) (twittering-http-post (assq 'account-info c)
								    (assq 'host c)
								    (assq 'method c)
								    http-parameters
								    (assq 'format-str c)
								    additional-info
								    jg_layer/twitter-http-chunk-sentinel-recursive
								    jg_layer/twitter-http-chunk-cleanup-sentinel
								    ))
	 ;; Finalize attempted, retry status. check processing info for state "in_progress"
	 ((and finalize (> status 300)) (twittering-http-post (assq 'account-info c)
                                                              (assq 'host c)
                                                              (assq 'method c)
                                                              (cons '("command" . "STATUS") (cdr http-parameters))
                                                              (assq 'format-str c)
                                                              additional-info
                                                              nil
                                                              jg_layer/twitter-http-chunk-cleanup-sentinel
                                                              ))
	 ;; finalize success if success, if no processing_info, or state is "succeeded"
	 ((and finalize (< status 300)) (print "Add media ID to tweet"))
	 )
	)
      )

    (defun jg_layer/twitter-http-chunk-sentinel-recursive (p s c h)
      ;; the recursive portion of the chunking upload
      ;; when no more chunks remain:
      ;; send a finalize message, repeating a status query until success
      ;; then insert the media ID to the tweet buffer
      (assert (and (> (assq 'status h) 200) (< 300 (assq 'status h))))
      (let ((finalize (eq 1 (length (assq 'chunks c))))
            (command (if  finalize "FINALIZE" "APPEND"))
            (media_id_string (assq 'media_id_string h))
            (chunks (if (not finalize) (cdr (assq 'chunks c)) '()))
            (chunk (if (not finalize) (car chunks) nil))
            (http-parameters '(("command" . ,command)
                               ("media_id" . ,media_id_string)
                               ,(if (not finalize) `("media_data" . ,(car chunk)))
                               ,(if (not finalize) `("segment_index" . ,(cadr chunk)))
                               ))
            (additional-info `((account-info ,(assq 'account-info c))
                               (host ,(assq 'host c))
                               (format-str ,(assq 'format-str c))
                               (chunks ,chunks)))
            )
	(print (format "Recursive Sentinel: %s" command))
	(twittering-http-post (assq 'account-info c)
                              (assq 'host c)
                              (assq 'method c)
                              http-parameters
                              (assq 'format-str c)
                              additional-info
                              (if (not finalize) jg_layer/twitter-http-chunk-sentinel-recursive nil)
                              jg_layer/twitter-http-chunk-cleanup-sentinel
                              )
	)
     )

    (defun jg_layer/twitter-http-chunk-sentinel (p s c h)
      ;; Get the additional-info needed from connection-info
      ;; ie: get chunks and
      (let ((command "APPEND")
            (media_id_string (assq 'media_id_string h))
            (chunk (car (assq 'chunks c)))
            (chunk_index (car chunk))
            (chunk_data (cadr chunk))
            (http-parameters '(("command" . ,command)
                               ("media_id" . ,media_id_string)
                               ("media_data" . ,chunk_data)
                               ("segment_index" . ,chunk_index)
                               ))
            (additional-info `((account-info ,(assq 'account-info c))
                               (host ,(assq 'host c))
                               (format-str ,(assq 'format-str c))
                               (chunks ,(assq 'chunks c))))
            )
	(print "Initial Sentinel")
	(twittering-http-post (assq 'account-info c)
                              (assq 'host c)
                              (assq 'method c)
                              http-parameters
                              (assq 'format-str c)
                              additional-info
                              jg_layer/twitter-http-chunk-sentinel-recursive
                              )
	)
      )

    (defun jg_layer/twitter-open-and-encode-picture (candidate)
      """ read in the file, encode it, and give to uploading
		returns a tuple of (list((index . chunk)) file_size)
 """
      (assert (file-exists-p candidate))
      ;; TODO add size and file type checks

      (print "Opening and Encoding Picture")
      (with-temp-buffer
	(insert-file-contents candidate)
	(let* ((encoded (base64-encode-string (buffer-string)))
               ;;size
               (fsize (f-size candidate))
               ;; chunks - reduce over encoded grabbing size X substrings
               (chunks '())
               (startPos 0)
               (endOfString (length encoded))
               (chunkSize 1000)
               (lastChunkIndex 0)
               )
          (while (< (+ startPos chunkSize) endOfString)
            ;;get next chunk
            (let ((endOfChunk (+ startPos chunkSize)))
              (setq 'chunks (cons `(,lastChunkIndex . ,(substring-no-properties encoded startPos endOfChunk)) chunks)
                    'lastChunkIndex (+ 1 lastChunkIndex)
                    'startPos endOfChunk
                    )
              )
            )
          (set 'chunks (cons `(,lastChunkIndex . ,(substring-no-properties encoded startPos endOfString)) chunks))
          `(,chunks ,fsize))))


    (defun jg_layer/twitter-upload-image (candidate)
      """ init, chunk and finalize an image upload, return media id """
      (let* ((data (jg_layer/twitter-open-and-encode-picture candidate))
             (data_chunks (car data))
             (data_size (cadr data))
             (account-info-alist (twittering-get-main-account-info))
             (host "upload.twitter.com/1.1/media")

             (method "upload")
             (format-str "json")
             (media-type "jpg")
             (http-parameters `(("command" . "INIT")
				("total_bytes" . (number-to-string data_size))
				("media_type" . "image/png") ;; TODO do this programmatically
				))
             (additional-info `((account-info ,account-info-alist)
				(host ,host)
				(format-str ,format-str)
				(chunks data_chunks))
                              )
             )
	(print "Starting Upload")
	;; start the upload
	(twittering-http-post account-info-alist
                              host
                              method
                              http-parameters
                              format-str
                              additional-info
                              jg_layer/twitter-http-init-sentinel
                              )
	)
      )

    ;; implement the action properly
    (setq-default jg_layer/twitter-image-helm-source
                  (helm-make-source "Find Image" 'helm-source-ffiles
				    :action '(("action" . jg_layer/twitter-upload-image))))

    (defun jg_layer/twitter-add-picture ()
      (interactive)
      (helm :sources jg_layer/twitter-image-helm-source
            :input "./"))

    (let ((km twittering-edit-mode-map))
      (define-key km (kbd "C-c C-o") 'jg_layer/twitter-add-picture)
      )

    )
)
;;----------------------------------------
(when (configuration-layer/package-usedp 'auto-complete)
  (defun jg_layer/ac-trigger ()
    (interactive)
    (auto-complete)
    )
  )


(when (configuration-layer/package-usedp 'org)
  (defun jg_layer/open_link_in_buffer ()
    (interactive)
    (org-open-at-point 'in-emacs)
    )


  (defun jg_layer/list-agenda-files ()
    """ Creates a temporary, Org-mode buffer with links to agenda files """
    (interactive)
    (with-output-to-temp-buffer "*Agenda Files*"
      (set-buffer "*Agenda Files*")
      (insert "Agenda Files: ")
      (insert "\n")
      (mapc (lambda (x)
              (let ((file_name (last (split-string x "/" t ".org"))))
		(insert (format "[[%s][%s]]\n" x file_name))
		)) org-agenda-files)
      (org-mode)
      )
    )

  (defun jg_layer/make-bar-chart (data)
    (mapc (lambda (x)
            (princ (string-join `(,(car x)
                                  ,(make-string (- (+ 10 maxTagLength) (length (car x))) ?\ )
                                  ": "
                                  ,(number-to-string (cadr x))
                                  ,(make-string (- 5 (length (number-to-string (cadr x)))) ?\ )
                                  " : "
                                  ,(make-string (cadr x) ?=)
                                  "\n"
                                  ))))
          data))



  (defun jg_layer/org-get-buffer-tags ()
    """ TODO change this to use org-get-tags """
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
        tag-set
        )
      )
    )

  (defun jg_layer/tag-occurences-in-open-buffers()
    """ retrieve all tags in all open buffers, print to a temporary buffer """
    (interactive)
    (let* ((allbuffers (buffer-list))
           (alltags (make-hash-table :test 'equal))
           (hashPairs nil)
           (sorted '())
           (maxTagLength 0))
      (map 'list (lambda (bufname)
                   (with-current-buffer bufname
                     (let ((buftags (jg_layer/org-get-buffer-tags)))
                       (maphash (lambda (k v)
                                  (puthash k (+ v (gethash k alltags 0)) alltags))
                                buftags)
                       ))) allbuffers)

      (maphash (lambda (k v) (push `(,k ,v) hashPairs)) alltags)
      ;; (setq sorted (sort hashPairs (lambda (a b) (string-lessp (car a) (car b)))))
      (setq sorted (sort hashPairs (lambda (a b) (> (cadr a) (cadr b)))))
      (setq maxTagLength (apply `max (mapcar (lambda (x) (length (car x))) sorted)))
      (with-output-to-temp-buffer "*tags - all*"
        (jg_layer/make-bar-chart sorted)
        )
      )
    )

  (defun jg_layer/tag-occurances ()
    """ Count all occurrences of all tags and bar chart them """
    (interactive)
    ;;save eventually to a new buffer
      (let ((tag-set (jg_layer/org-get-buffer-tags))
            (hashPairs nil)
            (sorted '())
            (maxTagLength 0))
        (maphash (lambda (k v) (push `(,k ,v) hashPairs)) tag-set)
        ;; (setq sorted (sort hashPairs (lambda (a b) (string-lessp (car a) (car b)))))
        (setq sorted (sort hashPairs (lambda (a b) (> (cadr a) (cadr b)))))
        (setq maxTagLength (apply `max (mapcar (lambda (x) (length (car x))) sorted)))
        ;;print them all out
        (with-output-to-temp-buffer "*tags*"
          (jg_layer/make-bar-chart sorted)
          )
        )
      )

  )

(defun jg_layer/org-setup-tagging ()
  """ Setup Helm Tagging, but to be called in init.d
		so both org and helm are set up """
        (require 'helm)
        (defun jg_layer/org-set-tags (x)
          """ Toggle Selected Tags """
          (let* ((candidates (helm-marked-candidates))
                 (current-tags (org-get-tags nil t)))
            (print candidates)
            (print current-tags)
            (map 'list (lambda (candidate)
                         (print candidate)
                         (if (not (-contains? current-tags candidate))
                             (push candidate current-tags)
                           (setq current-tags (remove candidate current-tags))
                           )) candidates)
            (org-set-tags current-tags)
            ))

        (defun jg_layer/sort-candidates (a b)
          """ Sort candidates by colour then lexicographically """
          (let ((aprop (get-text-property 0 'font-lock-face a))
                (bprop (get-text-property 0 'font-lock-face b)))
            (cond
             ((and aprop bprop (string-lessp a b)) t)
             ((and aprop (not bprop)) t)
             ((and (not aprop) (not bprop) (string-lessp a b)))
             )))

        (defun jg_layer/org-tagging-properties-add (candidates-map)
          """ Given Candidates, colour them if they are assigned, then sort them  """
          (let* ((candidates (hash-table-keys candidates-map))
                 (current-tags (org-get-tags nil t))
                 (propertied-tags (map 'list (lambda (candidate)
                                               (if (-contains? current-tags candidate)
                                                   (put-text-property 0 (length candidate) 'font-lock-face 'rainbow-delimiters-depth-1-face candidate))
                                               candidate) candidates))
                 )
            (sort propertied-tags 'jg_layer/sort-candidates)
            )
          )

        ;; The Two Sources For Tagging Helm
        (setq jg_layer/org-tagging-helm `((name . "Helm Tagging")
                                          (action . jg_layer/org-set-tags)
                                          )
              jg_layer/org-tagging-fallback-source (helm-build-dummy-source ""
                                                     :action '((helm-pattern . jg_layer/org-set-tags)))
              )

        (defun jg_layer/org-tagging-helm-start ()
          """ Opens the Tagging Helm """
          (interactive)
          (let* ((candidates (jg_layer/org-tagging-properties-add (jg_layer/org-get-buffer-tags)))
                 (main-source (cons `(candidates . ,candidates) jg_layer/org-tagging-helm)))
            (helm :sources '(main-source jg_layer/org-tagging-fallback-source)
                  :input "")
            )
          )
        )

;;----------------------------------------
(defun jg_layer/insert-lparen ()
  """ utility to insert a (  """
  (interactive)
  (insert "(")
  )

(defun jg_layer/insert-rparen ()
  """ utility to insert a ) """
  (interactive)
  (insert ")")
  )

(defun jg_layer/flatten (lst)
  """ Utility to flatten a list """
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (assert (listp lst))
      (funcall internal lst))))


(defun jg_layer/clear-buffer ()
  """ Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ """
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

;;----------------------------------------
(defun jg_layer/example_transient_func ()
  (interactive)
  (print "blah")
  )

(spacemacs|define-transient-state example_transient
  :title "an example transient state"
  :doc (concat "
[_q_]^^ Quit         [_h_]^^ Example Func
[_t_]^^ Example Func
")
  :bindings
  ("q" nil :exit t)
  ("t" jg_layer/example_transient_func)
  ("h" jg_layer/example_transient_func :exit t)
  )

(spacemacs/set-leader-keys "." 'spacemacs/example_transient-transient-state/body)


;;----------------------------------------
;; (setq-default jg_layer/example_helm-source
;;               (helm-make-source "Find Image" 'helm-source-ffiles
;;                 :action '(("action" . (lambda (candidate) (insert candidate))))))

;; (defun jg_layer/example_helm-call ()
;;   (interactive)
;;   (helm :sources jg_layer/example_helm-image-helm-source
;;         :input "./"))
