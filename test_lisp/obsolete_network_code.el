(require 'tramp)
  (require 'request)
  (print "SETTING UP TWITTER EXTENSION")
  (defun jg_layer/getassq (name l)
    (cdr (twittering-assoc-string name l))
    )

  (defun jg_layer/send-request (info append_info http_params sentinel &optional use_get print_flag)
    (let* (
           (additional-info `(
                              (target-buffer . ,(jg_layer/getassq 'target-buffer info))
                              (account-info . ,(jg_layer/getassq 'account-info info))
                              (host . ,(jg_layer/getassq 'host info))
                              (method . ,(jg_layer/getassq 'method info))
                              (format-str . ,(jg_layer/getassq 'format-str info)))
                              )
           (func (if use_get 'twittering-http-get 'twittering-http-post))
           (account_info (jg_layer/getassq 'account-info info))
           (host (jg_layer/getassq 'host info))
           (method (jg_layer/getassq 'method info))
           (format-str (jg_layer/getassq 'format-str info))
           (uri (string-join `(,host ,method "." ,format-str)))
           (all_params (append http_params account_info))
           )
      (print (format "Request: %s %s " use_get func))
      (if append_info
          (set 'additional-info (append append_info additional-info)))
      (if print_flag
          (print (format "Send-Request:\n%s\n%s\n%s\n%s\n%s\n%s" account_info
                         host method http_params format-str additional-info)))

      (print (format "REQUEST URI: %s" uri))
      (print (format "All Params: %s" all_params))
      (request
       :type "POST"
       :data all_params
       :parser 'json-read
       :success (lambda (&allow-other-keys)
                  (print (format "REQUEST SENTINEL: %s" 'args)))
       )
      )
    )

(defun jg_layer/check_status (p s c h)
  (print (format "Checking Status: %s" h))
  (let* ((processing_info (jg_layer/getassq 'processing_info h))
         (state (jg_layer/getassq 'state processing_info))
         (media_id (jg_layer/getassq 'x-mediaid h)))
    (cond
     ((string-equal "failed" state) (print "UPLOAD FAILED"))
     ((string-equal "pending" state) (print "PENDING"))
     ((string-equal "in_progress" state) (print "IN PROGRESS"))
     ((string-equal "succeeded" state) (jg_layer/add_media_id_to_tweet media_id))
     ('t (print "UNKNOWN STATE")))
    )
  "0"
  )

(defun jg_layer/twitter-http-sentinel-final (p s c h)
  ;; if a failure occurs:
  ;; resend the chunk
  (print (format "Final sentinel: %s" h))
  (let* ((status (string-to-number (jg_layer/getassq 'status-code h)))
         (media_id_string (jg_layer/getassq 'media_id c))
         (target-buffer (jg_layer/getassq 'target-buffer c))
         )
    (print "----")
    (cond
     ;; if bad, get status
     ;; ((<= 400 (string-to-number (jg_layer/getassq 'status-code h)))
     ;;  (jg_layer/send-request c nil `(("command" . "STATUS")
     ;;                                 ("media_id" . ,media_id_string))
     ;;                         'jg_layer/check_status
     ;;                         't 't))
     ((jg_layer/getassq 'processing_info h)
      (progn (print "Need to get a status")))
     ('t (jg_layer/add_media_id_to_tweet media_id_string target-buffer))
     ))
  "0"
  )

(defun jg_layer/twitter-http-chunk-sentinel-recursive (p s c h)
    ;; the recursive portion of the chunking upload
    ;; when no more chunks remain:
    ;; send a finalize message, repeating a status query until success
    ;; then insert the media ID to the tweet buffer
    (print (format "Recursive Sentinel: %s" h))
    (if (< 300 (string-to-number (jg_layer/getassq 'status-code h)))
        (progn (print "FAILING") (assert nil)))

    (let* ((finalize (eq 1 (length (jg_layer/getassq 'chunks c))))
          (command (if finalize "FINALIZE" "APPEND"))
          (media_id_string (jg_layer/getassq 'x-mediaid h))
          (chunks (cdr (jg_layer/getassq 'chunks c)))
          (chunk (car chunks))
          (http-parameters `(("command" . ,command)
                             ("media_id" . ,media_id_string)))
          (append_info `((chunks . ,chunks) (media_id . ,media_id_string)))
          )
      (if (not finalize)
          (set 'http-parameters (append `(("media_data" . ,(cdr chunk))
                                          ("content_type" . "multipart/form-data")
                                          ("Content-Transfer-Encoding" . "base64")
                                          ("segment_index" . ,(number-to-string (car chunk))))
                                        http-parameters)))
      (print "----")
      (print (format "%s Chunk id: %s / %s " command (car chunk) (length chunks)))
      (jg_layer/send-request c append_info http-parameters
                             (if (not finalize)
                                 'jg_layer/twitter-http-chunk-sentinel-recursive
                               'jg_layer/twitter-http-sentinel-final
                               )
                             nil
                             (if finalize 't nil))
      )
      "0"
    )

(defun jg_layer/twitter-http-chunk-sentinel (p s c h)
    ;; Get the additional-info needed from connection-info
    ;; ie: get chunks and
    (print "Initial Sentinel")
    (let* ((media_id_string (jg_layer/getassq 'x-mediaid h))
           (chunk (car (jg_layer/getassq 'chunks c)))
           (chunk_index (number-to-string (car chunk)))
           (chunk_data (cdr chunk))
           (http-parameters `(("command" . "APPEND")
                              ("media_id" . ,media_id_string)
                              ("media_data" . ,chunk_data)
                              ("content_type" . "multipart/form-data")
                              ("segment_index" . ,chunk_index)
                              ("Content-Transfer-Encoding" . "base64")
                              ))
           (append_info `((chunks . ,(jg_layer/getassq 'chunks c))
                          (media_id . ,media_id_string)))
           )
      (print "----")
      (print (format "Chunk id: %s / %s " chunk_index (length (jg_layer/getassq 'chunks c))))
      (jg_layer/send-request c append_info http-parameters
                             'jg_layer/twitter-http-chunk-sentinel-recursive
                             )
     )
    "0"
    )

(defun jg_layer/twitter-open-and-encode-picture (candidate)
    """ read in the file, encode it, and give to uploading
        returns a tuple of (list((index . chunk)) file_size)
      """
    (assert (file-exists-p candidate))
    ;; TODO add size and file type checks

    (with-temp-buffer
      (insert-file-contents-literally candidate)
      (let* ((data (buffer-substring-no-properties 1 (point-max)))
             (encoded (base64-encode-string (buffer-string)))
             ;;size
             (fsize (length encoded))
             ;; chunks - reduce over encoded grabbing size X substrings
             (chunks '())
             (startPos 1)
             (endOfString (length encoded))
             (chunkSize 5000)
             (lastChunkIndex 0)
             )
        (assert (< fsize 500000))
        (while (< (+ startPos chunkSize) endOfString)
          ;;get next chunk
          (let ((endOfChunk (+ startPos chunkSize)));;(- chunkSize 0))))
            (setq chunks (cons `(,lastChunkIndex . ,(substring-no-properties encoded startPos endOfChunk)) chunks)
                  lastChunkIndex (+ 1 lastChunkIndex)
                  startPos endOfChunk
                  )
            )
          )
        (set 'chunks (cons `(,lastChunkIndex . ,(substring-no-properties encoded startPos))
                           chunks))
        `(,(reverse chunks) ,fsize))))

(defun jg_layer/twitter-upload-image (candidate)
    """ init, chunk and finalize an image upload, return media id """
    (let* ((data (jg_layer/twitter-open-and-encode-picture candidate))
           (data_chunks (car data))
           (data_size (cadr data))
           (account-info-alist (twittering-get-main-account-info))
           (http-parameters `(("command" . "INIT")
                              ("total_bytes" . ,(number-to-string data_size))
                              ;; TODO do this programmatically
                              ("media_category" . "TweetImage")
                              ("media_type" . "image/png")
                              ))
           (additional-info `((account-info . ,account-info-alist)
                              (host . "upload.twitter.com/")
                              (method . "1.1/media/upload")
                              (format-str . "json")
                              (target-buffer . ,(buffer-name))))
           (append-info `((chunks . ,data_chunks)))
           )
      (print (format "Starting Upload size: %s" data_size))
      ;; start the upload
      (jg_layer/send-request additional-info
                             append-info
                             http-parameters
                             'jg_layer/twitter-http-chunk-sentinel
                             )
      )
    )
