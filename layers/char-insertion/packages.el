(defconst char-insertion-packages
  '(
    helm
    )
)

(defun char-insertion/post-init-helm ()
  ;; (get-char-code-property (char-after (point-min)) 'name)
  ;; (charset-chars 'unicode-bmp 1)
  ;; (charset-dimension 'unicode-bmp)
  ;; ((lambda (x) (get-char-code-property (decode-char 'unicode-bmp (string-to-number (format "%d" x))) 'name)) #xFFD5 )

  (spacemacs/set-leader-keys "x C" 'jg_layer/char-inserting-helm-start)
  )



