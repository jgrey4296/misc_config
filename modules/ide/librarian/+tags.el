;;; +tags.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(use-package! librarian-tag-helm
  :after librarian
  :commands librarian-tag-helm
  )

(use-package! librarian-tag-ivy
  :after librarian
  :defer t
  )

(use-package! rawtag-mode
  :commands rawtag-mode
  )

(use-package! subfile-mode
  :commands subfile-mode
  )


(evil-ex-define-cmd "ht[ag]"  #'librarian-tag-helm)
(evil-ex-define-cmd "t[ag]"   #'librarian-tag-helm)
(evil-ex-define-cmd "T[ag]"   #'librarian-tag-helm)
(evil-ex-define-cmd "it[ag]"  #'librarian-tag-ivy)

(defvar jg-tag-loc-twitter-account-index  (expand-file-name "~/.config/bibliography/.temp/index/tw_acct.index"))
(defvar jg-tag-loc-twitter-grep-index     (expand-file-name "~/.config/bibliography/.temp/index/grep_tags.index"))
(defvar jg-tag-loc-twitter-tag-index      (expand-file-name "~/.config/bibliography/.temp/index/tw_tag.index"))
(defvar jg-tag-loc-twitter                "/Volumes/documents/twitter_threads/")

(speckler-add! auto-modes ()
  '(subfile
    ("\\.sub\\'"                 . subfile-mode)
    )
  )
(speckler-add! popup ()
  '(tagging
    ("^\\*Helm-Bookmark-Results\\*"  :side right :ttl nil :width 0.4 :quit t :select nil :priority 50)
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 25, 2025
;; Modified:   January 25, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +tags.el ends here
