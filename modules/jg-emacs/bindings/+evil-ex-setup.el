;;; main/personal/+evil-setup.el -*- lexical-binding: t; -*-

(message "Setting up Evil-Ex bindings: %s" (current-time-string))

(defvar evil-ex-commands nil
  " Custom Evil-ex commands")

;; definition of said commands, adapted from evil-maps

(evil-ex-define-cmd "os"                #'org-store-link)
(evil-ex-define-cmd "oi"                #'org-insert-last-stored-link)
(evil-ex-define-cmd "oo"                #'+jg-personal-open-link)
(evil-ex-define-cmd "OO"                #'+jg-personal-open-link-externally)
;; TODO: registers
;; TODO: yasnippet

;; file / buffer loading
(evil-ex-define-cmd "e[dit]"            #'evil-edit)
(evil-ex-define-cmd "w[rite]"           #'evil-write)
(evil-ex-define-cmd "wa[ll]"            #'evil-write-all)
(evil-ex-define-cmd "sav[eas]"          #'evil-save)
(evil-ex-define-cmd "r[ead]"            #'evil-read)
(evil-ex-define-cmd "b[uffer]"          #'evil-buffer)

(evil-ex-define-cmd "bn[ext]"           #'evil-next-buffer)
(evil-ex-define-cmd "bp[revious]"       #'evil-prev-buffer)
(evil-ex-define-cmd "bN[ext]" "bprevious")
(evil-ex-define-cmd "sb[uffer]"         #'evil-split-buffer)
(evil-ex-define-cmd "sbn[ext]"          #'evil-split-next-buffer)
(evil-ex-define-cmd "sbp[revious]"      #'evil-split-prev-buffer)
(evil-ex-define-cmd "sbN[ext]" "sbprevious")

;; buffer and file listings
(evil-ex-define-cmd "buffers"           #'buffer-menu)
(evil-ex-define-cmd "files"             #'evil-show-files)
(evil-ex-define-cmd "ls" "buffers")

;; deleting / yanking
;; (evil-ex-define-cmd "c[hange]"       #'evil-change)
(evil-ex-define-cmd "c[opy]"            #'evil-copy)
(evil-ex-define-cmd "m[ove]"            #'evil-move)
(evil-ex-define-cmd "d[elete]"          #'evil-ex-delete)
(evil-ex-define-cmd "y[ank]"            #'evil-ex-yank)
;;goto top of buffer
(evil-ex-define-cmd "top"               #'evil-goto-char)
;; bring a line up
(evil-ex-define-cmd "j[oin]"            #'evil-ex-join)
;; alignment
(evil-ex-define-cmd "le[ft]"            #'evil-align-left)
(evil-ex-define-cmd "ri[ght]"           #'evil-align-right)
(evil-ex-define-cmd "ce[nter]"          #'evil-align-center)
;; windows/buffer creation
(evil-ex-define-cmd "sp[lit]"           #'evil-window-split)
(evil-ex-define-cmd "vs[plit]"          #'evil-window-vsplit)
(evil-ex-define-cmd "new"               #'evil-window-new)
(evil-ex-define-cmd "ene[w]"            #'evil-buffer-new)
(evil-ex-define-cmd "vne[w]"            #'evil-window-vnew)
;; window deletion
(evil-ex-define-cmd "clo[se]"           #'evil-window-delete)
(evil-ex-define-cmd "on[ly]"            #'delete-other-windows)
;; quitting emacs
(evil-ex-define-cmd "q[uit]"            #'evil-quit)
(evil-ex-define-cmd "wq"                #'evil-save-and-close)
(evil-ex-define-cmd "quita[ll]"         #'evil-quit-all)
(evil-ex-define-cmd "qa[ll]" "quitall")
(evil-ex-define-cmd "cq[uit]"           #'evil-quit-all-with-error-code)
(evil-ex-define-cmd "wqa[ll]"           #'evil-save-and-quit)
(evil-ex-define-cmd "xa[ll]" "wqall")
(evil-ex-define-cmd "x[it]"             #'evil-save-modified-and-close)
(evil-ex-define-cmd "exi[t]"            #'evil-save-modified-and-close)
(evil-ex-define-cmd "bd[elete]"         #'evil-delete-buffer)
(evil-ex-define-cmd "bw[ipeout]"        #'evil-delete-buffer)
;; state change
(evil-ex-define-cmd "g[lobal]"          #'evil-ex-global)
(evil-ex-define-cmd "v[global]"         #'evil-ex-global-inverted)
(evil-ex-define-cmd "norm[al]"          #'evil-ex-normal)
;; substitution
(evil-ex-define-cmd "s[ubstitute]"      #'evil-ex-substitute)
(evil-ex-define-cmd "&"                 #'evil-ex-repeat-substitute)
(evil-ex-define-cmd "&&"                #'evil-ex-repeat-substitute-with-flags)
(evil-ex-define-cmd "~"                 #'evil-ex-repeat-substitute-with-search)
(evil-ex-define-cmd "~&"                #'evil-ex-repeat-substitute-with-search-and-flags)
;; registers and marks
(evil-ex-define-cmd "registers"         #'evil-show-registers)
(evil-ex-define-cmd "marks"             #'evil-show-marks)
(evil-ex-define-cmd "delm[arks]"        #'evil-delete-marks)
(evil-ex-define-cmd "ju[mps]"           #'evil-show-jumps)
(evil-ex-define-cmd "noh[lsearch]"      #'evil-ex-nohighlight)
(evil-ex-define-cmd "f[ile]"            #'evil-show-file-info)
;; shifting / aligning
(evil-ex-define-cmd "<"                 #'evil-shift-left)
(evil-ex-define-cmd ">"                 #'evil-shift-right)
;; print last line number
(evil-ex-define-cmd "="                 #'evil-ex-line-number)
(evil-ex-define-cmd "!"                 #'evil-shell-command)
(evil-ex-define-cmd "@:"                #'evil-ex-repeat)
(evil-ex-define-cmd "mak[e]"            #'evil-make)
;; errors
(evil-ex-define-cmd "cc"                #'evil-goto-error)
(evil-ex-define-cmd "cfir[st]"          #'first-error)
(evil-ex-define-cmd "cr[ewind]"         #'first-error)
(evil-ex-define-cmd "cn[ext]"           #'next-error)
(evil-ex-define-cmd "cp[revious]"       #'previous-error)
(evil-ex-define-cmd "set-initial-state" #'evil-ex-set-initial-state)
(evil-ex-define-cmd "show-digraphs"     #'evil-ex-show-digraphs)
;;sorting
(evil-ex-define-cmd "sor[t]"            #'evil-ex-sort)
;; window resizing
(evil-ex-define-cmd "res[ize]"          #'evil-ex-resize)

(provide 'jg-evil-ex-bindings)
