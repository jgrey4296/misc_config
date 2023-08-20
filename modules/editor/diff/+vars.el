;;; +vars.el -*- lexical-binding: t; -*-


(setq diff-command "diff"
      diff-switches "-u"
      ;; (string-join '(
      ;;                "--color" "never" ;; always/auto/never
      ;;                "--syntax-highlight" "off" ;; on/off
      ;;                "--display" "side-by-side" ;; side-by-side/inline
      ;;                ) " ")

      vdiff-diff-algorithm 'diff
      vdiff-diff3-command '("diff3")

      ediff-custom-diff-program "diff"
      ediff-custom-diff-options "-c"

      vc-git-diff-switches '("--histogram")
      )

(setq ediff-diff-options "-w" ; turn off whitespace checking
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;;-- vdiff

(setq vdiff-lock-scrolling t ;; Whether to lock scrolling by default when starting vdiff

      ;; diff program/algorithm to use. Allows choice of diff or git diff along with
      ;; the various algorithms provided by these commands. See
      ;; `vdiff-diff-algorithms' for the associated command line arguments.
      vdiff-diff-algorithms '((diff . "diff -u")
                              (diff-minimal . "diff -u --minimal")
                              (git-diff           . "git --no-pager diff --no-index --no-color")
                              (git-diff-myers     . "git --no-pager diff --myers --no-index --no-color")
                              (git-diff-minimal   . "git --no-pager diff --minimal --no-index --no-color")
                              (git-diff-patience  . "git --no-pager diff --patience --no-index --no-color")
                              (git-diff-histogram . "git --no-pager diff --histogram --no-index --no-color")
                              (custom . "difft")
                              )

      vdiff-disable-folding nil
      vdiff-fold-padding 6 ;; Unchanged lines to leave unfolded around a fold
      vdiff-min-fold-size 4 ;; Minimum number of lines to fold
      vdiff-may-close-fold-on-point t ;; allow closing new folds around point after updates.

      ;; Function that returns the string printed for a closed fold. The arguments
      ;; passed are the number of lines folded, the text on the first line, and the
      ;; width of the buffer.
      vdiff-fold-string-function 'vdiff-fold-string-default

      ;; Default syntax table class code to use for identifying "words" in
      ;; `vdiff-refine-this-change'. Some useful options are
      ;;
      ;; "w"   (default) words
      ;; "w_"  symbols (words plus symbol constituents)
      ;;
      ;; For more information see
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
      vdiff-default-refinement-syntax-code "w"
      vdiff-auto-refine nil ;; If non-nil, automatically refine all hunks.

      ;; How to represent subtractions (i.e., deleted lines). The
      ;; default is full which means add the same number of (fake) lines
      ;; as those that were removed. The choice single means add only one
      ;; fake line. The choice fringe means don't add lines but do
      ;; indicate the subtraction location in the fringe.
      vdiff-subtraction-style 'full

      ;; Character to use for filling subtraction lines. See also `vdiff-subtraction-style'.
      vdiff-subtraction-fill-char ?-

      )

;;-- end vdiff

;;-- diff-hl
(setq diff-hl-flydiff-delay 0.5  ; default: 0.3
      diff-hl-show-staged-changes nil
      )

;;-- end diff-hl

;;-- specs
(spec-handling-add! popup
                    `(diff
                      ("^\\*diff-hl" :select nil :size ,#'+popup-shrink-to-fit)
                      )
                    )

;;-- end specs
