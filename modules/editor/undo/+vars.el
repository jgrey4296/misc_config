;;; +vars.el -*- lexical-binding: t; -*-

 ;; Increase undo history limits to reduce likelihood of data loss
 ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
 ;; truncating the undo history and corrupting the tree. See
 ;; https://github.com/syl20bnr/spacemacs/issues/12110
(setq-default undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              )
