;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-

;;-- definitions
(defvar +ivy-buffer-preview nil
  "If non-nil, preview buffers while switching, à la `counsel-switch-buffer'.

When nil, don't preview anything.
When non-nil, preview non-virtual buffers.
When 'everything, also preview virtual buffers")

(defvar +ivy-buffer-unreal-face 'font-lock-comment-face
  "The face for unreal buffers in `ivy-switch-to-buffer'.")

(defvar +ivy-edit-functions nil
  "A plist mapping ivy/counsel commands to commands that generate an editable
results buffer.")

;;-- end definitions

;;-- ivy
(setq ivy-height 17
      ivy-wrap t
      ivy-fixed-height-minibuffer t
      ivy-sort-max-size 7500
      ivy-magic-slash-non-match-action nil ;; disable magic slash on non-match
      ivy-use-virtual-buffers nil ;; don't show recent files in switch-buffer
      ivy-virtual-abbreviate 'full ;; ...but if that ever changes, show their full path
      ivy-on-del-error-function #'ignore ;; don't quit minibuffer on delete-error
      ivy-use-selectable-prompt t ;; enable ability to select prompt (alternative to `ivy-immediate-done')
      ivy-rich-parse-remote-buffer nil
      ivy-read-action-function #'ivy-hydra-read-action
      ivy-read-action-format-function #'ivy-read-action-format-columns
      ivy-switch-buffer-faces-alist nil ;; Remove built-in coloring of buffer list; we do our own
      ivy-initial-inputs-alist nil   ;; Don't use ^ as initial input. Set this here because `counsel' defines more of its own, on top of the defaults.
      swiper-action-recenter t
      avy-all-windows t
      )
;;-- end ivy

;;-- company
(setq company-idle-delay 1)
;;-- end company

;;-- specs
(spec-handling-add! popup nil
                    '(ivy
                      ("^\\*ivy-occur" :size 0.35 :ttl 0 :quit nil)
                      )
                    )
;;-- end specs