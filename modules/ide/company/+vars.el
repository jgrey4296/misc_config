;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-company-active-map (make-sparse-keymap))

(defvar jg-company-search-map (make-sparse-keymap))

(speckler-setq! company ()
  company-minimum-prefix-length 2
  company-idle-delay 1
  company-tooltip-limit 14
  company-tooltip-align-annotations t
  company-require-match 'never
  company-selection-wrap-around t
  company-global-modes '(not erc-mode circe-mode message-mode help-mode gud-mode vterm-mode)
  company-frontends
  '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
    company-echo-metadata-frontend)  ; show selected candidate docs in echo area

  ;; Buffer-local backends will be computed when loading a major mode, so
  ;; only specify a global default here.
  company-backends '(company-capf)

  ;; These auto-complete the current selection when
  ;; `company-auto-commit-chars' is typed. This is too magical. We
  ;; already have the much more explicit RET and TAB.
  company-insertion-on-trigger nil

  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  company-dabbrev-other-buffers nil
  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  company-dabbrev-ignore-case   nil
  company-dabbrev-downcase      nil

  company-dict-dir (expand-file-name "company-dicts" templates-loc)
  )

(after! company-files
  ;; Fix `company-files' completion for org file:* links
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)")

  )

(speckler-add! company ()
  '(prog-mode (:mode company-dabbrev-code company-capf) (:disfavour company-keywords company-abbrev) (:back company-files))
  '(text-mode (:mode company-dabbrev company-ispell))
  '(conf-mode (:mode company-dabbrev-code company-shell-env))
  '(minibuffer-inactive-mode (:mode company-dabbrev-code company-capf) (:back company-files))
  '(minibuffer-mode (:mode company-dabbrev-code company-capf) (:back company-files))
  )
