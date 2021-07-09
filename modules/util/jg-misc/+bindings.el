;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up jg-misc bindings: %s" (current-time-string))
(map! :leader
      :desc "Have you Played?" "o h h" #'+jg-misc-helm-rps-have-you-playeds
      )

(evil-make-intercept-map messages-buffer-mode-map)

(map! :map help-map
      "DEL" #'free-keys
      )

(defun +jg-misc-free-key-binding-update ()
  (map! :map free-keys-mode-map
        :desc "Change Buffer" :n "b" #'free-keys-change-buffer
        :desc "Revert Buffer" :n "g" #'revert-buffer
        :desc "Describe Mode" :n "h" #'describe-mode
        :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
        :desc "Quit"          :n "q" #'quit-window
        )
  (evil-make-intercept-map free-keys-mode-map)
  )

(map! :map (sh-mode-map shell-mode-map)
      :localleader
      :desc "Docs: Brew"  "1" (cmd! (+jg-browse-url "https://brew.sh/"))
      :desc "Docs: Awk"   "2" (cmd! (+jg-browse-url "https://www.gnu.org/software/gawk/manual/gawk.html"))
      )

(map! :map csharp-mode-map
      :localleader
      :desc "Docs: Unity"    "1" (cmd! (+jg-browse-url "https://docs.unity3d.com/Manual/index.html"))
      :desc "Docs: Unity C#" "2" (cmd! (+jg-browse-url "https://docs.unity3d.com/ScriptReference/index.html"))
      :desc "Docs: Mono"     "3" (cmd! (+jg-browse-url "http://docs.go-mono.com/"))
      :desc "Docs: MS C#"    "4" (cmd! (+jg-browse-url "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/"))
)

(map! :map fsharp-mode-map
      :localleader
      :desc "Docs: F#"       "1" (cmd! (+jg-browse-url "https://docs.microsoft.com/en-us/dotnet/fsharp/"))
      )
