;;; +vars.el -*- lexical-binding: t; -*-

(setq scala-indent:align-parameters t
      ;; indent block comments to first asterix, not second
      scala-indent:use-javadoc-style t)

(spec-handling-add! lookup-regular nil
                    (scala-mode
                     ("Scala Documentation" . "https://docs.scala-lang.org/")
                     ("Scala API" . "https://docs.scala-lang.org/api/all.html")
                     ("Scala Cheatsheet" . "https://docs.scala-lang.org/cheatsheets/index.html")
                     ("Scala Language Reference" . "https://docs.scala-lang.org/scala3/reference/")
                     ("Scala Language Spec" . "https://scala-lang.org/files/archive/spec/2.13/")
                     ("SBT Reference" . "https://www.scala-sbt.org/1.x/docs/index.html")
                     ("Scala LSP" . "https://scalameta.org/metals/docs/")
                    )
