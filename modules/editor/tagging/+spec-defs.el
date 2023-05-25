;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new-hooks! tagging
                          (setq-local tagging-minor-mode-handlers
                                      (list :new (plist-get val :new)
                                            :set (plist-get val :set)
                                            :get (plist-get val :get)
                                            :buff (plist-get val :buff)
                                            )
                                      )
                          )
