(defvar freest-mode-font-lock-keywords
  '(("\\<receive\\>\\|\\<send\\>\\|\\<match\\>\\|\\<with\\>\\|\\<select\\>\\|\\<fork\\>\\|\\<new\\>\\|\\<rec\\>\\|\\<dualof\\>" . font-lock-keyword-face))
;\\<packet\\>
  )
;\\>|match\\|send\\|select\\|fork
(define-derived-mode freest-mode haskell-mode "FreeST"
  "A major mode to edit FreeST files. (.cfs)"
  (font-lock-add-keywords nil freest-mode-font-lock-keywords))
;(add-to-list 'auto-mode-alist '("\\.receive\\'" . freest-mode))
;(add-to-list 'auto-mode-alist '("\\.match\\'" . freest-mode))
(provide 'freest-mode)


