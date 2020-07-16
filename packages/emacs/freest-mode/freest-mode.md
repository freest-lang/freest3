# Emacs syntax highlight mode for FreeST files (.cfs)

## FreeST mode

Copy freest-mode folder to any folder you like (~/.emacs.d/ in this case)
Then add the following lines to your emacs init file:

```
(add-to-list 'load-path "~/.emacs.d/freest-mode/")

(require 'freest-mode)

;; This automatically chooses freest-mode when editing FreeST files.
(add-to-list 'auto-mode-alist '("\\.cfs\\'" . freest-mode)) 
```
