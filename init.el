;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :completion
       (company +childframe)           ; the ultimate code completion backend
       helm                ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       hydra
       ;;(popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;unicode           ; extended unicode support for various languages
       ;;vc-gutter         ; vcs diff in the fringe
       ;;window-select     ; visually switch windows

       :editor
       ;;file-templates    ; auto-snippets for empty files
       ;;(format +onsave)  ; automated prettiness
       snippets          ; my elves. They type so I don't have to

       :emacs
       ;;dired             ; making dired pretty [functional]
       ;;vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       ;;vterm             ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       lookup              ; navigate your code and its documentation
       lsp
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;tmux              ; an API for interacting with tmux

       :lang
       assembly          ; assembly for fun or debugging
       (cc +lsp)
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;data              ; config/data formats
       ;;emacs-lisp        ; drown in parentheses
       ;;go                ; the hipster dialect
       ;;ledger            ; an accounting system in Emacs
       ;;markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        ;;+dragndrop       ; drag & drop files/images into org buffers
        ;;+hugo            ; use Emacs for hugo blogging
        ;;+jupyter        ; ipython/jupyter support for babel
        ;;+pandoc          ; export-with-pandoc support
        ;;+pomodoro        ; be fruitful with the tomato technique
        ;;+present
        )        ; using org-mode for presentations
       plantuml          ; diagrams for confusing people more
       python       ; beautiful is better than ugly
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                ; she sells {ba,z,fi}sh shells on the C xor

       :config
       ;;literate
       (default +bindings))
