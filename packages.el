;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! google-translate)
(package! clang-format)
(package! cprg :recipe (:host github :repo "sergey-pashaev/cprg"))
(package! rmsbolt)
(package! helm-dash)

(package! undo-tree :disable t)
(package! volatile-highlights :disable t)
(package! dtrt-indent :disable t)
(package! goto-addr :disable t)
(package! highlight-numbers :disable t)
(package! rainbow-delimeters :disable t)
(package! forge :disable t)
(package! github-review :disable t)
(package! magit-todos :disable t)
(package! magit-gitflow :disable t)
(package! evil-magit :disable t)
(package! auto-yasnippet :disable t)
(package! highlight-quoted :disable t)
(package! macrostep :disable t)
(package! overseer :disable t)
(package! elisp-demos :disable t)
(package! flycheck-cask :disable t)
(package! markdown-toc :disable t)
(package! edit-indirect :disable t)
(package! grip-mode :disable t)
(package! evil-markdown :disable t)
(package! modern-c++-font-lock :disable t)
(package! demangle-mode :disable t)
(package! company-glsl :disable t)
(package! cuda-mode :disable t)
(package! disaster :disable t)
(package! opencl-mode :disable t)
(package! helm-org :disable t)
(package! helm-flx :disable t)
(package! helm-describe-modes :disable t)
(package! helm-company :disable t)
(package! helm-c-yasnippet :disable t)
(package! irony-eldoc :disable t)
(package! flycheck-irony :disable t)
(package! company-irony :disable t)
(package! company-irony-c-headers :disable t)
(package! irony :disable t)
(package! helm-rtags :disable t)
(package! ivy-rtags :disable t)
(package! rtags :disable t)
(package! company-dict :disable t)
(package! mips-mode :disable t)
(package! haxor-mode :disable t)
