;;; $DOOMDIR/config.el --- Private configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(setq user-mail-address "pashaev.sergey@gmail.com"
      user-full-name "Sergey Pashaev")

(setq doom-font (font-spec :family "Liberation Mono" :size 14))
(setq doom-theme 'doom-solarized-light)
(setq doom-inhibit-indent-detection t)

(setq org-directory "~/org/")

;; Don't display line numbers.
(setq display-line-numbers-type nil)

;; Use decimal, not octal.
(setq read-quoted-char-radix 10)

;; Guess target directory.
(setq dired-dwim-target t)

;; Show all files with human readable sizes.
(setq dired-listing-switches "-alhG") ; --group-directories-first

;; Stop asking whether to save newly added abbrev when quitting emacs.
(setq save-abbrevs nil)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; Change the recentering order.
(setq recenter-positions '(top middle bottom))

;; Show some lines below "last" line before start scrolling.
(setq scroll-margin 5)

;; Show buffer filepath at frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Push some limits
(setq save-place-limit 500)
(setq recentf-max-saved-items 1000)

;;; Load external files.
(load! "elisp/functions")
(load! "elisp/keys")
(load! "elisp/ccls-extra")
(load! "elisp/yandex-browser")

;; (psv/update-org-agenda-files)

;;; Packages configurations:

;; Trim whitespace only in changed regions.
(use-package ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point nil))


(use-package google-translate
  :init
  (progn
    (require 'google-translate-default-ui)
    (setq google-translate-output-destination 'echo-area)
    (setq google-translate-default-source-language "auto")
    (setq google-translate-default-target-language "ru"))
    (setq google-translate-backend-method 'curl))


(use-package helm
  :config
  (setq helm-split-window-inside-p t)
  (setq helm-buffer-max-length 60))


(use-package ivy
  :config
  (setq ivy-height 10)) ; for `swiper-isearch'


(use-package windmove
  :init
  (progn
    (require 'windmove)
    (setq windmove-wrap-around t)
    (windmove-default-keybindings 'meta)))


(use-package cprg
  :init
  (require 'cprg)
  (cprg-set-globs "_c_++"          '("*.h" "*.c" "*.cc" "*.cpp" "*.mm" "*.m"))
  (cprg-set-globs "_h_eaders"      '("*.h"))
  (cprg-set-globs "_t_ests"        '("*test.cc" "*tests.cc"))
  (cprg-set-globs "_u_nittests"    '("*unittest.cc" "*unittests.cc"))
  (cprg-set-globs "_m_ojom"        '("*.mojom"))
  (cprg-set-globs "_b_uild"        '("*.gn" "*.gni" "*.grd" "*.grdp" "*.spec" "DEPS"))
  (cprg-set-globs "_y_aml"         '("*.yaml" "*.yml"))
  (cprg-set-globs "_j_ava"         '("*.java" "*.kt"))
  (cprg-set-globs "_p_ython"       '("*.py"))
  (cprg-set-globs "_e_lisp"        '("*.el"))
  (cprg-set-globs "_x_ml"          '("*.xml"))
  (cprg-set-globs "rea_d_me"       '("*.md"))
  (cprg-set-globs "js_o_n"         '("*.json"))
  (cprg-set-globs "_w_eb"          '("*.html" "*.js" "*.css"))
  (cprg-set-globs "_g_radle"       '("*.gradle"))
  (cprg-set-globs "_f_eatures"     '("*features.cc" "*switches.cc" "*features.h" "*switches.h"))
  (cprg-load-hydra))


;;; Common minor modes:
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode t) ; subtle highlighting of matching parens
(delete-selection-mode -1) ; don't delete the selection with a keypress


;; russian input indication
(defun psv/update-cursor-color ()
  "Change cursor color with keyboard layout change."
  (set-cursor-color (if current-input-method "red" "black")))

(when (display-graphic-p)
  (add-hook 'post-command-hook 'psv/update-cursor-color))

(defun psv/toggle-russian-input-method ()
  "Toggle internal input method between default and russian."
  (interactive)
  (if (string= current-input-method "russian-computer")
      (deactivate-input-method)
    (set-input-method "russian-computer")))


;; Browse with yandex-browser
(defconst *psv/yandex-browser-program* "yandex-browser")
(defconst *psv/browser-url-yandex-browser-arguments* nil)

(defun psv/browse-url-yandex-browser (url &optional _new-window)
  "Ask the Yandex Browser WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `*psv/browser-url-yandex-browser-arguments*' are also
passed to browser.  The optional argument NEW-WINDOW is not
used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat *psv/yandex-browser-program* " " url)
           nil
           *psv/yandex-browser-program*
           (append
            *psv/browser-url-yandex-browser-arguments*
            (list url)))))

(setq browse-url-browser-function 'psv/browse-url-yandex-browser)

;; Tune lsp
(setq lsp-auto-guess-root nil)
(setq lsp-keep-workspace-alive t)
(setq lsp-enable-semantic-highlighting nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-prefer-flymake nil)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature nil)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-use-childframe nil)
(setq lsp-ui-flycheck-live-reporting nil)
(setq lsp-ui-peek-always-show t)
(setq lsp-ui-peek-list-width 60)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-symbol nil)


;; Tune company
(setq company-minimum-prefix-length 3)
(setq company-idle-delay nil) ; disable idle completion


;; "Disable" smartparens
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(map! :after smartparens
      :map smartparens-mode-map
      [M-right] nil
      [M-left] nil
      [C-right] nil
      [C-left] nil)

;; Tune org-mode for speed
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#why-is-emacsdoom-slow
(remove-hook 'org-mode-hook #'org-superstar-mode)
(after! org
  (setq org-fontify-quote-and-verse-blocks nil
        org-fontify-whole-heading-line nil
        org-hide-leading-stars nil
        org-startup-indented nil))

;; Disable modern-c++-font-lock-mode
(remove-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; dash
(defun psv/helm-dash-cpp-doc ()
  "Enable C++ dash docset for c++ buffers."
  (interactive)
  (setq-local helm-dash-docsets '("C++")))

(defun psv/helm-dash-python-doc ()
  "Enable python3 dash docset for python buffers."
  (interactive)
  (setq-local helm-dash-docsets '("Python 3")))

(defun psv/helm-dash-bash-doc ()
  "Enable bash dash docset for shell buffers."
  (interactive)
  (setq-local helm-dash-docsets '("Bash")))

(defconst psv/helm-dash-docsets '("C++" "Python 3" "Bash")
  "My default docset list.")

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-common-docsets psv/helm-dash-docsets)
  (add-hook 'c++-mode-hook 'psv/helm-dash-cpp-doc)
  (add-hook 'python-mode-hook 'psv/helm-dash-python-doc)
  (add-hook 'sh-mode-hook 'psv/helm-dash-bash-doc)
  :bind
  ("C-c ?" . helm-dash-at-point))

;; c++ style
(defconst psv/cc-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(c-add-style "psv/cc-mode" psv/cc-style)

(require 'cc-vars)
(setq c-default-style "psv/cc-mode"
      c-basic-offset 2)

;; python
(setq py-autopep8-options '("--max-line-length=79"))

;; clipetty
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; ERT
(require 'ert)

(defun psv/ert-silently ()
  (interactive)
  (ert t))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq comint-buffer-maximum-size 20480)

(use-package unicode-fonts
  :ensure t
  :init
  (unicode-fonts-setup))
