;;; keys.el --- Global keys

;;; Commentary:

;;; Code:

(map! "C-s"     #'swiper-isearch)
(map! "C-c <"   #'lsp-find-references)
(map! "C-x f"   #'cprg-hydra)
(map! "C-x k"   #'kill-this-buffer)
(map! "C-\\"    #'psv/toggle-russian-input-method)
(map! "C-c d"   #'psv/duplicate-current-line-or-region)
(map! "C-M-="   #'psv/diff-current-buffer-with-file)
(map! "C-x C-j" #'dired-jump)
(map! "M-RET"   #'+company/complete)
(map! "C-;"     #'avy-goto-word-1)
(map! "C-x ="   #'magit-diff-buffer-file)
(map! "M-\\"    #'psv/goto-match-paren)
(map! "C-<f6>"  #'whitespace-mode)
(map! "C-<f12>" #'toggle-truncate-lines)
(map! "<f5>"    #'yb-goto-global-notes)
(map! "C-x t t" #'psv/ert-silently)
(map! "C-M-\\"  #'clang-format-region)

;; Unbound dangerous keys.
(global-unset-key (kbd "C-x C-c"))

;; Unbound "C-x 5 0 <-> O" typo.
(global-unset-key (kbd "C-x 5 0"))

;; Unbound suspend-frame function.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'keys)
;;; keys.el ends here
