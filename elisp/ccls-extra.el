;;; ccls-extra.el --- ccls extra functions

;;; Commentary:

;;; Code:

(defun psv/ccls-peek-references ()
  "Peek references."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"))

(defun psv/ccls-peek-callee ()
  "Peek callee's."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

(defun psv/ccls-peek-caller ()
  "Peek callers."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/call"))

(defun psv/ccls-peek-references-address ()
  "Peek references where we take address."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 128)))

(defun psv/ccls-peek-references-read ()
  "Peek references where we read read variable."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
    (plist-put (lsp--text-document-position-params) :role 8)))

(defun psv/ccls-peek-references-write ()
  "Peek references where we write to variable."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

(defun psv/ccls-peek-references-macro ()
  "Peek references of macro expansion."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

(defun psv/ccls-peek-references-not-call ()
  "Peek non-call references."
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :excludeRole 32)))

(defun psv/ccls-peek-inheritance-base ()
  "Peek base of class."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels 1)))

(defun psv/ccls-peek-inheritance-derived ()
  "Peek derived classes."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels 1 :derived t)))

(defun psv/ccls-tree-inheritance-base ()
  "Show base tree of type."
  (interactive)
  (ccls-inheritance-hierarchy nil))

(defun psv/ccls-tree-inheritance-derived ()
  "Show derived tree of type."
  (interactive)
  (ccls-inheritance-hierarchy t))

(defun psv/ccls-tree-caller ()
  "Show tree of callers."
  (interactive)
  (ccls-call-hierarchy nil))

(defun psv/ccls-tree-callee ()
  "Show tree of callee's."
  (interactive)
  (ccls-call-hierarchy t))

(defun psv/ccls-peek-member ()
  "Peek members of type."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/member"))

(defun psv/ccls-peek-variables ()
  "Peek variables."
  (interactive)
  (lsp-ui-peek-find-custom "$ccls/vars"))

(defhydra psv/hydra-ccls (:hint t)
  "Where?"
  ("c" psv/ccls-peek-caller "peek caller")
  ("C" psv/ccls-tree-caller "tree caller")
  ("r" psv/ccls-peek-references-read "read")
  ("w" psv/ccls-peek-references-write "write")
  ("b" psv/ccls-peek-inheritance-base "peek base")
  ("B" psv/ccls-tree-inheritance-base "tree base")
  ("d" psv/ccls-peek-inheritance-derived "peek derived")
  ("D" psv/ccls-tree-inheritance-derived "tree derived")
  ("m" psv/ccls-peek-member "member")
  ("n" psv/ccls-peek-references-not-call "not call")
  ("a" psv/ccls-peek-references "references")
  ("v" psv/ccls-peek-variables "variables"))

(map! "C-x w" #'psv/hydra-ccls/body)

(provide 'ccls-extra)
;;; ccls-extra.el ends here
