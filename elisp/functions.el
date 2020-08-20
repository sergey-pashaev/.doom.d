;;; functions.el --- Base functions

;;; Commentary:

;;; Code:
(defun psv/get-org-files (dirs)
  "Return list of org files in DIRS for agenda."
  (apply 'append
         (mapcar (lambda (dir)
                   (directory-files-recursively dir org-agenda-file-regexp))
                 dirs)))

(defvar psv/org-agenda-dirs '("~/Yandex.Disk/notes/" "~/org/")
  "List of directories to search org files.")

(defun psv/update-org-agenda-files ()
  "Update `org-agenda-files' with list of org files from DIRS."
  (setq org-agenda-files (psv/get-org-files psv/org-agenda-dirs)))

(defun psv/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun psv/diff-current-buffer-with-file ()
  "Diff current buffer with associated file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun psv/goto-match-paren ()
  "Go to matching bracket if on (){}[], similar to vi-style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun psv/buffer-file-path ()
  "Return current buffer filename or default directory."
  (interactive)
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun psv/put-to-clipboard (str)
  "Put STR into clipboard & kill ring."
  (when str
    (kill-new str)))

(provide 'functions)
;;; functions.el ends here
