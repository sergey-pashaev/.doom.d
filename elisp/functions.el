;;; functions.el --- Base functions

;;; Commentary:

;;; Code:
(defun psv/get-org-files (dirs)
  "Return list of org files in DIRS for agenda."
  (apply 'append
         (mapcar (lambda (dir)
                   (directory-files-recursively dir org-agenda-file-regexp))
                 dirs)))

(defvar psv/org-agenda-dirs '("~/workspace/ya/notes/")
  "List of directories to search org files.")

(defun psv/update-org-agenda-files ()
  "Update `org-agenda-files' with list of org files from DIRS."
  (setq org-agenda-files (psv/get-org-files psv/org-agenda-dirs)))

(defun psv/flush-lines-with-selected-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((str (regexp-quote (buffer-substring-no-properties beg end))))
      (flush-lines str (point-min) (point-max)))))

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

(defun psv/pos-at-line-beg (l)
  "Return end position (point) of line number L."
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (beginning-of-line)
    (point)))

(defun psv/pos-at-line-end (l)
  "Return beginning position (point) of line number L."
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (end-of-line)
    (point)))

(defun psv/hl-regions (regions)
  "Highlight list of REGIONS."
  (dolist (r regions)
    (psv/hl-region (car r) (cadr r))))

(defun psv/hl-regions-clear (regions)
  "Clear highlight of list of REGIONS."
  (dolist (r regions)
    (psv/hl-region-del (car r) (cadr r))))

(defun psv/hl-region (beg end)
  "Highlight region from BEG point to END point."
  (message "hl region lines: %d %d" beg end)
  (let ((overlay (make-overlay
                  (psv/pos-at-line-beg beg)
                  (psv/pos-at-line-end end))))
    (overlay-put overlay 'yb-blame-gap t) ; mark our overlay to distinct from others
    (overlay-put overlay 'face '(:background "pale green"))))

(defun psv/hl-region-del (beg end)
  "Remove all overlays from BEG point to END point."
  (remove-overlays (psv/pos-at-line beg) (psv/pos-at-line end)))

(defun yb-blame-gap-next ()
  "Go to next blame gap region."
  (interactive)
  (let ((p (point))
         (overlays (overlays-in (point-min) (point-max))))
    (catch 'foo
    (dolist (ov overlays)
      (when (and (overlay-get ov 'yb-blame-gap)
                 (> (overlay-start ov) p))
        (goto-char (overlay-start ov))
        (throw 'foo nil))))))

(defun yb-blame-gap-prev ()
  "Go to previous blame gap region."
  (interactive)
  (let ((p (point))
         (overlays (overlays-in (point-min) (point-max))))
    (catch 'foo
    (dolist (ov overlays)
        (when (and (overlay-get ov 'yb-blame-gap)
                   (< (overlay-start ov) p))
          (goto-char (overlay-start ov))
          (throw 'foo nil))))))

(defun yb-blame-gap-init ()
  "Highlight blame gap regions."
  (interactive)
  (let* ((root (magit-toplevel))
        (path (magit-file-relative-name))
        (ranges (shell-command-to-string (format "yb-blame-gap.py %s %s"
                                                 root path))))
    (dolist (r (split-string ranges "\n" t))
      (let ((range (read (concat "(" r ")"))))
        (psv/hl-region (car range) (cadr range))))))

(defun yb-blame-gap-clear ()
  "Remove all blame gap regions."
  (interactive)
    (let ((overlays (overlays-in (point-min) (point-max))))
      (dolist (ov overlays)
          (when (overlay-get ov 'yb-blame-gap)
            (delete-overlay ov)))))

;;;###autoload
(define-minor-mode yb-blame-gap-mode
  "Highlight areas where you can insert code w/o cc."
  :lighter " YBG"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-n") 'yb-blame-gap-next)
            (define-key map (kbd "C-p") 'yb-blame-gap-prev)
            (define-key map (kbd "q") 'yb-blame-gap-clear)
            map)
  (yb-blame-gap-init))

(defun psv/black ()
  "Run black on current file."
  (interactive)
  (let* ((fpath (buffer-file-name))
         (cmd (format "python3 -m black %s" fpath)))
    (compilation-start cmd)))

(defun psv/mypy ()
  "Run mypy on current file."
  (interactive)
  (let* ((fpath (buffer-file-name))
         (cmd (format "python3 -m mypy %s" fpath)))
    (compilation-start cmd)))

(provide 'yb-blame-gap-mode)

(provide 'functions)
;;; functions.el ends here
