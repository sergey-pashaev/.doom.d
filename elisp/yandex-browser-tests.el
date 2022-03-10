;;; yandex-browser-tests.el --- Unit tests for yandex-browser module.

;;; Commentary:
;;; Suitable for interactive runs only, due to complexity of loading all needed
;;; dependencies to run it with clean "emacs -Q".

;;; Code:
(require 'ert)
; (require 'yandex-browser)

(defmacro with-cwd (cwd &rest body)
  "Evaluate BODY with `default-directory' setted to CWD, restore it after eval."
  `(let ((old-cwd default-directory))
         (cd ,cwd)
       (let ((result ,@body))
         (cd old-cwd)
         result)))

(ert-deftest xxx-2 ()
  "Test `yb-org-link' formatted output."
  (should (equal (yb-org-link "abc" "123") "[[abc][123]]"))
  (should (equal (yb-search-term "term") "dGVybQ=="))
  (should (equal (yb-what-project (expand-file-name "~/workspace/ya/browser1/")) 'yandex-browser))
  (should (equal (yb-what-project (expand-file-name "~/workspace/ya/chromium/src/")) 'chromium))
  (should (equal (with-cwd (expand-file-name "~/workspace/ya/browser1/") (yb-read-branch)) "master"))
  (should (equal (with-cwd (expand-file-name "~/workspace/ya/browser2/") (yb-read-branch)) "wp/BROWSER-131080/1"))
  (should (equal (with-cwd (expand-file-name "~/workspace/ya/browser3/") (yb-read-branch)) "master-21.3.0/rc"))
  )

(defun yb-repo (n)
  "Return expanded path to browser repo N."
  (expand-file-name (format "~/workspace/ya/browser%d/" n)))

(ert-deftest test-yb-guess-ticket-bug ()
  (should (equal (with-cwd (yb-repo 1) (yb-guess-ticket)) ""))
  (should (equal (with-cwd (yb-repo 2) (yb-guess-ticket)) "BROWSER-131080"))
  (should (equal (with-cwd (yb-repo 3) (yb-guess-ticket)) "master-21"))
  )

(provide 'yandex-browser-tests)
;;; yandex-browser-tests.el ends here
