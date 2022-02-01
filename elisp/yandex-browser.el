;;; yandex-browser.el --- Dev tools for chromium, yandex browser project

;;; Commentary:

;;; Code:
(require 'projectile)
(require 'f)
(require 'cl-lib)
(require 'url-util)
(require 'subr-x)
(require 'magit)

(require 'ox-md)

(org-export-define-derived-backend 'st 'md
  :menu-entry
  '(?y "Export to Startrek"
       ((?y "To temporary buffer"
            (lambda (a s v b) (ox-st-export-as-st a s v)))))
  :translate-alist
  '(
    (link . ox-st-link)
    (headline . ox-st-headline)
    (code . ox-st-verbatim)
    (verbatim . ox-st-verbatim)
    (example-block . ox-st-example-block)
    (src-block . ox-st-example-block)
    ))

(defun ox-st-export-as-st (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'st "*Org ST: export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun ox-st-example-block (block desc info)
 (let ((lang (org-element-property :language block))
        (body (org-remove-indentation
               (org-export-format-code-default block info))))
   (format "%%%%%s\n%s%%%%" (if lang (concat "(" lang ")") "") body)))

(defun ox-st-verbatim (verbatim _contents _info)
  (let ((value (org-element-property :value verbatim)))
    (format "##%s##" value)))

(defconst ox-st-link-rx "(yb-goto-yb-link \"\\(.*\\)\")")

(defconst ox-st-project-url "https://bitbucket.browser.yandex-team.ru/projects/STARDUST/repos/browser")
(defconst ox-st-line-reference-url-format "((%s/browse/src/%s?at=refs%%2Fheads%%2F%s#%s %s))"
  "Line reference in repo browser url format.
1. project url string
2. filepath string
3. branch string
4. line number
5. desc")

(defun ox-st-headline (headline desc info)
  (let ((title (org-element-property :raw-value headline))
        (level (org-element-property :level headline)))
    (concat (format "===== %s\n" title) desc)))

(defun ox-st-link (link desc info)
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (desc (if desc desc "")))
    (cond
     ((member type '("http" "https"))
      (format "((%s:%s %s))" type raw-path desc))
     ((member type '("elisp"))
      (if (and (string= type "elisp")
               (string-match ox-st-link-rx raw-path))
          (let* ((parts (s-split ":" (substring raw-path 18 -2)))
                 (path (nth 0 parts))
                 (line (nth 1 parts))
                 (branch (nth 2 parts))
                 (search-term (base64-decode-string (nth 3 parts))))
            (format ox-st-line-reference-url-format
                    ox-st-project-url
                    path
                    (url-hexify-string branch)
                    line
                    search-term
                    )))))))

(provide 'ox-st)

;; common
(defun yb-get-branch ()
  "Return current git branch."
  (magit-get-current-branch))

(defun yb-read-branch ()
  "Return current git branch.
Read branch name from minibuffer if called with prefix argument."
  (if current-prefix-arg
      (read-from-minibuffer "Branch: " (magit-get-current-branch))
    (magit-get-current-branch)))

(defun yb-read-symbol ()
  "Read symbol name from minibuffer."
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (read-string "Symbol: " symbol))))

(defun yb-symbol-or-line ()
  "Return symbol or line."
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        symbol
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(defun yb-buffer-path ()
  "Return current buffer filename or default directory."
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun yb-put-to-clipboard (str)
  "Put STR into clipboard & kill ring."
  (when str
    (kill-new str)))

(defun yb-org-link (url text)
  "Return org link to URL with TEXT."
  (format "[[%s][%s]]" url text))

(defun yb-search-term (term)
  "Encode search TERM for yb link into base64."
  (interactive)
  (base64-encode-string term t))

(defun yb-org-roam-link (url line-num branch &optional desc)
  "Return org-roam link to URL with optional DESC."
  (if desc
      (format "[[yb:%s:%d:%s:%s][%s]]" url line-num branch (yb-search-term desc) desc)
    (format "[[yb:%s:%d:%s]]" url line-num branch)))

(defun yb-what-project (&optional root)
  "Return type of browser project for ROOT path.
Returns 'chromium, 'yandex-browser or nil if other."
  (let ((path (if root
                  root
                (projectile-project-root))))
    (cond
     ((chromium-project-path-p path)
      'chromium)
     ((yb-project-path-p path)
      'yandex-browser)
     (t
      nil))))

(defun yb-complete-dir (dir prompt)
  "Complete dir name inside DIR with PROMPT."
  (let ((dir-contents (directory-files dir)))
    (ido-completing-read (concat prompt " ")
                         (cl-remove-if-not (lambda (name)
                                             (file-directory-p (concat dir name)))
                                           dir-contents))))

;; code search
(defconst chromium-repo-path (expand-file-name "~/workspace/ya/chromium/src/")
  "Chromium project root path.")

(defconst chromium-cs-url-format
  "https://cs.chromium.org/search/?q=%s&sq=package:chromium&type=cs"
  "Chromium code search format, %s = SEARCH TERM.")

(defconst chromium-cs-symbol-reference-url-format
  "https://source.chromium.org/chromium/chromium/src/+/%s:%s;?q=%s&ss=chromium"
  "Chromium code search symbol reference format.")

(defconst chromium-cs-line-reference-url-format
  "https://source.chromium.org/chromium/chromium/src/+/%s:%s;l=%d?ss=chromium"
  "Chromium code search line reference format.")

(defun chromium-make-symbol-reference-url (branch filepath line symbol)
  "Construct url to SYMBOL at FILEPATH at BRANCH."
  (if symbol
      (format chromium-cs-symbol-reference-url-format branch filepath symbol)
    (format chromium-cs-line-reference-url-format branch filepath line)))

(defun chromium-project-path-p (path)
  "Whether given PATH is chromium project path."
  (string-prefix-p chromium-repo-path (expand-file-name path)))

(defun chromium-buffer-relative-path ()
  "Return current buffer path relative to chromium project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (chromium-project-path-p root))
        (let* ((abs-path (yb-buffer-path))
               (rel-path (substring abs-path (length root))))
          (if (s-starts-with? "src/" rel-path)
              (substring rel-path (length "src/"))
            rel-path))
      (user-error "Not in chromium project"))))

(defun chromium-symbol-reference ()
  "Return reference to current symbol as url."
  (interactive)
  (chromium-make-symbol-reference-url (yb-read-branch)
                                      (chromium-buffer-relative-path)
                                      (line-number-at-pos (point))
                                      (yb-read-symbol)))

(defun chromium-copy-symbol-reference ()
  "Copy current symbol reference to kill ring."
  (interactive)
  (let ((url (chromium-symbol-reference)))
    (yb-put-to-clipboard url)
    (message "Chromium url copied.")))

(defun chromium-copy-symbol-reference-org ()
  "Copy current symbol reference to kill ring as org link."
  (interactive)
  (let ((text (thing-at-point 'symbol))
        (url (chromium-symbol-reference)))
    (yb-put-to-clipboard (yb-org-link url text))
    (message "Chromium url for org-mode copied.")))

(defun chromium-copy-file-reference-roam ()
  "Copy current file reference to kill ring as org-roam link."
  (interactive)
  (let* ((line-num (line-number-at-pos (point)))
         (branch (yb-get-branch))
         (url (yb-org-roam-link (chromium-buffer-relative-path) line-num branch)))
    (yb-put-to-clipboard url)
    (message "%s for org-roam copied." url)))

(defun chromium-browse-symbol-reference ()
  "Open current symbol in browser."
  (interactive)
  (browse-url-default-browser (chromium-symbol-reference)))

;; line reference
(defconst yb-project-url "https://bitbucket.browser.yandex-team.ru/projects/STARDUST/repos/browser")

(defconst yb-repo-path (expand-file-name "~/workspace/ya/browser"))

(defconst yb-repo-line-reference-url-format "%s/browse/%s?at=refs%%2Fheads%%2F%s#%d"
  "Line reference in repo browser url format.
1. project url string
2. filepath string
3. branch string
4. line number")

(defun yb-make-line-reference-url (branch filepath line)
  "Construct url to LINE at FILEPATH at BRANCH."
  (format yb-repo-line-reference-url-format
          yb-project-url
          filepath
          (url-hexify-string branch)
          line))

(defun yb-project-path-p (path)
  "Whether given PATH is yandex-browser project path."
  (string-prefix-p yb-repo-path (expand-file-name path) t))

(defun yb-buffer-relative-path ()
  "Return current buffer path relative to browser project root."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium)
      (chromium-buffer-relative-path))
     ((eq project 'yandex-browser)
      (yb-yandex-buffer-relative-path)))))

(defun yb-yandex-buffer-relative-path ()
  "Return current buffer path relative to browser project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (yb-project-path-p root))
        (let* ((abs-path (yb-buffer-path))
               (rel-path (substring abs-path (length root))))
          rel-path)
      (user-error "Not in yandex-browser project"))))

(defun yb-line-reference ()
  "Return reference to current line as url."
  (interactive)
  (yb-make-line-reference-url (yb-read-branch)
                              (yb-yandex-buffer-relative-path)
                              (line-number-at-pos (point))))

(defun yb-copy-line-reference ()
  "Copy current line reference to kill ring."
  (interactive)
  (let ((url (yb-line-reference)))
    (yb-put-to-clipboard url)
    (message "Url copied.")))

(defun yb-copy-line-reference-org ()
  "Copy current line reference to kill ring as org link."
  (interactive)
  (let ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (url (yb-line-reference)))
    (yb-put-to-clipboard (yb-org-link url text))
    (message "Url for org-mode copied.")))

(defun yb-reference-roam-desc ()
  "Return current line or selected region text w/o properties."
  (interactive)
  (s-trim  (if (use-region-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (buffer-substring-no-properties (line-beginning-position)
                                             (line-end-position)))))

(defun yb-copy-file-reference-roam ()
  "Copy current file reference to kill ring as org-roam link."
  (interactive)
  (let* ((rel-path (yb-yandex-buffer-relative-path))
         (path (if (s-starts-with? "src/" rel-path)
                   (substring rel-path (length "src/"))
                 rel-path))
         (line-num (line-number-at-pos (point)))
         (branch (yb-get-branch))
         (url (yb-org-roam-link path line-num branch (yb-reference-roam-desc))))
    (yb-put-to-clipboard url)
    (message "%s for org-roam copied." url)))

(defun yb-browse-line-reference ()
  "Open current line in browser."
  (interactive)
  (browse-url-default-browser (yb-line-reference)))

(defun yb-copy-reference ()
  "Copy current line/symbol reference to kill ring."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium)
      (chromium-copy-symbol-reference))
     ((eq project 'yandex-browser)
      (yb-copy-line-reference)))))

(defun yb-copy-reference-roam()
  "Copy current file reference to kill ring as org-roam link."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium) (chromium-copy-file-reference-roam))
     ((eq project 'yandex-browser) (yb-copy-file-reference-roam)))))

(defun yb-copy-reference-org ()
  "Copy current line/symbol reference to kill ring as org link."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium) (chromium-copy-symbol-reference-org))
     ((eq project 'yandex-browser) (yb-copy-line-reference-org)))))

(defun yb-browse-reference ()
  "Browse current line/symbol at repo web interface."
  (interactive)
  (let ((project (yb-what-project)))
    (cond
     ((eq project 'chromium) (chromium-browse-symbol-reference))
     ((eq project 'yandex-browser) (yb-browse-line-reference)))))

(defun yb-copy-reference-x ()
  "Defun eXtra url copy."
  (interactive)
  (progn
    (yb-copy-reference-roam)
    (let ((roam-url (current-kill 0 t)))
      (yb-copy-reference-org)
      (let ((url (current-kill 0 t)))
        (yb-put-to-clipboard (format "- bb:    %s\n- local: %s\n" url roam-url))))))

(defhydra yb-reference-hydra (:hint t)
  "Current line operations"
  ("r" yb-copy-reference-roam "copy url for org-roam")
  ("c" yb-copy-reference "copy url")
  ("o" yb-copy-reference-org "copy url for org-mode")
  ("g" yb-browse-reference "open url in browser")
  ("x" yb-copy-reference-x "eXtra copy")
  )

;; yandex buttons
(defconst yb-tracker-format "https://st.yandex-team.ru/%s"
  "Ticket URL format.
1. %s = ticket id")

(defconst yb-bitbucket-pr-format
  "https://bitbucket.browser.yandex-team.ru/projects/STARDUST/repos/browser/pull-requests/%s"
  "Bitbucket pull request URL format.
1. %s = pull request number")

(defun yb-browse-button-url (button)
  "Browse BUTTON."
  (browse-url (button-get button 'url)))

(defun yb-make-browse-button (beg end url)
  "Make button with URL from BEG to END positions in buffer."
  (make-button beg end
               'action 'yb-browse-button-url
               'follow-link t
               'url url
               'help-echo url))

(defun yb-match-browse-button (match url)
  "Make button with URL out of MATCH."
  (yb-make-browse-button (match-beginning match) (match-end match) url))

(defun yb-make-browse-buttons (beg end)
  "Add buttons for supported services in region from BEG to END."
  (interactive "r")
  ;; StarTrek
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil))
      (while (re-search-forward "[A-Z]\\{2,\\}-[0-9]+" end t)
        (let ((url (format yb-tracker-format (match-string 0))))
          (yb-match-browse-button 0 url)))))
  ;; pull requests
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil))
      (while (re-search-forward "pull request #\\([0-9]+\\)" end t)
        (let ((url (format yb-bitbucket-pr-format (match-string 1))))
          (yb-match-browse-button 0 url))))))

;;;###autoload
(defun magit-insert-revision-message--yandex-buttons (f &rest args)
  (let ((beg (point)))
    (apply f args)
    (yb-make-browse-buttons beg (point))))

;;;###autoload
(with-eval-after-load 'magit-diff
  (advice-add 'magit-insert-revision-message :around
              'magit-insert-revision-message--yandex-buttons))

(with-eval-after-load 'magit-status
  (advice-add 'magit-insert-revision-message :around
              'magit-insert-revision-message--yandex-buttons))

;; gn refs buffer
(defconst yb-notes-path (expand-file-name "~/workspace/ya/notes/"))
(defconst yb-depot-tools-path (expand-file-name "~/workspace/ya/depot_tools/"))
(defconst yb-gn-path (expand-file-name "gn" yb-depot-tools-path))
(defconst yb-chromium-depot-tools-path (expand-file-name "~/workspace/ya/chromium_depot_tools/"))
(defconst yb-chromium-gn-path (expand-file-name "gn" yb-chromium-depot-tools-path))
(defconst yb-gn-refs-buffer-name "*yb-gn-refs*")
(defconst yb-yakuza-path (expand-file-name "yakuza" yb-depot-tools-path))

(defvar yb-current-build-profile "")

(defun yb-build-command (build-profile target)
  "Return build command for TARGET in BUILD-PROFILE."
  (format "ninja -C %s -j 50 %s" build-profile target))

(defun yb-build-target (build-profile target)
  "Compile TARGET for BUILD-PROFILE."
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "src/")))
    (compilation-start (yb-build-command build-profile target))))

(define-compilation-mode gn-refs-mode "gn-refs"
  (set (make-local-variable 'compilation-disable-input) t)
  (add-hook 'compilation-filter-hook 'yb-gn-refs-filter nil t))

(defun yb-gn-refs-filter ()
  "Handle match highlighting escape sequences inserted by the grep process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight target matches.
        (while (re-search-forward "//\\([a-zA-Z_/]+\\)?:\\([a-zA-Z_]+\\)" (point-max) t)
            (let* ((component (match-string 1))
                   (binary (match-string 2))
                   (target (if component
                               (format "%s:%s" component binary)
                             binary)))
              (yb-gn-refs-match-button 0 (yb-build-command yb-current-build-profile target)
                                       (concat (projectile-project-root) "src/"))
              (cl-incf grep-num-matches-found)))))))

(defun yb-gn-refs ()
  "Run gn refs for current source file.
List all gn refs that using current file in *yb-gn-refs* buffer."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (yb-project-path-p root))
        (let* ((file (psv/buffer-file-path))
               (dir (concat root "src/"))
               (build-dir (substring (yb-select-build-profile) (length dir)))
               (default-directory dir) ; used by process as default directory
               (cmd (format "%s refs %s --all %s"
                            yb-gn-path
                            build-dir
                            file)))
          (setq yb-current-build-profile build-dir)
          (compilation-start cmd 'gn-refs-mode))
      (user-error "Not in yandex-browser project"))))

(defun yb-gn-refs-button-action (button)
  "Action function for BUTTON in gn-refs buffer."
  (psv/put-to-clipboard (button-get button 'cmd))
  (message "Copied: %s for %s"
           (button-get button 'cmd)
           (button-get button 'dir))
  ;(yb-build-target (button-get button 'cmd))
  )

(defun yb-gn-refs-make-button (beg end cmd dir)
  "Make button from BEG to END with action w/ CMD & DIR."
  (make-button beg end
               'action 'yb-gn-refs-button-action
               'follow-link t
               'cmd cmd
               'dir dir
               'help-echo cmd))

(defun yb-gn-refs-match-button (match cmd dir)
  "Create button out of MATCH with given CMD & DIR as action."
  (yb-gn-refs-make-button (match-beginning match) (match-end match) cmd dir))

;; yb trace
(defconst yb-trace-buffer-name "*yb-trace*")
(defvar yb-trace-frames '() "Currently collected trace frames.")

(cl-defstruct (yb-trace-frame
               (:constructor yb-trace-make-frame
                             (project-type
                              project-root
                              filepath
                              line-text
                              line-num
                              &optional
                              branch
                              note
                              symbol)))
  "Trace frame struct."
  project-type
  project-root
  filepath
  line-text
  line-num
  branch
  note
  symbol)

(defun yb-trace-get-frame ()
  "Return yb-trace-frame for current line."
  (let ((project-type (yb-what-project))
        (project-root (projectile-project-root))
        (filepath (yb-buffer-relative-path))
        (line-text (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
        (line-num (line-number-at-pos (point)))
        (cur-symbol
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol))))
    (yb-trace-make-frame
     project-type
     project-root
     filepath
     line-text
     line-num
     (yb-get-branch)
     nil
     cur-symbol)))

(defun yb-trace-make-link (frame)
  "Convert FRAME struct into `org-mode' link."
  (format "** [[%s][|YANDEX|]] [[%s][|CHROMIUM|]] [[file:%s::%s][%s]]\n"
          ;; remote yandex link
          (yb-make-line-reference-url
           (yb-trace-frame-branch frame)
           (yb-trace-frame-filepath frame)
           (yb-trace-frame-line-num frame))
          ;; remote chromium link
          (chromium-make-symbol-reference-url
           "master" ;; todo: fix it somehow
           (substring (yb-trace-frame-filepath frame) (length "src/"))
           (yb-trace-frame-line-num frame)
           (yb-trace-frame-symbol frame)
           )
          ;; local link
          (concat (yb-trace-frame-project-root frame)
                  (yb-trace-frame-filepath frame))
          (yb-trace-frame-line-text frame)
          ;; local text
          (yb-trace-frame-line-text frame)
          ))

(defun yb-trace-clear ()
  "Clear current trace."
  (interactive)
  (setq yb-trace-frames '()))

(defun yb-trace-start (name)
  "Start tracing with NAME."
  (interactive "s Name of trace: ")
  (if (get-buffer yb-trace-buffer-name)
      (kill-buffer yb-trace-buffer-name))
  (let ((buf (get-buffer-create yb-trace-buffer-name)))
    (with-current-buffer buf
      (org-mode)
      (org-insert-heading)
      (insert (format "%s\n" name)))))

;; todo: what to do with local uncommited changes in local branch?
(defun yb-trace-add ()
  "Add current line reference to trace buffer."
  (interactive)
  (let ((buf (get-buffer yb-trace-buffer-name))
        (frame (yb-trace-get-frame)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (yb-trace-make-link frame))
        (goto-char (point-max))))))

(defun yb-trace-copy-org-reference ()
  "Copy to clipboard org link to traced point."
  (interactive)
  (let ((link (yb-trace-make-link (yb-trace-get-frame))))
    (yb-put-to-clipboard link)
    (message "Copied: %s" link)))

(defhydra yb-trace-action-hydra (:hint t)
  "trace actions"
  ("b" yb-trace-start "begin")
  ("a" yb-trace-add "add")
  ("c" yb-trace-clear "clear")
  ("t" yb-trace-copy-org-reference "copy frame trace ref"))

;; include statement
(defun yb-buffer-relative-path-include ()
  "Return buffer relative path (and cut \"src/\" if needed."
  (let ((project (yb-what-project)))
    (cond ((eq project 'yandex-browser)
           (substring (yb-buffer-relative-path)
                      (length "src/")))
          ((eq project 'chromium)
           (chromium-buffer-relative-path)))))

(defun yb-make-include-statement ()
  "Generate include statement for current file."
  (format "#include \"%s\"" (yb-buffer-relative-path-include)))

(defun yb-copy-include-statement ()
  "Put the current file include statement into clipboard."
  (interactive)
  (let ((include (yb-make-include-statement)))
    (yb-put-to-clipboard include)
    (message "Copied: %s" include)))

;; jump to file in other project
(defun yb-select-other-project ()
  "Select other project root."
  (projectile-completing-read
   "Switch to file in project: "
   (projectile-relevant-known-projects)))

(defun yb-select-other-project-file ()
  "Return path to file in other project."
  (let* ((from-project (projectile-project-root))
         (from-project-type (yb-what-project from-project))
         (from-path (yb-buffer-relative-path))
         (to-project (yb-select-other-project))
         (to-project-type (yb-what-project to-project)))
    (cond
     ;; chromium -> yandex-browser (add src/)
     ((and (eq from-project-type 'chromium)
           (eq to-project-type 'yandex-browser))
      (concat to-project "src/" from-path))
     ;; yandex-browser -> chromium (cut src/)
     ((and (eq from-project-type 'yandex-browser)
           (eq to-project-type 'chromium))
      (concat to-project (substring from-path (length "src/"))))
     ;; others -> do nothing
     (t
      (concat to-project from-path)))))

(defun yb-visit-file-other-project ()
  "Visit file in other project with same relative path as current buffer.
With passed universal argument it visits file in other window."
  (interactive)
  (let ((position (point))
        (line-num (line-number-at-pos (point)))
        (from-path (yb-buffer-relative-path))
        (to-path (yb-select-other-project-file)))
    (if (f-exists? to-path)
        (if current-prefix-arg
            ;; visit in other window
            (progn
              (delete-other-windows)
              (split-window-right)
              (other-window 1)
              (find-file to-path)
              (goto-line line-num)
              ;; (goto-char position)
              (recenter-top-bottom)
              (other-window 1))
          ;; visit in current window
          (progn
            (find-file to-path)
            (goto-line line-num)
            ;; (goto-char position)
            (recenter-top-bottom)))
      (user-error (format "file [%s] doesn't exist" to-path)))))

(defun yb-diff-file-other-project ()
  "Diff current file with file on same path in other project."
  (interactive)
  (let ((from-path (yb-buffer-relative-path))
        (from-project (projectile-project-root))
        (to-path (yb-select-other-project-file)))
    (if (f-exists? to-path)
        (ediff (concat from-project from-path) to-path)
      (user-error (format "file [%s] doesn't exist" to-path)))))

(bind-key "C-c >" 'yb-visit-file-other-project)

(defun yb-guess-ticket ()
  "Guess current ticket from branch."
  (interactive)
  (let* ((filename (buffer-name))
         (filepath (yb-buffer-path))
         (branch (yb-get-branch))
         (ticket-rx "[A-Z]\\{2,\\}-[0-9]+"))
    (cond
     ;; try filename first
     ((and filename(string-match ticket-rx filename)) (match-string 0 filename))
     ;; then path to current file
     ((and filepath (string-match ticket-rx filepath)) (match-string 0 filepath))
     ;; then current git branch might help
     ((and (string-match ticket-rx branch)) (match-string 0 branch))
     ;; ask user if nothing found
     (t (let* ((ticket (yb-complete-dir yb-notes-path "Ticket?")))
          ticket)))))

(defun yb-goto-arch-notes ()
  "Go to arch notes."
  (interactive)
  (let ((path (expand-file-name "~/workspace/ya/notes/arch/")))
    (when (not (file-directory-p path))
      (dired-create-directory path))
    (dired path)))

(defun yb-goto-global-notes ()
  "Go to global notes."
  (interactive)
  (let* ((dir yb-notes-path)
         (notes (concat dir "todo.org")))
    (when (not (file-directory-p dir))
      (dired-create-directory dir)
    (when (not (file-exists-p notes))
      (f-write-text "" 'utf-8 notes)
      (psv/update-org-agenda-files))) ; update agenda files once new org file
                                      ; added
    (find-file notes)
    (goto-char (point-max))))

(defun yb-goto-ticket-notes ()
  "Go to ticket notes notes."
  (interactive)
  (let* ((dir yb-notes-path)
         (ticket (yb-guess-ticket))
         (path (concat dir ticket))
         (notes (concat path (format "/%s.org" ticket))))
    (when (not (file-directory-p path))
      (dired-create-directory path)
    (when (not (file-exists-p notes))
      (f-write-text "" 'utf-8 notes)
      (psv/update-org-agenda-files))) ; update agenda files once new org file
                                      ; added
    (find-file notes)
    (goto-char (point-max))))

(defun yb-goto-ticket-tracker ()
  "Go to ticket in tracker."
  (interactive)
  (let* ((ticket (yb-guess-ticket))
         (url (format yb-tracker-format ticket)))
        (browse-url url)))

(defconst yb-wiki-format "https://wiki.yandex-team.ru/users/bioh/browser/notes/%s/"
  "Wiki URL format.
1. %s = ticket id")

(defun yb-goto-ticket-wiki ()
  "Go to ticket in wiki."
  (interactive)
  (let* ((ticket (yb-guess-ticket))
         (url (format yb-wiki-format ticket)))
        (browse-url url)))

;; compile single file
(defun yb-select-build-profile ()
  "Select build profile of current project."
  (interactive)
  (let* ((root (projectile-project-root))
         (out-dir (concat root "src/out/")))
    (if (f-exists? out-dir)
        (concat out-dir (yb-complete-dir out-dir
                                         "Profile:")))))

(defun yb-compile-single-file ()
  "Compile current file."
  (interactive)
  (let* ((build-dir (yb-select-build-profile))
         (path (substring (yb-buffer-relative-path) (length "src/")))
         (cmd (format "%s -i all -k 10000 -C %s ../../%s^"
                      yb-yakuza-path
                      build-dir
                      path)))
    (let ((default-directory (concat (projectile-project-root) "src/")))
      (compilation-start cmd))))

(defun yb-compile-single-file-cmd ()
  "Show current file compilation commands."
  (interactive)
  (let* ((build-dir (yb-select-build-profile))
         (path (substring (yb-buffer-relative-path) (length "src/")))
         (cmd (format "ninja -C %s -t commands ../../%s^" build-dir path)))
    (let ((default-directory (concat (projectile-project-root) "src/")))
      (compilation-start cmd))))

(defvar yb-builds-alist
  '(("Debug.Desktop.32" . "use_distclang=true is_debug=true is_component_build=true enable_nacl=false target_cpu=\"x86\"")
    ("Debug.Desktop.64" . "use_distclang=true is_debug=true is_component_build=true enable_nacl=false target_cpu=\"x64\"")
    ("Release.Desktop.32" . "use_distclang=true is_debug=false is_component_build=true enable_nacl=false target_cpu=\"x86\"")
    ("Release.Desktop.64" . "use_distclang=true is_debug=false is_component_build=true enable_nacl=false target_cpu=\"x64\"")
    ("Debug.Android.Emulator" . "use_distclang=true is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"x86\"")
    ("Release.Android.Emulator" . "use_distclang=true is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"x86\"")
    ("Debug.Android.32" . "use_distclang=true is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm\"")
    ("Debug.Android.64" . "use_distclang=true is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm64\"")
    ("Release.Android.32" . "use_distclang=true is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm\"")
    ("Release.Android.64" . "use_distclang=true is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm64\"")))

(defun yb-prepare-build ()
  "Prepare browser build directory."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (yb-project-path-p root))
        (let* ((file (psv/buffer-file-path))
               (dir (concat root "src/"))
               (build-profile (yb-select-build "Build:"))
               (build-args (cdr (assoc build-profile yb-builds-alist)))
               (default-directory dir) ; used by process as default directory
               (cmd (format "%s gen out/%s --args='%s' --ide=qtcreator --ninja-extra-args=\"-j 50\""
                            yb-gn-path
                            build-profile
                            build-args)))
          (when build-profile
            (compilation-start cmd)))
      (user-error "Not in yandex-browser project"))))

(defun yb-select-build (prompt)
  "Complete build name with PROMPT."
  (let ((builds (mapcar 'car yb-builds-alist)))
    (projectile-completing-read (concat prompt " ") builds)))

(defvar yb-chromium-builds-alist
  '(("Debug.Desktop.32" . "is_debug=true is_component_build=true enable_nacl=false target_cpu=\"x86\"")
    ("Debug.Desktop.64" . "is_debug=true is_component_build=true enable_nacl=false target_cpu=\"x64\"")
    ("Release.Desktop.32" . "is_debug=false is_component_build=true enable_nacl=false target_cpu=\"x86\"")
    ("Release.Desktop.64" . "is_debug=false is_component_build=true enable_nacl=false target_cpu=\"x64\"")
    ("Debug.Android.Emulator" . "is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"x86\"")
    ("Release.Android.Emulator" . "is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"x86\"")
    ("Debug.Android.32" . "is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm\"")
    ("Debug.Android.64" . "is_debug=true is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm64\"")
    ("Release.Android.32" . "is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm\"")
    ("Release.Android.64" . "is_debug=false is_component_build=true enable_nacl=false target_os=\"android\" target_cpu=\"arm64\"")))

(defun yb-prepare-chromium-build ()
  "Prepare browser build directory."
  (interactive)
  (let ((root (projectile-project-root)))
    (if (and root (chromium-project-path-p root))
        (let* ((dir root)
               (build-profile (yb-select-build "Build:"))
               (build-args (cdr (assoc build-profile yb-chromium-builds-alist)))
               (default-directory dir) ; used by process as default directory
               (cmd (format "%s gen out/%s --args='%s' --ide=qtcreator --ninja-extra-args=\"-j 50\""
                            yb-chromium-gn-path
                            build-profile
                            build-args)))
          (when build-profile
            (compilation-start cmd)))
      (user-error "Not in chromium project"))))

(defun yb-select-chromium-build (prompt)
  "Complete build name with PROMPT."
  (let ((builds (mapcar 'car yb-chromium-builds-alist)))
    (projectile-completing-read (concat prompt " ") builds)))

(defun yb-todo ()
  "Go to end of browser todo list."
  (interactive)
  (find-file (concat yb-notes-path "todo.org"))
  (goto-char (point-max)))

(defun yb-move-file-diff ()
  "Move current file diff/patch to same file in other project."
  (interactive)
  (let* ((rel-path (yb-buffer-relative-path))
         (from-project (projectile-project-root))
         (from-path (expand-file-name (concat (file-name-as-directory from-project)
                                              rel-path)))
         (to-project (yb-select-other-project))
         (to-path (expand-file-name (concat (file-name-as-directory to-project)
                                            rel-path))))
    (if (f-exists? to-path)
        (progn
          (message (format "Move diff for [%s] from [%s] to [%s]" rel-path from-project to-project))
          (let ((rc (call-process-shell-command
           (format "yb-move-diff %s %s %s"
                   from-project
                   to-project
                   rel-path)
           nil
           "*yb-move-diff-debug*"
           t)))
            (when (not (eq rc 0))
              (user-error (format "Move diff failed, rc=%s" rc)))))
      (user-error (format "file [%s] doesn't exist" to-path)))))

(defun yb-overwrite-file-diff ()
  "Overwrite current file diff/patch to same file in other project."
  (interactive)
  (let* ((rel-path (yb-buffer-relative-path))
         (from-project (projectile-project-root))
         (from-path (expand-file-name (concat (file-name-as-directory from-project)
                                              rel-path)))
         (to-project (yb-select-other-project))
         (to-path (expand-file-name (concat (file-name-as-directory to-project)
                                            rel-path))))
    (if (f-exists? to-path)
        (progn
          (message (format "Move diff for [%s] from [%s] to [%s]" rel-path from-project to-project))
          (let ((rc (call-process-shell-command
           (format "yb-over-move-diff %s %s %s"
                   from-project
                   to-project
                   rel-path)
           nil
           "*yb-over-move-diff-debug*"
           t)))
            (when (not (eq rc 0))
              (user-error (format "Move diff failed, rc=%s" rc)))))
      (user-error (format "file [%s] doesn't exist" to-path)))))


(defun yb-x-at-point ()
  "Run x at point script."
  (interactive)
  (let* ((abs-path (buffer-file-name))
         (line (line-number-at-pos))
         (column (current-column))
         (location (format "%s:%d:%d" abs-path line column))
         (build-dir (yb-select-build-profile))
         )
    (let ((rc (call-process-shell-command (format "yb x --build-profile %s %s"
                                                  build-dir
                                                  location)
                                          nil
                                          "*yb-x-debug*"
                                          t))))))

(defun yb-goto-blacklists ()
  "Open directory with blacklists."
  (interactive)
  (let* ((project (projectile-project-root))
         (rel-path "src/build/yandex/ci/configs/platforms")
         (abs-path (expand-file-name (concat
                                      (file-name-as-directory project)
                                      rel-path))))
    (dired abs-path)))

(defun yb-goto-yb-link (path-in)
  "Goto yb:PATH-IN link."
  (let* ((parts (s-split ":" path-in))
         (len (length parts))
         (path (nth 0 parts))
         (line (if (> len 2) (nth 1 parts) 0))
         (branch (if (> len 2) (nth 2 parts) "master"))
         (search-term (if (> len 2) (nth 3 parts) (nth 1 parts)))
         (project (yb-select-other-project))
         (project-type (yb-what-project project))
         (abs-path (concat project
                           (cond
                            ((eq project-type 'chromium) path)
                            ((eq project-type 'yandex-browser) (concat "src/" path))))))
    (if (f-exists? abs-path)
        (progn
          (find-file abs-path)
          (when search-term
            (message "search for: %s" (base64-decode-string search-term))
            (goto-line (point-min))
            (search-forward (s-replace "\"" "" (base64-decode-string search-term))
                            nil
                            nil)))
      (user-error (format "file [%s] doesn't exist" abs-path)))))

(setq org-link-abbrev-alist
      '(("github"      . "https://github.com/%s")
        ("youtube"     . "https://youtube.com/watch?v=%s")
        ("google"      . "https://google.com/search?q=")
        ("gimages"     . "https://google.com/images?q=%s")
        ("gmap"        . "https://maps.google.com/maps?q=%s")
        ("duckduckgo"  . "https://duckduckgo.com/?q=%s")
        ("wikipedia"   . "https://en.wikipedia.org/wiki/%s")
        ("wolfram"     . "https://wolframalpha.com/input/?i=%s")
        ("yb"          . "elisp:(yb-goto-yb-link \"%s\")")
        ("doom-repo"   . "https://github.com/hlissner/doom-emacs/%s")))

(defun yb-search-buffer-p ()
  "Return non-nil if current buffer is search (ripgrep) buffer."
  (let* ((name (buffer-name)))
    (s-starts-with-p "*ripgrep-search*" name)))

(defun yb-get-notes-dir ()
  "Return path to selected or guessed ticket notes."
  (let* ((ticket (yb-guess-ticket))
         (path (concat yb-notes-path ticket)))
    (file-name-as-directory path)))

(defun yb-save-search ()
  "Save current search buffer to current ticekt notes."
  (interactive)
  (when (yb-search-buffer-p)
    (let* ((notes-dir (yb-get-notes-dir))
           (name (read-string "Name: ")))
      (write-file (concat notes-dir name)))))

;; hydra
(defhydra yb-tools (:hint t)
  "yandex-browser tools"
  ("a" yb-goto-arch-notes "arch notes")
  ("c" yb-compile-single-file "compile file")
  ("d" yb-todo "todo")
  ("g" yb-gn-refs "gn refs")
  ("l" yb-goto-blacklists "blacklists")
  ("m" yb-move-file-diff "move diff")
  ("n" yb-goto-ticket-notes "ticket notes")
  ("o" yb-overwrite-file-diff "overwrite diff")
  ("p" yb-prepare-build "prepare build")
  ("r" yb-reference-hydra/body "line/symbol reference operations" :exit t)
  ("s" yb-goto-ticket-tracker "ticket tracker")
  ("t" yb-trace-action-hydra/body "trace" :exit t)
  ("v" yb-visit-file-other-project "visit other project")
  ("w" yb-goto-ticket-wiki "ticket wiki")
  ("x" yb-x-at-point "x? at point")
  )

(bind-key "C-c y" 'yb-tools/body)

;; TODO: check branch -> find notes
;; TODO: check notes -> find branch & repo

(provide 'yandex-browser)
;;; yandex-browser.el ends here
