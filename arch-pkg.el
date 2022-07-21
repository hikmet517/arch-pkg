(require 'tabulated-list)
(require 'help-mode)
(require 'button)

(defconst arch-pkg-sync-db-path "/var/lib/pacman/sync/")
(defconst arch-pkg-local-db-path "/var/lib/pacman/local/")

(defvar arch-pkg-db nil "database (local and sync merged)")
(defvar arch-pkg-list '() "database (local and sync merged)")

(defvar arch-pkg-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] #'arch-pkg-describe-package)
    map))


;; helpers
(defun arch-pkg-reset-internal-data ()
  (interactive)
  (setq arch-pkg-sync-db nil
        arch-pkg-local-db nil
        arch-pkg-list nil
        arch-pkg-db nil))


(defun arch-pkg--format-date (n)
  (format-time-string "%Y-%m-%d %H:%M" n))


(defun arch-pkg--format-status (n)
  (aref ["installed" "dependency" ""] n))


(defun arch-pkg--format-size (n)
  (cond
   ((< n 1024)
    (format "%d B" n))
   ((< (/ n 1024.0) 1024.0)
    (format "%.1f KiB" (/ n 1024.0)))
   ((< (/ n 1024.0 1024.0) 1024.0)
    (format "%.1f MiB" (/ n 1024.0 1024.0)))
   ((< (/ n 1024.0 1024.0 1024.0) 1024.0)
    (format "%.1f GiB" (/ n 1024.0 1024.0 1024.0)))))


(defun arch-pkg--parse-desc (beg end)
  (let ((key "")
        (val "")
        (pkg (make-hash-table :test #'equal))
        (try-to-add (lambda (k v h)
                      (let ((kk (string-trim k))
                            (vv (string-trim v)))
                        (when (and (not (string-empty-p kk))
                                   (not (string-empty-p vv)))
                          (puthash kk vv h))))))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (skip-chars-forward "\n\t ")
        (let ((line (string-as-multibyte (buffer-substring-no-properties (point) (line-end-position)))))
          (unless (string-empty-p line)
            ;; TODO check ending %
            (if (= (aref line 0) ?%)  ; key found
                (progn
                  (funcall try-to-add key val pkg)
                  (setq key (substring line 1 (- (length line) 1)))
                  (setq val ""))
              (progn                  ; value found
                (setq val (concat val line "\n")))))
          (forward-line)
          (skip-chars-forward "\n\t ")))
      (funcall try-to-add key val pkg)
      pkg)))


(defun arch-pkg--read-desc-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (arch-pkg--parse-desc (point-min) (point-max))))


(defun arch-pkg--read-gz (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))

    (let ((i (point-min))
          (pkgs (make-hash-table :test #'equal))
          (buf-size (buffer-size)))

      (while (< i buf-size)
        (let ((name (replace-regexp-in-string "[[:cntrl:]]+" "" (buffer-substring-no-properties i (+ i 100))))
              (size (string-to-number (buffer-substring-no-properties (+ i 124) (+ i 124 11)) 8))
              (typeflag (buffer-substring-no-properties (+ i 156) (+ i 157))))

          (when (and (/= size 0)
                     (not (string= typeflag "x")))

            (let ((desc (arch-pkg--parse-desc (+ i 512) (+ i 512 size))))
              (puthash (gethash "NAME" desc) desc pkgs)))

          (setq i (+ (* (ceiling (/ (+ (1- i) 512 size) 512.0)) 512) 1))))
      pkgs)))


(defun arch-pkg--read-sync-db ()
  (let ((repo-all (make-hash-table :test #'equal)))
    (dolist (repo-file (directory-files arch-pkg-sync-db-path))
      (unless (or (string= repo-file ".")
                  (string= repo-file ".."))
        (message "reading: %s" (expand-file-name repo-file arch-pkg-sync-db-path))
        (let ((repo (arch-pkg--read-gz (expand-file-name repo-file arch-pkg-sync-db-path))))
          (maphash (lambda (name pkg)
                     (puthash "REPOSITORY" (file-name-base repo-file) pkg)
                     (puthash name pkg repo-all))
                   repo))))
    repo-all))


(defun arch-pkg--read-local-db ()
  (let ((db (make-hash-table :test #'equal)))
    (dolist (dir (directory-files arch-pkg-local-db-path))
      (unless (or (string= dir ".")
                  (string= dir ".."))
        (let ((pkg-dir (expand-file-name dir arch-pkg-local-db-path)))
          (when (file-directory-p pkg-dir)
            (let ((desc (arch-pkg--read-desc-file (expand-file-name
                                                   "desc"
                                                   pkg-dir))))
              (puthash (gethash "NAME" desc) desc db))))))
    db))


(define-derived-mode arch-pkg-list-mode tabulated-list-mode "Arch Package List"
  "Major mode for list of Arch packages

\\{arch-pkg-list-mode-map}"
  (visual-line-mode +1)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Package" 36 t)
          ("Version" 15 t)
          ("Repository" 12 t)
          ("Status" 12 t)
          ("Date" 17 t)
          ("Size" 0 t)]))


;; TODO: add required packages
(defun arch-pkg-list-packages ()
  (interactive)

  ;; read merge sync and local data and merge them into db
  (unless arch-pkg-db
    (let ((arch-pkg-sync-db (arch-pkg--read-sync-db))
          (arch-pkg-local-db (arch-pkg--read-local-db)))
      (setq arch-pkg-db (make-hash-table :test #'equal))
      ;; add everything in sync-db to db
      (maphash (lambda (k v) (puthash k v arch-pkg-db)) arch-pkg-sync-db)

      ;; traverse local-db, if found in db, merge it,
      ;; if not (meaning that it is a foreign package) add it
      (maphash (lambda (local-db-name local-db-pkg)
                 (let ((db-pkg (gethash local-db-name arch-pkg-db)))
                   (if db-pkg
                       ;; found, merge it
                       (maphash (lambda (k v) (puthash k v db-pkg)) local-db-pkg)
                     ;; not found, foreign package, add it
                     (puthash local-db-name local-db-pkg arch-pkg-db))))
               arch-pkg-local-db)

      ;; parse some fieds
      (maphash (lambda (name pkg)
                 (maphash (lambda (k v)
                            (cond
                             ((string= k "DEPENDS")
                              (let ((deps (split-string v "\n")))
                                ;; add dep to required by field
                                (puthash k deps pkg)
                                (dolist (d deps)
                                  (let ((dep-pkg (gethash d arch-pkg-db)))
                                    (when dep-pkg
                                      (push name (gethash "REQUIREDBY" dep-pkg)))))))
                             ((string= k "OPTDEPENDS")
                              (puthash k (split-string v "\n") pkg))
                             ((or (string= k "BUILDDATE")
                                  (string= k "INSTALLDATE")
                                  (string= k "SIZE")
                                  (string= k "ISIZE")
                                  (string= k "CSIZE")
                                  (string= k "REASON"))
                              (puthash k (string-to-number v) pkg))))
                          pkg))
               arch-pkg-db)))

  ;; create list for tabulated-list-entries
  (unless arch-pkg-list
    (maphash (lambda (name pkg)
               (push (list name
                           (vector (cons (gethash "NAME" pkg)
                                         (list
                                          'action
                                          (lambda (but) (arch-pkg-describe-package (button-label but)))))
                                   (gethash "VERSION" pkg)
                                   (or (gethash "REPOSITORY" pkg) "")
                                   (arch-pkg--format-status
                                    (or (gethash "REASON" pkg)
                                        (and (gethash "INSTALLDATE" pkg) 0)
                                        2))
                                   (arch-pkg--format-date
                                    (gethash "INSTALLDATE" pkg))
                                   (let ((size (gethash "SIZE" pkg)))
                                     (if size
                                         (arch-pkg--format-size size)
                                       ""))
                                   ))
                     arch-pkg-list))
             arch-pkg-db)

    ;; sort by package name
    (setq arch-pkg-list (sort arch-pkg-list (lambda (s1 s2) (string< (car s1)
                                                                     (car s2))))))

  ;; create buffer and display
  (let ((buf (get-buffer-create "Arch Packages")))
    (display-buffer buf)
    (set-buffer buf)
    (arch-pkg-list-mode)
    (setq tabulated-list-entries arch-pkg-list)
    (tabulated-list-init-header)
    (tabulated-list-print)))


;; FIX: when called from other buffer except "arch packages", it crashes
(defun arch-pkg-describe-package (&optional name)
  (interactive)
  (unless name
    (setq name (tabulated-list-get-id)))
  (let* ((pkg (gethash name arch-pkg-db))
         (key-col '(("NAME" . "Name")
                    ("VERSION" . "Version")
                    ("DESC" . "Description")
                    ("URL" . "Url")
                    ("LICENSE" . "Licenses")
                    ("REASON" . "Status")
                    ("REPOSITORY" . "Repository")
                    ("GROUPS" . "Groups")
                    ("DEPENDS" . "Dependencies")
                    ("OPTDEPENDS" . "Optional")
                    ("REQUIREDBY" . "Required By")
                    ("ARCH" . "Architecture")
                    ("PACKAGER" . "Maintainer")
                    ("BUILDDATE" . "Build Date")
                    ("INSTALLDATE" . "Install Date")
                    ("CSIZE" . "Download Size")
                    ("ISIZE" . "Installed Size"))))
    (let ((buf (get-buffer-create (format "*arch-pkg <%s>*" name)))
          (width (apply #'max (mapcar (lambda (s) (length (cdr s))) key-col))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq buffer-file-coding-system 'utf-8)

          ;; insert
          (dolist (kc key-col)
            (let ((val (gethash (car kc) pkg))
                  (key (cdr kc)))
              (when val
                (insert (propertize (string-pad (concat key ": ") (+ width 2) 32 t)
                                    'font-lock-face '(bold font-lock-function-name-face)))
                (cond
                 ((string= key "Url")
                  (insert-text-button val 'action (lambda (but)
                                                    (browse-url (button-label but))))
                  (insert "\n"))
                 ((or (string= key "Build Date")
                      (string= key "Install Date"))
                  (insert (arch-pkg--format-date val) "\n"))
                 ((or (string= key "Installed Size")
                      (string= key "Download Size"))
                  (insert (arch-pkg--format-size val) "\n"))
                 ((or (string= key "Dependencies")
                      (string= key "Required By")
                      (string= key "Optional"))
                  (dolist (dep (sort val #'string<))
                    (insert-text-button dep 'action
                                        (lambda (but)
                                          (arch-pkg-describe-package
                                           (car (split-string (button-label but) ":")))))
                    (insert " "))
                  (insert "\n"))
                 (t (insert (format "%s\n" val)))))))
          (goto-char (point-min))
          (setq buffer-read-only t)
          (help-mode)
          (pop-to-buffer buf))))))

;; (maphash (lambda (k v)
;;            (prin1 k (current-buffer))
;;            (insert ": ")
;;            (prin1 v (current-buffer))
;;            (insert "\n"))
;;          (gethash "guile" arch-pkg-db))
