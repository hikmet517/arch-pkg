;;; arch-pkg.el --- Browse Archlinux packages in Emacs  -*- lexical-binding: t -*-

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Keywords: convenience
;; URL: "https://github.com/hikmet517/arch-pkg"
;; Version: 0.1

;;; Commentary:
;; Browse Archlinux packages in Emacs, using an interface similar to package.el

;;; TODO:
;; print installed files
;; find optional dependency status
;; show package url
;; fontconfig requires libexpat.so=1-64, which is in expat package
;; acpi_call-dkms and acpi_call-lts both provide acpi_call

;;; Code:

;;;; Libraries

(require 'tabulated-list)
(require 'help-mode)
(require 'button)
(require 'subr-x)


;;;; Variables

(defconst arch-pkg-sync-db-path "/var/lib/pacman/sync/")
(defconst arch-pkg-local-db-path "/var/lib/pacman/local/")

(defvar arch-pkg-db nil "Database (local and sync merged).")
(defvar arch-pkg-list '() "Package list for `tabulated-list-entries'.")
(defvar arch-pkg-providedby nil "Database for sonames from PROVIDE field.  soname => (package1 package2)")

(defvar arch-pkg-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] #'arch-pkg-describe-package)
    map))

(define-button-type 'help-arch-package
  :supertype 'help-xref
  'help-function 'arch-pkg-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))


;;;; Helper functions
(defun arch-pkg-reset-internal-data ()
  "Reset internal data, for debugging only."
  (interactive)
  (setq arch-pkg-list nil
        arch-pkg-db nil
        arch-pkg-providedby nil))


(defun arch-pkg--format-date (n)
  "Format unix date N (integer) as ISO date string."
  (format-time-string "%Y-%m-%d %H:%M" n))


(defun arch-pkg--format-status (n)
  "Format package status N (an integer)."
  (aref ["installed" "dependency" ""] n))


(defun arch-pkg--format-size (n)
  "Format size given in bytes N (an integer)."
  (cond
   ((< n 1024)
    (format "%d B" n))
   ((< (/ n 1024.0) 1024.0)
    (format "%.1f KiB" (/ n 1024.0)))
   ((< (/ n 1024.0 1024.0) 1024.0)
    (format "%.1f MiB" (/ n 1024.0 1024.0)))
   ((< (/ n 1024.0 1024.0 1024.0) 1024.0)
    (format "%.1f GiB" (/ n 1024.0 1024.0 1024.0)))))


(defun arch-pkg--extract-package-name (s)
  "Extract package name from string S."
  (string-match (rx line-start (group (+ (any lower numeric "-" "+" "."))) (* any) line-end) s)
  (match-string 1 s))


(defun arch-pkg--parse-depends-str (s)
  "Parse dependecy string S.
Example: libglib-2.0.so=0-64 returns ('libglib-2.0.so' '=' '0-64')"
  (string-match (rx line-start
                    (group (+ (any lower numeric "-" "+" ".")))
                    (group (? (* (or "<" "=" ">"))))
                    (group (? (* not-newline)))
                    line-end)
                s)
  (list (match-string 1 s)
        (match-string 2 s)
        (match-string 3 s)))




;;;; Functions

(defun arch-pkg--find-pkg-from-dependency (s)
  "Find list of packages that provides dependency given in string S.
Example: finds 'libisl' from string 'libisl.so=23-64'."
  (setq s (arch-pkg--extract-package-name s))
  (if (string-suffix-p ".so" s)
      (let ((lst '()))
        (maphash (lambda (name pkg)
                   (let ((provides (gethash "PROVIDES" pkg)))
                     (when provides
                       (dolist (pr provides)
                         (when (string= (arch-pkg--extract-package-name pr)
                                        s)
                           (push name lst))))))
                 arch-pkg-db)
        lst)
    (list (intern s))))


(defun arch-pkg--parse-desc (beg end)
  "Parse descr buffer given between BEG and END."
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
        (let ((line (decode-coding-string (buffer-substring-no-properties (point) (line-end-position))
                                          'utf-8)))
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
  "Read and parse descr file FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (arch-pkg--parse-desc (point-min) (point-max))))


(defun arch-pkg--read-gz (file)
  "Read gzipped package file FILE.
Read gzipped package file, uncompress it, parse descr files
into a hashmap and return it."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))

    (let ((i (point-min))
          (pkgs (make-hash-table))
          (buf-size (buffer-size)))

      (while (< i buf-size)
        (let ((size (string-to-number (buffer-substring-no-properties (+ i 124) (+ i 124 11)) 8))
              (typeflag (buffer-substring-no-properties (+ i 156) (+ i 157)))
              ;; (name (replace-regexp-in-string "[[:cntrl:]]+" "" (buffer-substring-no-properties i (+ i 100))))
              )

          (when (and (/= size 0)
                     (not (string= typeflag "x")))

            (let ((desc (arch-pkg--parse-desc (+ i 512) (+ i 512 size))))
              (puthash (intern (gethash "NAME" desc)) desc pkgs)))

          (setq i (+ (* (ceiling (/ (+ (1- i) 512 size) 512.0)) 512) 1))))
      pkgs)))


(defun arch-pkg--read-sync-db ()
  "Read sync package database under `arch-pkg-sync-db-path' (all repos)."
  (let ((repo-all (make-hash-table)))
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
  "Read local packages, all files under `arch-pkg-local-db-path'."
  (let ((db (make-hash-table)))
    (dolist (dir (directory-files arch-pkg-local-db-path))
      (unless (or (string= dir ".")
                  (string= dir ".."))
        (let ((pkg-dir (expand-file-name dir arch-pkg-local-db-path)))
          (when (file-directory-p pkg-dir)
            (let ((desc (arch-pkg--read-desc-file (expand-file-name
                                                   "desc"
                                                   pkg-dir))))
              (puthash (intern (gethash "NAME" desc)) desc db))))))
    db))


(defun arch-pkg--create-db ()
  "Read local and sync databases and merge them into `arch-pkg-db'."

  ;; clean old data first
  (setq arch-pkg-db nil
        arch-pkg-providedby nil)

  (let ((arch-pkg-sync-db (arch-pkg--read-sync-db))
        (arch-pkg-local-db (arch-pkg--read-local-db)))

    (setq arch-pkg-db (make-hash-table)
          arch-pkg-providedby (make-hash-table :test #'equal))

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
                            (puthash k (split-string v "\n") pkg))
                           ((string= k "PROVIDES")
                            (dolist (p (split-string v "\n"))
                              (when (not (member name (gethash p arch-pkg-providedby)))
                                (push name (gethash p arch-pkg-providedby)))))
                           ((or (string= k "OPTDEPENDS")
                                (string= k "LICENSE"))
                            (puthash k (split-string v "\n") pkg))
                           ((or (string= k "BUILDDATE")
                                (string= k "INSTALLDATE")
                                (string= k "SIZE")
                                (string= k "ISIZE")
                                (string= k "CSIZE")
                                (string= k "REASON"))
                            (puthash k (string-to-number v) pkg))))
                        pkg))
             arch-pkg-db)

    ;; create REQUIREDBY field
    (maphash (lambda (name pkg)
               (dolist (dep (gethash "DEPENDS" pkg))
                 (let ((depname (intern (car (arch-pkg--parse-depends-str dep)))))
                   (if (gethash depname arch-pkg-db)
                       (push (symbol-name name) (gethash "REQUIREDBY" (gethash depname arch-pkg-db)))
                     (dolist (p (gethash (symbol-name depname) arch-pkg-providedby))
                       (when (gethash p arch-pkg-db)
                         (push (symbol-name name) (gethash "REQUIREDBY" (gethash p arch-pkg-db)))))))))
             arch-pkg-db)))


;;;###autoload
(define-derived-mode arch-pkg-list-mode tabulated-list-mode "Arch Package List"
  "Major mode for browsing a list of Archlinux packages.

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
          ("Size" 0 t)])
  (setq tabulated-list-padding 2))


;;;###autoload
(defun arch-pkg-list-packages ()
  "Display a list of packages."
  (interactive)

  ;; read merge sync and local data and merge them into db
  (unless arch-pkg-db
    (arch-pkg--create-db))

  ;; create list for tabulated-list-entries
  (unless arch-pkg-list
    (maphash (lambda (name pkg)
               (push (list name
                           (vector (cons (gethash "NAME" pkg)
                                         (list
                                          'action
                                          (lambda (but)
                                            (arch-pkg-describe-package (arch-pkg--extract-package-name (button-label but))))))
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
                                       ""))))
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


;;;###autoload
(defun arch-pkg-describe-package (&optional package)
  "Display the full documentation of Archlinux package PACKAGE (string or symbol)."
  (interactive
   (list
    (progn
      (unless arch-pkg-db
        (arch-pkg--create-db))
      (unless (eq major-mode 'arch-pkg-list-mode)
        (completing-read "Describe Arch Package: "
                         (hash-table-keys arch-pkg-db))))))

  (when (null package)
    (if (eq major-mode 'arch-pkg-list-mode)
        (setq package (tabulated-list-get-id))
      (error "Package name needed")))

  (setq package (intern (car (arch-pkg--parse-depends-str
                              (if (stringp package)
                                  package
                                (symbol-name package))))))

  (let ((pkg (gethash package arch-pkg-db))
        (key-col '(("NAME" . "Name")
                   ("VERSION" . "Version")
                   ("DESC" . "Description")
                   ("URL" . "Url")
                   ("" . "Package Url")
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

    ;; if not found, this may be something provided by some other packages
    (when (not pkg)
      (let ((pkgs (gethash (symbol-name package) arch-pkg-providedby)))
        (when pkgs
          (setq package (intern (completing-read "Multiple packages provides this: "
                                                 (mapcar #'symbol-name pkgs)
                                                 nil
                                                 t)))
          (setq pkg (gethash package arch-pkg-db)))))

    (when pkg
      (let ((width (apply #'max (mapcar (lambda (s) (length (cdr s))) key-col))))

        (help-setup-xref (list #'arch-pkg-describe-package package)
                         (called-interactively-p 'interactive))

        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (let ((inhibit-read-only t))
              (erase-buffer)
              (setq buffer-file-coding-system 'utf-8)

              ;; insert
              (dolist (kc key-col)
                (let ((val (gethash (car kc) pkg))
                      (key (cdr kc)))
                  (when (or val (string= "" (car kc)))
                    (insert (propertize (string-pad (concat key ": ") (+ width 3) ?\s t)
                                        'font-lock-face '(bold font-lock-function-name-face)))
                    (cond
                     ((string= key "Url")
                      (help-insert-xref-button val 'help-url val)
                      (insert "\n"))
                     ((string= key "Package Url")
                      (let ((pkg-url (format "https://archlinux.org/packages/%s/%s/%s/"
                                             (gethash "REPOSITORY" pkg)
                                             (gethash "ARCH" pkg)
                                             (gethash "NAME" pkg))))
                        (help-insert-xref-button pkg-url 'help-url pkg-url)
                        (insert "\n")))
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
                        (help-insert-xref-button dep 'help-arch-package dep)
                        (insert " "))
                      (insert "\n"))
                     ((string= key "Licenses")
                      (insert (string-join val ", ") "\n"))
                     (t (insert (format "%s\n" val)))))))
              (help-mode))))))))

(provide 'arch-pkg)
;;; arch-pkg.el ends here
