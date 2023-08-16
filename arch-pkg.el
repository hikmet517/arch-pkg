;;; arch-pkg.el --- Browse Archlinux packages in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2022 Hikmet Altıntaş

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Maintainer: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Created: 22 Jul 2022
;; Keywords: tools
;; URL: https://github.com/hikmet517/arch-pkg
;; Version: 0.1
;; Package-Requires: ((tabulated-list "1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Browse Archlinux packages in Emacs, using an interface similar to built-in `package.el'.

;;; TODO:
;; add filters
;; mode init/destruct
;; take versions into account
;; test: fontconfig requires libexpat.so=1-64, which is in expat package
;; test: gcc depends on some .so libs
;; test: jack as a feature
;; test: acpi_call-dkms and acpi_call-lts both provide acpi_call

;;; Code:

;;;; Libraries

(require 'tabulated-list)
(require 'help-mode)
(require 'button)
(require 'subr-x)
(require 'rx)


;;;; Variables

(defconst arch-pkg-sync-db-path "/var/lib/pacman/sync/")
(defconst arch-pkg-local-db-path "/var/lib/pacman/local/")

(defvar arch-pkg-db nil "Database (local and sync merged).")
(defvar arch-pkg-providedby nil
  "Database for sonames and features from PROVIDE field.
soname => (package1 package2)
string => (symbols)")

(defvar arch-pkg-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] #'arch-pkg-describe-package)
    (define-key map "r" #'revert-buffer)
    map))

(define-button-type 'help-arch-package
  :supertype 'help-xref
  'help-function 'arch-pkg-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))

(define-button-type 'help-arch-package-installed
  :supertype 'help-xref
  'help-function 'arch-pkg-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package")
  'face '(:inherit font-lock-type-face :underline t))


;;;; User options

(defgroup arch-pkg nil
  "Arch-pkg customization."
  :group 'arch-pkg
  :prefix "arch-pkg-"
  :link '(url-link "https://github.com/hikmet517/arch-pkg"))

(defcustom arch-pkg-install-command "sudo pacman -S %s"
  "Package install command.  %s will be replaced by package name."
  :type '(string)
  :group 'arch-pkg)

(defcustom arch-pkg-delete-command "sudo pacman -R %s"
  "Package delete command.  %s will be replaced by package name."
  :type '(string)
  :group 'arch-pkg)



;;;; Helper functions

(defun arch-pkg-reset-internal-data ()
  "Reset internal data, for debugging only."
  (interactive)
  (setq arch-pkg-db nil
        arch-pkg-providedby nil))


(defun arch-pkg--print-hashmap (map)
  "Print hash-map MAP in current buffer.
For debugging."
  (maphash (lambda (k v)
             (prin1 k (current-buffer))
             (insert ": ")
             (prin1 v (current-buffer))
             (insert "\n"))
           map))


(defun arch-pkg--print-package (s)
  "Print the package S in current buffer.
For debugging."
  (when (stringp s)
    (setq s (intern s)))
  (let ((pkg (gethash s arch-pkg-db)))
    (if pkg
        (arch-pkg--print-hashmap pkg)
      (insert "package not found"))))


(defun arch-pkg--format-date (n)
  "Format unix date N (integer) as ISO date string."
  (format-time-string "%Y-%m-%d %H:%M" n))


(defun arch-pkg--format-status (n &optional show-not-installed)
  "Format package status N (an integer).
When SHOW-NOT-INSTALLED is t, print \"not installed\"."
  (if show-not-installed
      (aref ["installed" "dependency" "not installed"] n)
    (aref ["installed" "dependency" ""] n)))


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


(defun arch-pkg--parse-depends-str (s)
  "Parse dependecy string S.
Example: libglib-2.0.so=0-64 returns ('libglib-2.0.so' '=' '0-64')"
  (string-match (rx line-start
                    (group (+ (any lower numeric "_" "-" "+" ".")))
                    (group (? (* (or "<" "=" ">"))))
                    (group (? (* not-newline)))
                    line-end)
                s)
  (list (match-string 1 s)
        (match-string 2 s)
        (match-string 3 s)))


(defun arch-pkg--extract-package-name (s)
  "Extract package name from string S.
Returns string."
  (car (arch-pkg--parse-depends-str s)))


(defun arch-pkg--propertize (s)
  "Add properties to string S.
Used in `arch-pkg-describe-package'"
  (propertize s 'font-lock-face '(bold font-lock-function-name-face)))



;;;; Functions

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
        (message "Reading: %s" (expand-file-name repo-file arch-pkg-sync-db-path))
        (let ((repo (arch-pkg--read-gz (expand-file-name repo-file arch-pkg-sync-db-path))))
          (maphash (lambda (name pkg)
                     (puthash "REPOSITORY" (file-name-base repo-file) pkg)
                     (puthash name pkg repo-all))
                   repo))))
    repo-all))


(defun arch-pkg--read-local-db ()
  "Read local packages, all files under `arch-pkg-local-db-path'."
  (let ((db (make-hash-table)))
    (message "Reading files under: %s" arch-pkg-local-db-path)
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
    (maphash (lambda (_name pkg)
               (maphash (lambda (k v)
                          (cond
                           ((or (string= k "DEPENDS")
                                (string= k "PROVIDES")
                                (string= k "OPTDEPENDS")
                                (string= k "LICENSE")
                                (string= k "GROUPS")
                                (string= k "MAKEDEPENDS")
                                (string= k "CHECKDEPENDS"))
                            (puthash k (split-string v "\n") pkg))
                           ((or (string= k "BUILDDATE")
                                (string= k "INSTALLDATE")
                                (string= k "SIZE")
                                (string= k "ISIZE")
                                (string= k "CSIZE")
                                (string= k "REASON"))
                            (puthash k (string-to-number v) pkg))))
                        pkg)
               ;; set REASON
               (unless (gethash "REASON" pkg)
                 (puthash "REASON" (if (gethash "INSTALLDATE" pkg) 0 2) pkg)))
             arch-pkg-db)

    ;; fill arch-pkg-providedby
    (maphash (lambda (name pkg)
               (let ((provides (gethash "PROVIDES" pkg)))
                 (when provides
                   (dolist (p provides)
                     (when (not (member name (gethash p arch-pkg-providedby)))
                       (push name (gethash (arch-pkg--extract-package-name p) arch-pkg-providedby)))))))
             arch-pkg-db)

    ;; create additional fields: REQUIREDBY, OPTIONALFOR
    (maphash (lambda (name pkg)
               ;; create REQUIREDBY from DEPENDS
               (when-let ((deps (gethash "DEPENDS" pkg)))
                 (dolist (dep deps)
                   (let* ((depname (intern (arch-pkg--extract-package-name dep)))
                          (deppkg (gethash depname arch-pkg-db)))
                     (if deppkg
                         ;; if package exists, add it
                         (when (not (member name (gethash "REQUIREDBY" deppkg)))
                           (push name (gethash "REQUIREDBY" deppkg)))
                       ;; if it doesn't exist, it might be a feature, check `arch-pkg-providedby'
                       (dolist (p (gethash (symbol-name depname) arch-pkg-providedby))
                         (let ((pr (gethash p arch-pkg-db)))
                           (if pr
                               (when (not (member name (gethash "REQUIREDBY" pr)))
                                 (push name (gethash "REQUIREDBY" pr)))
                             (message "Package '%s' not found for REQUIREDBY field" p))))))))


               ;; create OPTIONALFOR from OPTDEPENDS
               (when-let ((deps (gethash "OPTDEPENDS" pkg)))
                 (dolist (dep deps)
                   (let* ((depname (intern (arch-pkg--extract-package-name dep)))
                          (deppkg (gethash depname arch-pkg-db)))
                     (if deppkg
                         ;; if package exists, add it
                         (when (not (member name (gethash "OPTIONALFOR" deppkg)))
                           (push name (gethash "OPTIONALFOR" deppkg)))
                       ;; if it doesn't exist, it might be a feature, check `arch-pkg-providedby'
                       (dolist (p (gethash (symbol-name depname) arch-pkg-providedby))
                         (let ((pr (gethash p arch-pkg-db)))
                           (if pr
                               (when (not (member name (gethash "OPTIONALFOR" pr)))
                                 (push name (gethash "OPTIONALFOR" pr)))
                             (message "Package '%s' not found for OPTIONALFOR field" p)))))))))
             arch-pkg-db)

    (maphash (lambda (_name pkg)
               (when-let ((reqs (gethash "REQUIREDBY" pkg)))
                 (puthash "REQUIREDBY"
                          (sort reqs (lambda (s1 s2) (string< (symbol-name s1) (symbol-name s2))))
                          pkg))
               (when-let ((opts (gethash "OPTIONALFOR" pkg)))
                 (puthash "OPTIONALFOR"
                          (sort opts (lambda (s1 s2) (string< (symbol-name s1) (symbol-name s2))))
                          pkg)))
             arch-pkg-db)))


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
          ("Size" 10 t)
          ("Description" 0 t)])
  (setq tabulated-list-padding 2)
  (toggle-truncate-lines +1)
  (setq revert-buffer-function 'arch-pkg-refresh))


;;;###autoload
(defun arch-pkg-refresh (&optional _arg _noconfirm)
  "Re-read database and list packages."
  (interactive)
  (arch-pkg--create-db)
  (arch-pkg-list-packages))


;;;###autoload
(defun arch-pkg-list-packages ()
  "Display a list of Archlinux packages."
  (interactive)

  ;; read merge sync and local data and merge them into db
  (unless arch-pkg-db
    (arch-pkg--create-db))

  ;; create list for tabulated-list-entries
  (let ((arch-pkg-list nil))
    (maphash (lambda (name pkg)
               (push (list name
                           (vector (cons (gethash "NAME" pkg)
                                         (list
                                          'action
                                          (lambda (but)
                                            (arch-pkg-describe-package (arch-pkg--extract-package-name (button-label but))))))
                                   (gethash "VERSION" pkg)
                                   (or (gethash "REPOSITORY" pkg) "")
                                   (arch-pkg--format-status (gethash "REASON" pkg))
                                   (arch-pkg--format-date
                                    (gethash "INSTALLDATE" pkg))
                                   (let ((size (gethash "SIZE" pkg)))
                                     (if size
                                         (arch-pkg--format-size size)
                                       ""))
                                   (gethash "DESC" pkg)))
                     arch-pkg-list))
             arch-pkg-db)

    ;; sort by package name
    (setq arch-pkg-list (sort arch-pkg-list (lambda (s1 s2) (string< (car s1)
                                                                     (car s2)))))

    ;; create buffer and display
    (let ((buf (get-buffer-create "*Arch Packages*")))
      (pop-to-buffer-same-window buf)
      (arch-pkg-list-mode)
      (setq tabulated-list-entries arch-pkg-list)
      (tabulated-list-init-header)
      (tabulated-list-print))))


(defun arch-pkg--make-button (text &rest properties)
  "Create button with TEXT and PROPERTIES, similar to `package-make-button'."
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         (progn
                           (require 'cus-edit) ; for the custom-button face
                           'custom-button)
                       'link)))
    (apply #'insert-text-button button-text 'face button-face 'follow-link t
           properties)))


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

  (setq package (intern (arch-pkg--extract-package-name
                         (if (stringp package)
                             package
                           (symbol-name package)))))

  (let ((pkg (gethash package arch-pkg-db)))

    (when (not pkg)
      ;; not found, this may be a feature provided by some other packages
      (when-let ((pkgs (gethash (symbol-name package) arch-pkg-providedby)))
        (if (cadr pkgs)
            ;; multiple choises, ask user
            (setq package (intern (completing-read (format "%s is provided by multiple packages: "
                                                           (symbol-name package))
                                                   (mapcar #'symbol-name pkgs)
                                                   nil
                                                   t)))
          ;; only one choise
          (setq package (car pkgs)))
        (setq pkg (gethash package arch-pkg-db))))

    (when pkg
      (let ((width 17))
        (help-setup-xref (list #'arch-pkg-describe-package package)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (let ((inhibit-read-only t))
              (erase-buffer)
              (setq buffer-file-coding-system 'utf-8)

              (insert (arch-pkg--propertize (string-pad "Name: " width ?\s t)))
              (insert (gethash "NAME" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Version: " width ?\s t)))
              (insert (gethash "VERSION" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Description: " width ?\s t)))
              (insert (gethash "DESC" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Url: " width ?\s t)))
              (let ((url (gethash "URL" pkg)))
                (help-insert-xref-button url 'help-url url))
              (insert "\n")

              (when-let* ((repo (gethash "REPOSITORY" pkg))
                          (arch (gethash "ARCH" pkg))
                          (name (gethash "NAME" pkg))
                          (pkg-url (format "https://archlinux.org/packages/%s/%s/%s/" repo arch name)))
                (insert (arch-pkg--propertize (string-pad "Package Url: " width ?\s t)))
                (help-insert-xref-button pkg-url 'help-url pkg-url)
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Licenses: " width ?\s t)))
              (insert (string-join (gethash "LICENSE" pkg) ", ") "\n")

              (let ((status (gethash "REASON" pkg)))
                (insert (arch-pkg--propertize (string-pad "Status: " width ?\s t)))
                (insert (arch-pkg--format-status status 'show-not-installed))
                (cond
                 ((= status 0)    ; installed
                  (insert " -- ")
                  (arch-pkg--make-button "Delete"
                                         'action #'arch-pkg-delete-action
                                         'package-name package)
                  (insert "\n"))
                 ((= status 2)          ;not installed
                  (insert " -- ")
                  (arch-pkg--make-button "Install"
                                         'action #'arch-pkg-install-action
                                         'package-name package)
                  (insert "\n"))
                 (t                     ; status = 1, dependency
                  (insert "\n"))))

              (insert (arch-pkg--propertize (string-pad "Repository: " width ?\s t)))
              (insert (gethash "REPOSITORY" pkg "") "\n")

              (when-let ((grp (gethash "GROUPS" pkg)))
                (insert (arch-pkg--propertize (string-pad "Groups: " width ?\s t)))
                (insert (string-join grp " ") "\n"))

              (when-let ((prs (gethash "PROVIDES" pkg)))
                (insert (arch-pkg--propertize (string-pad "Provides: " width ?\s t)))
                (dolist (pr prs)
                  (help-insert-xref-button pr 'help-arch-package pr)
                  (insert " "))
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Dependencies: " width ?\s t)))
              (if-let ((deps (gethash "DEPENDS" pkg)))
                  (dolist (dep deps)
                    (let ((p (gethash (intern (arch-pkg--extract-package-name dep)) arch-pkg-db)))
                      (if (and p (< (gethash "REASON" p) 2))
                          (help-insert-xref-button dep 'help-arch-package-installed dep)
                        (help-insert-xref-button dep 'help-arch-package dep)))
                    (insert " "))
                (insert "None"))
              (insert "\n")

              (when-let ((opts (gethash "OPTDEPENDS" pkg)))
                (insert (arch-pkg--propertize (string-pad "Optional: " width ?\s t)))
                (dolist (opt opts)
                  (let ((splitted (split-string opt ": ")))
                    (let ((p (gethash (intern (arch-pkg--extract-package-name (car splitted))) arch-pkg-db)))
                      (if (and p (< (gethash "REASON" p) 2))
                          (help-insert-xref-button (car splitted) 'help-arch-package-installed (car splitted))
                        (help-insert-xref-button (car splitted) 'help-arch-package (car splitted))))
                    (when (cadr splitted)
                      (insert ": " (cadr splitted)))
                    (insert "\n" (make-string width ?\s))))
                (delete-line)
                (delete-char -1)
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Required By: " width ?\s t)))
              (if-let ((reqs (gethash "REQUIREDBY" pkg)))
                  (dolist (req reqs)
                    (let ((p (gethash (intern (arch-pkg--extract-package-name (symbol-name req))) arch-pkg-db)))
                      (if (and p (< (gethash "REASON" p) 2))
                          (help-insert-xref-button (symbol-name req) 'help-arch-package-installed req)
                        (help-insert-xref-button (symbol-name req) 'help-arch-package req)))
                    (insert " "))
                (insert "None"))
              (insert "\n")

              (when-let ((opts (gethash "OPTIONALFOR" pkg)))
                (insert (arch-pkg--propertize (string-pad "Optional for: " width ?\s t)))
                (dolist (opt opts)
                  (let ((p (gethash (intern (arch-pkg--extract-package-name (symbol-name opt))) arch-pkg-db)))
                    (if (and p (< (gethash "REASON" p) 2))
                        (help-insert-xref-button (symbol-name opt) 'help-arch-package-installed opt)
                      (help-insert-xref-button (symbol-name opt) 'help-arch-package opt)))
                  (insert " "))
                (insert "\n"))

              (when-let ((cnf (gethash "CONFLICTS" pkg)))
                (insert (arch-pkg--propertize (string-pad "Conflicts with: " width ?\s t)))
                (dolist (c (split-string cnf "\n"))
                  (let ((p (gethash (intern (arch-pkg--extract-package-name c)) arch-pkg-db)))
                    (if (and p (< (gethash "REASON" p) 2))
                        (help-insert-xref-button c 'help-arch-package-installed c)
                      (help-insert-xref-button c 'help-arch-package c)))
                  (insert " "))
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Architecture: " width ?\s t)))
              (insert (gethash "ARCH" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Maintainer: " width ?\s t)))
              (insert (gethash "PACKAGER" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Build Date: " width ?\s t)))
              (insert (arch-pkg--format-date (gethash "BUILDDATE" pkg)) "\n")

              (when-let ((idate (gethash "INSTALLDATE" pkg)))
                (insert (arch-pkg--propertize (string-pad "Install Date: " width ?\s t)))
                (insert (arch-pkg--format-date idate) "\n"))

              (when-let ((isize (or (gethash "SIZE" pkg) (gethash "ISIZE" pkg))))
                (insert (arch-pkg--propertize (string-pad "Install Size: " width ?\s t)))
                (insert (arch-pkg--format-size isize) "\n"))

              (when-let ((csize (gethash "CSIZE" pkg)))
                (insert (arch-pkg--propertize (string-pad "Download Size: " width ?\s t)))
                (insert (arch-pkg--format-size csize) "\n"))

              (when-let ((val (gethash "VALIDATION" pkg)))
                (insert (arch-pkg--propertize (string-pad "Validation: " width ?\s t)))
                (insert val "\n"))

              (let ((status (gethash "REASON" pkg)))
                (when (or (= status 0)
                          (= status 1))
                  (insert (arch-pkg--propertize (string-pad "Files: " width ?\s t)))
                  (arch-pkg--make-button "Show files"
                                         'action #'arch-pkg-show-files-action
                                         'package-name (gethash "NAME" pkg)
                                         'version (gethash "VERSION" pkg)))))))))))


(defun arch-pkg-delete-action (button)
  (let ((pkg-name (button-get button 'package-name)))
    (message "%s" (symbolp pkg-name))
    (when (y-or-n-p (format-message "Delete package `%s'? " pkg-name))
      (arch-pkg-delete-package pkg-name))))


(defun arch-pkg-delete-package (package)
  (async-shell-command (format arch-pkg-delete-command package))
  (pop-to-buffer shell-command-buffer-name-async))


(defun arch-pkg-install-action (button)
  (let ((pkg-name (button-get button 'package-name)))
    (when (y-or-n-p (format-message "Install package `%s'? " pkg-name))
      (arch-pkg-install-package pkg-name))))


(defun arch-pkg-install-package (package)
  (async-shell-command (format arch-pkg-install-command package))
  (pop-to-buffer shell-command-buffer-name-async))


(defun arch-pkg-show-files-action (button)
  (let* ((pkg-name (button-get button 'package-name))
         (version (button-get button 'version))
         (filename (file-name-concat arch-pkg-local-db-path
                                     (concat pkg-name "-" version)
                                     "files")))
    (when (file-exists-p filename)
      (let ((buf (get-buffer-create (format "*Files of <%s>*" pkg-name))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents filename)
            (goto-char (point-min))
            (kill-line 1)               ; kill %FILE%
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                          (line-end-position))))
                (cond
                 ((string-empty-p line)
                  (kill-region (point) (point-max)))
                 ((string-suffix-p "/" line)
                  (kill-line 1))
                 (t
                  (insert "/")
                  (forward-line)))))
            (goto-char (point-min)))
          (text-mode)
          (setq buffer-read-only t)
          (display-buffer buf))))))


(provide 'arch-pkg)
;;; arch-pkg.el ends here
