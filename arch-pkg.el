;;; arch-pkg.el --- Browse Archlinux packages in Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Hikmet Altıntaş

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Maintainer: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Created: 22 Jul 2022
;; Keywords: tools
;; URL: https://github.com/hikmet517/arch-pkg
;; Version: 0.2
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
;; Browse Arch Linux packages in Emacs using an interface similar to built-in `package.el'.

;;; Check:
;; - fontconfig requires libexpat.so=1-64, which is in expat package
;; - gcc depends on some .so libs
;; - jack as a feature
;; - acpi_call-dkms and acpi_call-lts both provide acpi_call
;; - boost-libs has lots of indirect "requiredby"s

;;; Code:


;;;; Libraries

(require 'button)
(require 'cl-lib)
(require 'font-lock)
(require 'help-mode)
(require 'rx)
(require 'seq)
(require 'shell)
(require 'subr-x)
(require 'tabulated-list)
(require 'tree-widget)
(require 'url)


;;;; Variables

(defconst arch-pkg-sync-db-path "/var/lib/pacman/sync/")
(defconst arch-pkg-local-db-path "/var/lib/pacman/local/")
(defconst arch-pkg-package-url "https://archlinux.org/packages/%s/%s/%s/")
(defconst arch-pkg-aur-info-url "https://aur.archlinux.org/rpc/?v=5&type=info&arg=%s")
(defconst arch-pkg-aur-search-url "https://aur.archlinux.org/rpc/?v=5&type=search&arg=%s")

(defvar arch-pkg-db nil "Package database (local and sync merged).")
(defvar arch-pkg-providedby nil
  "Database for sonames and features from PROVIDE field.
soname => (package1 package2)
type: symbol => list of symbols")

(defvar arch-pkg-aur-db nil "Database to store the results of the last AUR search.")

(defvar-keymap arch-pkg-list-mode-map
  :doc "Local keymap for `arch-pkg-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"  #'arch-pkg-list-describe-package
  "d"    #'arch-pkg-describe-package
  "r"    #'revert-buffer
  "h"    #'arch-pkg-list--quick-help
  "/ /"  #'arch-pkg-list-clear-filter
  "/ n"  #'arch-pkg-list-filter-by-name
  "/ d"  #'arch-pkg-list-filter-by-description
  "/ f"  #'arch-pkg-list-filter-by-name-or-description
  "/ r"  #'arch-pkg-list-filter-by-repo)

(easy-menu-define arch-pkg-list-mode-menu arch-pkg-list-mode-map
  "Menu for `arch-pkg-list-mode'."
  '("Package"
    ["Describe Package" arch-pkg-list-describe-package :help "Display information about this package"]
    ["Refresh Package List" revert-buffer :help "Re-read the local package database"]
    "--"
    ("Filter Packages"
     ["Filter by Name" arch-pkg-list-filter-by-name
      :help
      "Prompt for regexp, display only packages whose names match the regexp"]
     ["Filter by Description" arch-pkg-list-filter-by-description
      :help
      "Prompt for regexp, display only packages whose descriptions match the regexp"]
     ["Filter by Name pr Description" arch-pkg-list-filter-by-name-or-description
      :help
      "Prompt for regexp, display only packages whose names or descriptions match the regexp"]
     ["Filter by Repository" arch-pkg-list-filter-by-repo
      :help
      "Prompt for repo(s), display only packages from those repositories"]
     ["Clear Filter" arch-pkg-list-clear-filter
      :help "Clear package list filtering, display the entire list again"])))

(defvar-keymap arch-pkg-aur-list-mode-map
  :doc "Local keymap for `arch-pkg-aur-list-mode' buffers."
  :parent tabulated-list-mode-map
  "C-m"  #'arch-pkg-aur-list-describe-package
  "r"    #'revert-buffer)


;;;; User options

(defgroup arch-pkg nil
  "Arch-pkg customization."
  :group 'arch-pkg
  :prefix "arch-pkg-"
  :link '(url-link "https://github.com/hikmet517/arch-pkg"))

(defcustom arch-pkg-install-command "sudo pacman -S %s"
  "Package install command.  %s will be replaced with package name."
  :type '(string)
  :group 'arch-pkg)

(defcustom arch-pkg-delete-command "sudo pacman -R %s"
  "Package delete command.  %s will be replaced with package name."
  :type '(string)
  :group 'arch-pkg)

(defcustom arch-pkg-aur-install-command "yay -S %s"
  "Package install command for AUR.  %s will be replaced with package name."
  :type '(string)
  :group 'arch-pkg)

;;;; Faces


(defface arch-pkg-name
  '((t :inherit link))
  "Face used on package names in the arch package menu.")

(defface arch-pkg-description
  '((t :inherit default))
  "Face used on package description summaries in the arch package menu.")

(defface arch-pkg-status-built-in
  '((t :inherit font-lock-builtin-face))
  "Face used on the fields other than name and description of built-in packages.")

(defface arch-pkg-status-installed
  '((t :inherit font-lock-constant-face))
  "Face used on the fields other than name and description of installed arch packages.")

(defface arch-pkg-status-not-installed
  '((t :inherit arch-pkg-status-built-in))
  "Face used on the fields other than name and description of not installed arch packages.")

(defface arch-pkg-status-dependency
  '((t :inherit font-lock-type-face))
  "Face used on the fields other than name and description of dependency packages.")

(defface arch-pkg-status-installed-foreign
  '((t :inherit (font-lock-string-face)))
  "Face used on the fields other than description of foreign arch packages.")


;;;; Data Types

(define-button-type 'help-arch-package
  :supertype 'help-xref
  'help-function 'arch-pkg-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))

(define-button-type 'help-arch-package-installed
  :supertype 'help-xref
  'help-function 'arch-pkg-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package")
  'face '(:inherit font-lock-type-face :underline t))

(define-button-type 'help-aur-package
  :supertype 'help-xref
  'help-function 'arch-pkg-aur-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))

(define-button-type 'help-aur-package-installed
  :supertype 'help-xref
  'help-function 'arch-pkg-aur-describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package")
  'face '(:inherit font-lock-type-face :underline t))

(define-button-type 'help-arch-package-files
  :supertype 'help-xref
  'help-function 'arch-pkg-show-files
  'help-echo (purecopy "mouse-2, RET: Show files"))

(define-button-type 'help-arch-package-dep-tree
  :supertype 'help-xref
  'help-function 'arch-pkg-show-dependency-tree
  'help-echo (purecopy "mouse-2, RET: Show dependency tree"))

(cl-defstruct (arch-pkg-desc (:constructor arch-pkg-desc-create))
  "Package description structure for \"desc\" files."
  ;; https://gitlab.archlinux.org/pacman/pacman/-/blob/v7.1.0/lib/libalpm/package.h?ref_type=tags#L90
  ;; MAN 5 PKGBUILD

  ;; (filename nil :type string)
  ;; (base nil :type string)
  (name nil :type string)
  (version nil :type string)
  (desc nil :type string)
  (url nil :type string)
  (packager nil :type string)

  ;; (md5sum nil :type string)
  ;; (sha1sums nil :type string)
  ;; (sha224sums nil :type string)
  ;; (sha256sums nil :type string)
  ;; (sha384sums nil :type string)
  ;; (sha512sums nil :type string)
  ;; (b2sums nil :type string)

  ;; (pgpsig  nil :type string)

  (arch nil :type string)

  (builddate nil :type integer)
  (installdate nil :type integer)

  (size nil :type integer)
  (isize nil :type integer)
  (csize nil :type integer)

  (licenses nil :type list)
  (groups nil :type list)

  (depends nil :type list)
  (optdepends nil :type list)
  (checkdepends nil :type list)
  (makedepends nil :type list)

  (conflicts nil :type list)
  (replaces nil :type list)
  (provides nil :type list)
  ;; (backup nil :type list)
  ;; (removes nil :type list)

  ;; 0: installed, 1: dependency, 2: not installed (0 and 2 assigned by us)
  (reason nil :type integer)

  ;; (xdata nil :type list)
  (validation nil :type string)

  ;; created
  (repository nil :type string)
  (requiredby nil :type list)
  (optionalfor nil :type list))

(cl-defun arch-pkg-desc-from-plist-str (&key name version desc url packager
                                             arch validation reason
                                             builddate installdate
                                             size isize csize
                                             license groups
                                             depends optdepends checkdepends makedepends
                                             conflicts replaces provides
                                             &allow-other-keys)
  "Create `arch-pkg-desc' from a plist of symbols and strings.
Optional keyword arguments: NAME, VERSION, DESC, URL, PACKAGER, ARCH,
VALIDATION, REASON, BUILDDATE, INSTALLDATE, SIZE, ISIZE, CSIZE, LICENSE,
GROUPS, DEPENDS, OPTDEPENDS, CHECKDEPENDS, MAKEDEPENDS, CONFLICTS,
REPLACES, PROVIDES."
  (when installdate
    (setq installdate (string-to-number installdate)))
  (arch-pkg-desc-create
   :name name
   :version version
   :desc desc
   :url url
   :packager packager
   :arch arch
   :builddate (when builddate (string-to-number builddate))
   :installdate installdate
   :size (when size (string-to-number size))
   :isize (when isize (string-to-number isize))
   :csize (when csize (string-to-number csize))
   :reason (if reason (string-to-number reason)
             (if installdate 0 2))
   :licenses (when license (string-split license "\n"))
   :groups (when groups (string-split groups "\n"))
   :depends (when depends (string-split depends "\n"))
   :optdepends (when optdepends (string-split optdepends "\n"))
   :checkdepends (when checkdepends (string-split checkdepends "\n"))
   :makedepends (when makedepends (string-split makedepends "\n"))
   :conflicts (when conflicts (string-split conflicts "\n"))
   :replaces (when replaces (string-split replaces "\n"))
   :provides (when provides (string-split provides "\n"))
   :validation validation))

(defun arch-pkg-desc-print (desc)
  "Print all the fields of DESC to the current buffer.
For debugging."
  (let* ((type (type-of desc))
         (slots (cdr (cl-struct-slot-info type))))
    (insert (format "#s(%s\n" type))
    (dolist (slot slots)
      (let* ((slot-name (car slot))
             (slot-val (cl-struct-slot-value type slot-name desc)))
        (when slot-val
          (insert (format "  %-12s: %S\n" slot-name slot-val)))))
    (insert ")\n")))

(defun arch-pkg-desc-installed-p (desc)
  "Check if DESC is installed."
  (let ((reason (arch-pkg-desc-reason desc)))
    (if reason
        (< (arch-pkg-desc-reason desc) 2)
      (not (null (arch-pkg-desc-installdate desc))))))


;;;; Helper functions

(defun arch-pkg--parse-depends-str (s)
  "Parse dependency string S.
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

(defun arch-pkg--get-desc (pkg)
  "Get `arch-pkg-desc' from PKG.
PKG may be of type `string', `symbol' or `arch-pkg-desc'."
  (when (stringp pkg)
    (setq pkg (intern (arch-pkg--extract-package-name pkg))))
  (cond ((arch-pkg-desc-p pkg)
         pkg)
        ((symbolp pkg)
         (unless arch-pkg-db
           (arch-pkg--create-db))
         (gethash pkg arch-pkg-db))
        (t nil)))

(defun arch-pkg--installed-p (p)
  "Check if package P is installed.
Package may be of type `string', `symbol', `alist' or `arch-pkg-desc'."
  (when-let* ((desc (arch-pkg--get-desc p)))
    (arch-pkg-desc-installed-p desc)))

(defun arch-pkg--dep-satisfied-p (dep)
  "Check if dependency DEP is satisfied.
DEP may be `string' or `symbol'."
  (when (stringp dep)
    (setq dep (intern (arch-pkg--extract-package-name dep))))
  (if (arch-pkg--installed-p dep)
      t  ; installed
    ;; might be a feature, check providedby
    (seq-some #'arch-pkg--installed-p (gethash dep arch-pkg-providedby))))

(defun arch-pkg-reset-internal-data ()
  "Reset internal data.
For debugging only."
  (interactive)
  (setq arch-pkg-db nil
        arch-pkg-providedby nil))

(defun arch-pkg--print-package (s)
  "Print the package S in current buffer.
For debugging."
  (when (stringp s)
    (setq s (intern s)))
  (let ((desc (gethash s arch-pkg-db)))
    (if desc
        (arch-pkg-desc-print desc)
      (insert "package not found"))))

(defun arch-pkg--format-date (date)
  "Format unix date DATE (integer) as ISO date string."
  (format-time-string "%Y-%m-%d %H:%M" date))

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

(defun arch-pkg--size-predicate (A B)
  "Size comparison between two lines A and B.
For `tabulated-list-mode'."
  (let ((descA (cdr (assoc (car A) arch-pkg-db)))
        (descB (cdr (assoc (car B) arch-pkg-db))))
    (< (or (arch-pkg-desc-isize descA)
           (arch-pkg-desc-size descA))
       (or (arch-pkg-desc-isize descB)
           (arch-pkg-desc-size descB)))))

(defun arch-pkg--propertize (s)
  "Add properties to string S.
Used in `arch-pkg-describe-package'"
  (propertize s 'font-lock-face '(bold font-lock-function-name-face)))


;;;; Functions

(defun arch-pkg--parse-desc (beg end)
  "Parse the portion of current descr buffer given with BEG and END and return `arch-pkg-desc'."
  (let ((key "")
        (val "")
        (pkg '())
        (try-to-add (lambda (pl k v)
                      (let ((kk (string-trim k))
                            (vv (string-trim v)))
                        (when (and (not (string-empty-p kk))
                                   (not (string-empty-p vv)))
                          (plist-put pl (intern (concat ":" (downcase kk))) vv))))))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (skip-chars-forward "\n\t ")
        (let ((line (decode-coding-string (buffer-substring-no-properties
                                           (point)
                                           (line-end-position))
                                          'utf-8)))
          (unless (string-empty-p line)
            (if (and (= (aref line 0) ?%)
                     (= (aref line (1- (length line))) ?%))  ; key found
                (progn
                  (setq pkg (funcall try-to-add pkg key val))
                  (setq key (substring line 1 (1- (length line))))
                  (setq val ""))
              (progn                    ; value found
                (setq val (concat val line "\n")))))
          (forward-line)
          (skip-chars-forward "\n\t ")))
      (setq pkg (funcall try-to-add pkg key val))
      (apply #'arch-pkg-desc-from-plist-str pkg))))

(defun arch-pkg--read-desc-file (file)
  "Read and parse descr file FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (arch-pkg--parse-desc (point-min) (point-max))))

(defun arch-pkg--read-gz (file)
  "Read gzipped package file FILE.
Read gzipped package file, uncompress it, parse descr files into an `alist' and return it."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (zlib-decompress-region (point-min) (point-max))
    (goto-char (point-min))

    (let ((i (point-min))
          (pkgs (make-hash-table))
          (buf-size (buffer-size)))

      (while (< i buf-size)
        (let ((size (string-to-number (buffer-substring-no-properties
                                       (+ i 124)
                                       (+ i 124 11))
                                      8))
              (typeflag (buffer-substring-no-properties (+ i 156) (+ i 157))))

          (when (and (/= size 0)
                     (not (string= typeflag "x")))
            (let ((desc (arch-pkg--parse-desc (+ i 512) (+ i 512 size))))
              (puthash (intern (arch-pkg-desc-name desc)) desc pkgs)))

          (setq i (1+ (* (ceiling (/ (+ (1- i) 512 size) 512.0)) 512)))))
      pkgs)))

(defun arch-pkg--read-sync-db ()
  "Read sync package database under `arch-pkg-sync-db-path' (all repo-all)."
  (let ((repo-all (make-hash-table)))
    (dolist (repo-file (directory-files arch-pkg-sync-db-path))
      (unless (or (string= repo-file ".")
                  (string= repo-file "..")
                  (not (string-suffix-p ".db" repo-file)))
        (message "Reading: %s" (expand-file-name repo-file arch-pkg-sync-db-path))
        (let ((repo (arch-pkg--read-gz (expand-file-name repo-file arch-pkg-sync-db-path)))
              (repo-name (file-name-base repo-file)))
          (maphash (lambda (name pkg)
                     (setf (arch-pkg-desc-repository pkg) repo-name)
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
              (puthash (intern (arch-pkg-desc-name desc)) desc db))))))
    db))

(defun arch-pkg--create-db ()
  "Read local and sync databases and merge them into `arch-pkg-db'."

  ;; clean old data first
  (setq arch-pkg-db nil
        arch-pkg-providedby nil)

  ;; read sync and local db and merge them into a final db
  (let ((arch-pkg-sync-db (arch-pkg--read-sync-db))
        (arch-pkg-local-db (arch-pkg--read-local-db)))

    (setq arch-pkg-db (make-hash-table)
          arch-pkg-providedby (make-hash-table))

    ;; add everything in sync-db to db
    (maphash (lambda (k v) (puthash k v arch-pkg-db)) arch-pkg-sync-db)

    ;; traverse local-db, if found in db, merge it,
    ;; if not (meaning that it is a foreign package) add it
    (maphash (lambda (pkg-name pkg-desc)
               (let* ((found-pkg-desc (gethash pkg-name arch-pkg-db)))
                 (if found-pkg-desc
                     (progn
                       ;; found, merge it (ignore xdata)
                       (setf (arch-pkg-desc-installdate found-pkg-desc)
                             (arch-pkg-desc-installdate pkg-desc))
                       (setf (arch-pkg-desc-reason found-pkg-desc)
                             (arch-pkg-desc-reason pkg-desc))
                       (setf (arch-pkg-desc-size found-pkg-desc)
                             (arch-pkg-desc-size pkg-desc))
                       (setf (arch-pkg-desc-validation found-pkg-desc)
                             (arch-pkg-desc-validation pkg-desc)))
                   ;; not found, foreign package, add it
                   (puthash pkg-name pkg-desc arch-pkg-db))))
             arch-pkg-local-db)

    ;; fill `arch-pkg-providedby' hashmap
    (maphash (lambda (name desc)
               (let ((provides (arch-pkg-desc-provides desc)))
                 (dolist (p provides)
                   (let ((p-sym (intern (arch-pkg--extract-package-name p))))
                     (cl-pushnew name (gethash p-sym arch-pkg-providedby))))))
             arch-pkg-db)

    ;; fill additional fields: `requiredby', `optionalfor' of `arch-pkg-desc'
    (maphash (lambda (pkg-name pkg-desc)
               ;; create `requiredby' from `depends'
               (dolist (dep (arch-pkg-desc-depends pkg-desc))
                 (let* ((dep-name (intern (arch-pkg--extract-package-name dep)))
                        (dep-desc (gethash dep-name arch-pkg-db)))
                   (if dep-desc
                       ;; if package exists, add it
                       (cl-pushnew pkg-name (arch-pkg-desc-requiredby dep-desc))
                     ;; if it doesn't exist, it might be a feature, check `arch-pkg-providedby'
                     (dolist (p (gethash dep-name arch-pkg-providedby))
                       (let ((pr (gethash p arch-pkg-db)))
                         (if pr
                             (let ((elem (cons dep-name pkg-name)))
                               (cl-pushnew elem (arch-pkg-desc-requiredby pr)))
                           (message "Package '%s' not found for requiredby field" p)))))))

               ;; create `optionalfor' from `optdepends'
               (dolist (dep (arch-pkg-desc-optdepends pkg-desc))
                 (let* ((dep-name (intern (arch-pkg--extract-package-name dep)))
                        (dep-desc (gethash dep-name arch-pkg-db)))
                   (if dep-desc
                       ;; if package exists, add it
                       (cl-pushnew pkg-name (arch-pkg-desc-optionalfor dep-desc))
                     ;; if it doesn't exist, it might be a feature, check `arch-pkg-providedby'
                     (dolist (p (gethash dep-name arch-pkg-providedby))
                       (let ((pr (gethash p arch-pkg-db)))
                         (if pr
                             (let ((elem (cons dep-name pkg-name)))
                               (cl-pushnew elem (arch-pkg-desc-optionalfor pr)))
                           (message "Package '%s' not found for optionalfor field" p))))))))
             arch-pkg-db)))

(define-derived-mode arch-pkg-list-mode tabulated-list-mode "Arch Package List"
  "Major mode for browsing a list of Archlinux packages.

\\{arch-pkg-list-mode-map}"
  (visual-line-mode +1)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Package" 36 t)
          ("Version" 15 t)
          ("Repository" 12 t)
          ("Status" 12 t)
          ("Date" 17 t)
          ("Size" 11 arch-pkg--size-predicate)
          ("Description" 0 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (let ((inhibit-message t))
    (toggle-truncate-lines +1))
  (setq revert-buffer-function 'arch-pkg-refresh))

(defun arch-pkg-refresh (&optional _arg _noconfirm)
  "Re-read database and list packages."
  (interactive)
  (arch-pkg--create-db)
  (arch-pkg-list-packages))

(defun arch-pkg-list--refresh ()
  "Re-populate the `tabulated-list-entries'."

  ;; create db if needed
  (unless arch-pkg-db
    (arch-pkg--create-db))

  (setq tabulated-list-entries nil)
  (maphash (lambda (name desc)
             (let ((face (if (null (arch-pkg-desc-repository desc))
                             'arch-pkg-status-installed-foreign
                           (nth (arch-pkg-desc-reason desc)
                                (list 'arch-pkg-status-installed
                                      'arch-pkg-status-dependency
                                      'arch-pkg-status-not-installed)))))
               (push (list name
                           (vector (list (arch-pkg-desc-name desc)
                                         'package-name (arch-pkg-desc-name desc)
                                         'follow-link t
                                         'action 'arch-pkg-list-describe-package
                                         'face 'arch-pkg-name
                                         'font-lock-face 'arch-pkg-name)
                                   (propertize (arch-pkg-desc-version desc) 'font-lock-face face)
                                   (propertize (or (arch-pkg-desc-repository desc) "") 'font-lock-face face)
                                   (propertize (arch-pkg--format-status (arch-pkg-desc-reason desc)) 'font-lock-face face)
                                   (propertize (arch-pkg--format-date (arch-pkg-desc-installdate desc)) 'font-lock-face face)
                                   (propertize (arch-pkg--format-size (or (arch-pkg-desc-isize desc)
                                                                          (arch-pkg-desc-size desc)))
                                               'font-lock-face face)
                                   (propertize (or (arch-pkg-desc-desc desc) "") 'font-lock-face 'arch-pkg-description)))
                     tabulated-list-entries)))
           arch-pkg-db)
  (sort tabulated-list-entries :in-place t))

(defun arch-pkg-list--display (&optional suffix)
  "Display the Arch Package List.
If SUFFIX is non-nil, append that to \"Package\" for the first
column in the header line."
  (setf (car (aref tabulated-list-format 0))
        (if suffix
            (concat "Package[" suffix "]")
          "Package"))
  (tabulated-list-init-header)
  (tabulated-list-print 'remember))

;;;###autoload
(defun arch-pkg-list-packages ()
  "Display a list of Arch Linux packages."
  (interactive)

  ;; create db if needed
  (unless arch-pkg-db
    (arch-pkg--create-db))

  ;; create buffer and display
  (let ((buf (get-buffer-create "*Arch Packages*")))
    (pop-to-buffer-same-window buf)
    (arch-pkg-list-mode)
    (arch-pkg-list--refresh)
    (arch-pkg-list--display)))

(defun arch-pkg-list--quick-help ()
  "Show short help for key bindings in `arch-pkg-list-mode'.
You can view the full list of keys with \\[describe-mode]."
  (interactive nil arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (let ((docs '("n - next"
                "p - previous"
                "g - refresh contents"
                "/ - filter")))
    (dolist (d docs)
      (add-text-properties 0 1 '(face help-key-binding) d))
    (message (string-join docs "\n"))))

(defun arch-pkg--ensure-pkg-list-mode ()
  "Signal a user-error if major mode is not `arch-pkg-list-mode'."
  (unless (derived-mode-p 'arch-pkg-list-mode)
    (user-error "The current buffer's mode is not Arch Package List Mode")))

(defun arch-pkg-list--filter-by (predicate suffix)
  "Filter \"*Arch Packages*\" buffer by PREDICATE and modify header with SUFFIX.
PREDICATE is a function which will be called with one argument, a
`pkg' `arch-pkg-desc', and returns non-nil if that object should be
listed in the Package Menu."
  (arch-pkg-list--refresh)
  (let ((found-entries '()))
    (dolist (entry tabulated-list-entries)
      (when (funcall predicate (arch-pkg--get-desc (car entry)))
        (push entry found-entries)))
    (if found-entries
        (progn
          (setq tabulated-list-entries (reverse found-entries))
          (arch-pkg-list--display suffix))
      (user-error "No packages found"))))

(defun arch-pkg-list-filter-by-name (name)
  "Filter the \"*Arch Packages*\" buffer by the regexp NAME.
Display only packages whose names match the regexp NAME.

When called interactively, prompt for NAME.

If NAME is nil or the empty string, show all packages."
  (interactive (list (read-regexp "Filter by name (regexp)"))
               arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (when (and name (not (string-empty-p name)))
    (arch-pkg-list--filter-by (lambda (pkg)
                                (string-match-p name (arch-pkg-desc-name pkg)))
                              (format "name:%s" name))))

(defun arch-pkg-list-filter-by-description (description)
  "Filter the \"*Arch Packages*\" buffer by the regexp DESCRIPTION.
Display only packages whose descriptions match the regexp
given as DESCRIPTION.

When called interactively, prompt for DESCRIPTION."
  (interactive (list (read-regexp "Filter by description (regexp)"))
               arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (when (and description (not (string-empty-p description)))
    (arch-pkg-list--filter-by (lambda (pkg)
                                (string-match-p description
                                                (arch-pkg-desc-desc pkg)))
                              (format "desc:%s" description))))

(defun arch-pkg-list-filter-by-name-or-description (name-or-description)
  "Filter the \"*Arch Packages*\" buffer by the regexp NAME-OR-DESCRIPTION.
Display only packages whose names or descriptions match the regexp
given as NAME-OR-DESCRIPTION.

When called interactively, prompt for NAME-OR-DESCRIPTION."
  (interactive (list (read-regexp "Filter by name or description (regexp)"))
               arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (when (and name-or-description (not (string-empty-p name-or-description)))
    (arch-pkg-list--filter-by (lambda (pkg)
                                (or (string-match-p name-or-description
                                                    (arch-pkg-desc-name pkg))
                                    (string-match-p name-or-description
                                                    (arch-pkg-desc-desc pkg))))
                              (format "desc:%s" name-or-description))))

(defun arch-pkg-list-filter-by-repo (repo)
  "Filter the \"*Arch Packages*\" buffer by the REPO name.
Display only packages whose repo matches the REPO.

When called interactively, prompt for REPO."
  (interactive (list (completing-read-multiple
                      "Filter by repository (comma separated): "
                      (let ((repos '()))
                        (maphash (lambda (_name pkg)
                                   (when-let* ((r (arch-pkg-desc-repository pkg)))
                                     (cl-pushnew r repos)))
                                 arch-pkg-db)
                        (sort repos :in-place t))))
               arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (arch-pkg-list--filter-by (lambda (pkg)
                              (let ((pkg-repo (arch-pkg-desc-repository pkg)))
                                (and pkg-repo (member pkg-repo repo))))
                            (concat "repo:" (string-join repo ","))))

(defun arch-pkg-list-clear-filter ()
  "Clear any filter currently applied to the \"*Arch Packages*\" buffer."
  (interactive nil arch-pkg-list-mode)
  (arch-pkg--ensure-pkg-list-mode)
  (arch-pkg-list-mode)
  (arch-pkg-list--refresh)
  (arch-pkg-list--display))

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

(defun arch-pkg-list-describe-package (&optional button)
  "Descibe package under BUTTON."
  (interactive nil arch-pkg-list-mode)
  (let ((pkg-name (if button (button-get button 'package-name)
                    (tabulated-list-get-id))))
    (if pkg-name
        (arch-pkg-describe-package pkg-name)
      (user-error "No package here"))))

;;;###autoload
(defun arch-pkg-describe-package (&optional package)
  "Display the full documentation of Archlinux package PACKAGE (`string' or `symbol')."
  (interactive)

  (unless package
    (unless arch-pkg-db
      (arch-pkg--create-db))
    (setq package (completing-read "Describe Arch package: "
                                   (hash-table-keys arch-pkg-db))))

  (setq package (intern (arch-pkg--extract-package-name
                         (if (stringp package)
                             package
                           (symbol-name package)))))

  (let ((desc (gethash package arch-pkg-db)))

    ;; not found, this may be a feature provided by some other packages
    (when (not desc)
      (when-let* ((pkgs (gethash package arch-pkg-providedby)))
        (if (cadr pkgs)
            ;; multiple choices, ask user
            (setq package (intern (completing-read (format "%s is provided by multiple packages: "
                                                           (symbol-name package))
                                                   (mapcar #'symbol-name pkgs) nil t)))
          ;; only one choice
          (setq package (car pkgs)))
        (setq desc (gethash package arch-pkg-db))))

    (when (not desc)
      (message "Package cannot be found: '%s'." package))

    (when desc
      (let ((width 17))
        (help-setup-xref (list #'arch-pkg-describe-package package)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (let ((inhibit-read-only t))
              (erase-buffer)
              (setq buffer-file-coding-system 'utf-8)

              (insert (arch-pkg--propertize (string-pad "Name: " width ?\s t)))
              (insert (arch-pkg-desc-name desc) "\n")

              (insert (arch-pkg--propertize (string-pad "Version: " width ?\s t)))
              (insert (arch-pkg-desc-version desc) "\n")

              (insert (arch-pkg--propertize (string-pad "Description: " width ?\s t)))
              (insert (arch-pkg-desc-desc desc) "\n")

              (insert (arch-pkg--propertize (string-pad "Upstream URL: " width ?\s t)))
              (let ((url (arch-pkg-desc-url desc)))
                (help-insert-xref-button url 'help-url url))
              (insert "\n")

              (when-let* ((repo (arch-pkg-desc-repository desc))
                          (arch (arch-pkg-desc-arch desc))
                          (name (arch-pkg-desc-name desc))
                          (pkg-url (format arch-pkg-package-url repo arch name)))
                (insert (arch-pkg--propertize (string-pad "Package URL: " width ?\s t)))
                (help-insert-xref-button pkg-url 'help-url pkg-url)
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Licenses: " width ?\s t)))
              (insert (string-join (arch-pkg-desc-licenses desc) ", ") "\n")

              (let ((status (arch-pkg-desc-reason desc)))
                (insert (arch-pkg--propertize (string-pad "Status: " width ?\s t)))
                (insert (arch-pkg--format-status status 'show-not-installed))
                (when (or (= status 0)
                          (= status 1))
                  (insert " (")
                  (help-insert-xref-button "files"
                                           'help-arch-package-files
                                           (arch-pkg-desc-name desc)
                                           (arch-pkg-desc-version desc))
                  (insert ")"))
                (cond
                 ((= status 0)    ; installed
                  (insert " -- ")
                  (arch-pkg--make-button "Delete"
                                         'action #'arch-pkg-delete-action
                                         'package-name package)
                  (insert "\n"))
                 ((= status 2)          ; not installed
                  (insert " -- ")
                  (arch-pkg--make-button "Install"
                                         'action #'arch-pkg-install-action
                                         'package-name package)
                  (insert "\n"))
                 (t                     ; status = 1, dependency
                  (insert "\n"))))

              (insert (arch-pkg--propertize (string-pad "Repository: " width ?\s t)))
              (insert (or (arch-pkg-desc-repository desc) "") "\n")

              (when-let* ((grp (arch-pkg-desc-groups desc)))
                (insert (arch-pkg--propertize (string-pad "Groups: " width ?\s t)))
                (insert (string-join grp " ") "\n"))

              (when-let* ((prs (arch-pkg-desc-provides desc)))
                (insert (arch-pkg--propertize (string-pad "Provides: " width ?\s t)))
                (dolist (pr prs)
                  (help-insert-xref-button pr 'help-arch-package pr)
                  (insert " "))
                (delete-char -1)
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Depends on: " width ?\s t)))
              (if-let* ((deps (sort (arch-pkg-desc-depends desc))))
                  (progn
                    (dolist (dep deps)
                      (help-insert-xref-button dep (if (arch-pkg--dep-satisfied-p dep)
                                                       'help-arch-package-installed
                                                     'help-arch-package)
                                               dep)
                      (insert " "))
                    (insert "(")
                    (help-insert-xref-button "dependency tree"
                                             'help-arch-package-dep-tree
                                             (arch-pkg-desc-name desc))
                    (insert ")"))
                (insert "None"))
              (insert "\n")

              (when-let* ((opts (arch-pkg-desc-optdepends desc)))
                (insert (arch-pkg--propertize (string-pad "Optional: " width ?\s t)))
                (dolist (opt opts)
                  (let ((splitted (split-string opt ": ")))
                    (help-insert-xref-button (car splitted)
                                             (if (arch-pkg--installed-p (car splitted))
                                                 'help-arch-package-installed
                                               'help-arch-package)
                                             (car splitted))
                    (when (cadr splitted)
                      (insert ": " (cadr splitted)))
                    (insert "\n" (make-string width ?\s))))
                (delete-line)
                (delete-char -1)
                (insert "\n"))

              (let ((reqs (sort (seq-filter #'symbolp (arch-pkg-desc-requiredby desc))))
                    (reqs-ind (sort (seq-group-by #'car (seq-filter #'consp (arch-pkg-desc-requiredby desc))))))
                (when (or reqs reqs-ind)
                  (insert (arch-pkg--propertize (string-pad "Required by: " width ?\s t)))
                  ;; required by
                  (when reqs
                    (dolist (req reqs)
                      (help-insert-xref-button (symbol-name req)
                                               (if (arch-pkg--installed-p req)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               req)
                      (insert " "))
                    (delete-char -1))
                  ;; indirect required by
                  (dolist (req reqs-ind)
                    (let ((p (car req)))
                      (insert " (through ")
                      (help-insert-xref-button (symbol-name p)
                                               (if (arch-pkg--installed-p p)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               p)
                      (insert ": "))
                    (dolist (p (sort (mapcar #'cdr (cdr req))))
                      (help-insert-xref-button (symbol-name p)
                                               (if (arch-pkg--installed-p p)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               p)
                      (insert " "))
                    (delete-char -1)
                    (insert ")"))
                  (insert "\n")))

              (let ((opts (sort (seq-filter #'symbolp (arch-pkg-desc-optionalfor desc))))
                    (opts-ind (sort (seq-group-by #'car (seq-filter #'consp (arch-pkg-desc-optionalfor desc))))))
                (when (or opts opts-ind)
                  (insert (arch-pkg--propertize (string-pad "Optional for: " width ?\s t)))
                  ;; optional dependencies
                  (when opts
                    (dolist (opt opts)
                      (help-insert-xref-button (symbol-name opt)
                                               (if (arch-pkg--installed-p opt)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               opt)
                      (insert " "))
                    (delete-char -1))
                  ;; indirect optional dependencies
                  (dolist (opt opts-ind)
                    (let ((p (car opt)))
                      (insert " (through ")
                      (help-insert-xref-button (symbol-name p)
                                               (if (arch-pkg--installed-p p)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               p)
                      (insert ": "))
                    (dolist (p (sort (mapcar #'cdr (cdr opt))))
                      (help-insert-xref-button (symbol-name p)
                                               (if (arch-pkg--installed-p p)
                                                   'help-arch-package-installed
                                                 'help-arch-package)
                                               p)
                      (insert " "))
                    (delete-char -1)
                    (insert ") "))
                  (insert "\n")))

              (when-let* ((cnf (arch-pkg-desc-conflicts desc)))
                (insert (arch-pkg--propertize (string-pad "Conflicts with: " width ?\s t)))
                (dolist (c cnf)
                  (help-insert-xref-button c
                                           (if (arch-pkg--installed-p c)
                                               'help-arch-package-installed
                                             'help-arch-package)
                                           c)
                  (insert " "))
                (delete-char -1)
                (insert "\n"))

              (when-let* ((rpl (arch-pkg-desc-replaces desc)))
                (insert (arch-pkg--propertize (string-pad "Replaces: " width ?\s t)))
                (dolist (r rpl)
                  (help-insert-xref-button r
                                           (if (arch-pkg--installed-p r)
                                               'help-arch-package-installed
                                             'help-arch-package)
                                           r)
                  (insert " "))
                (delete-char -1)
                (insert "\n"))

              (insert (arch-pkg--propertize (string-pad "Architecture: " width ?\s t)))
              (insert (arch-pkg-desc-arch desc) "\n")

              (insert (arch-pkg--propertize (string-pad "Maintainer: " width ?\s t)))
              (insert (arch-pkg-desc-packager desc) "\n")

              (insert (arch-pkg--propertize (string-pad "Build date: " width ?\s t)))
              (insert (arch-pkg--format-date (arch-pkg-desc-builddate desc)) "\n")

              (when-let* ((idate (arch-pkg-desc-installdate desc)))
                (insert (arch-pkg--propertize (string-pad "Install date: " width ?\s t)))
                (insert (arch-pkg--format-date idate) "\n"))

              (when-let* ((isize (or (arch-pkg-desc-size desc)
                                     (arch-pkg-desc-isize desc))))
                (insert (arch-pkg--propertize (string-pad "Install size: " width ?\s t)))
                (insert (arch-pkg--format-size isize) "\n"))

              (when-let* ((csize (arch-pkg-desc-csize desc)))
                (insert (arch-pkg--propertize (string-pad "Download size: " width ?\s t)))
                (insert (arch-pkg--format-size csize) "\n"))

              (when-let* ((val (arch-pkg-desc-validation desc)))
                (insert (arch-pkg--propertize (string-pad "Validation: " width ?\s t)))
                (insert val "\n")))))))))

(defun arch-pkg--post-refresh ()
  "Perform refreshes after an action."
  (let ((buf (get-buffer "*Arch Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (arch-pkg--create-db)
        (arch-pkg-list--refresh)
        (arch-pkg-list--display))))
  (let ((buf (get-buffer "*Help*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (revert-buffer)))))

(defun arch-pkg--run-command (name command)
  "Run COMMAND with the NAME.
When the command finishes updates the package list if needed.  Used for actions."
  (let* ((buf "*arch-pkg-command*")
         (process (start-process-shell-command name buf command)))
    (set-process-filter process #'comint-output-filter)
    (pop-to-buffer-same-window buf)
    (with-current-buffer buf
      (shell-command-mode))
    (set-process-sentinel
     process
     (lambda (process _event)
       (when (eq (process-status process) 'exit)
         (arch-pkg--post-refresh))))))

(defun arch-pkg-delete-package (package)
  "Run delete command for given PACKAGE."
  (arch-pkg--run-command "arch-pkg-delete"
                         (format arch-pkg-delete-command package)))

(defun arch-pkg-delete-action (button)
  "Delete action for BUTTON in help."
  (let ((pkg-name (button-get button 'package-name)))
    (when (y-or-n-p (format-message "Delete package `%s'? " pkg-name))
      (arch-pkg-delete-package pkg-name))))

(defun arch-pkg-install-package (package)
  "Run install command for given PACKAGE."
  (arch-pkg--run-command "arch-pkg-install"
                         (format arch-pkg-install-command package)))

(defun arch-pkg-install-aur-package (package)
  "Run install command for given PACKAGE."
  (arch-pkg--run-command "arch-pkg-install"
                         (format arch-pkg-aur-install-command package)))

(defun arch-pkg-install-action (button)
  "Install action for BUTTON in help."
  (let ((pkg-name (button-get button 'package-name)))
    (when (y-or-n-p (format-message "Install package `%s'? " pkg-name))
      (arch-pkg-install-package pkg-name))))

(defun arch-pkg-aur-install-action (button)
  "Install action for BUTTON in help."
  (let ((pkg-name (button-get button 'package-name)))
    (when (y-or-n-p (format-message "Install package `%s'? " pkg-name))
      (arch-pkg-install-aur-package pkg-name))))

(defun arch-pkg--get-package-files (package version)
  "Get files owned by PACKAGE with VERSION."
  (let ((filename (file-name-concat arch-pkg-local-db-path
                                    (concat package "-" version)
                                    "files"))
        (files '()))
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (search-forward "%FILES%" nil 'NOERROR)
        (forward-line)
        (let ((cont t))
          (while cont
            (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
              (cond
               ((string-empty-p line)
                (setq cont nil))
               ((not (string-suffix-p "/" line))
                (push (concat "/" line) files)))
              (forward-line))))))
    (reverse files)))

(defun arch-pkg-show-files (package version)
  "Show files for PACKAGE with VERSION."
  (let* ((files (arch-pkg--get-package-files package version)))
    (when files
      (help-setup-xref (list #'arch-pkg-show-files package version)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (format "Files owned by %s-%s:\n\n" package version))
          (dolist (file files)
            (insert-text-button file
                                'type 'help-arch-package-files
                                'action #'arch-pkg--find-file
                                'file file
                                'follow-link t)
            (insert "\n")))))))

(defun arch-pkg--find-file (&optional button)
  "Find file from the BUTTON or the current line."
  (interactive)
  (let ((filename (if button (button-get button 'file)
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))
    (unless (string-empty-p filename)
      (find-file filename))))

(defun arch-pkg--dep-tree-expander (tree)
  "Expand TREE by returning the children.  For dependency trees."
  (when-let* ((pkg (widget-get tree :tag))
              (desc (arch-pkg--get-desc pkg))
              (children (sort (arch-pkg-desc-depends desc))))
    (mapcar (lambda (val)
              `(tree-widget
                :tag ,val
                :expander arch-pkg--dep-tree-expander))
            children)))

(defun arch-pkg-show-dependency-tree (&optional pkg)
  "Show dependencies of PKG as a tree."
  (interactive)
  (unless arch-pkg-db
    (arch-pkg--create-db))
  (unless pkg
    (setq pkg (completing-read "Dependency tree for Arch package: "
                               (hash-table-keys arch-pkg-db))))
  (when-let* ((desc (arch-pkg--get-desc pkg))
              (pkg-name (arch-pkg-desc-name desc)))
    (help-setup-xref (list #'arch-pkg-show-dependency-tree pkg)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Dependencies of %s:\n\n" pkg-name))
          (widget-create 'tree-widget
                         :tag pkg-name
                         :open t
                         :expander #'arch-pkg--dep-tree-expander))))))

(define-derived-mode arch-pkg-aur-list-mode tabulated-list-mode "AUR Package List"
  "Major mode for browsing a list of packages in AUR Search results.

\\{arch-pkg-aur-list-mode}"
  (visual-line-mode +1)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Name" 36 t)
          ("Version" 15 t)
          ("Votes" 12 t)
          ("Popularity" 12 t)
          ("Last Updated" 17 t)
          ("Description" 0 t)])
  (setq tabulated-list-padding 2)
  (let ((inhibit-message t))
    (toggle-truncate-lines +1)))

(defun arch-pkg--aur-info-cb (status package)
  "Callback of `url-retrieve' for AUR info.  Evaluates STATUS and PACKAGE."
  (let ((err (plist-get status :error)))
    (when err
      (error "Fetch failed")
      (pp err)))

  (goto-char (point-min))
  ;; skip mime headers
  (forward-paragraph)

  (let ((obj (ignore-errors (json-parse-buffer))))
    (unless obj
      (error "Json parsing failed"))
    (when (string= (gethash "type" obj)
                   "error")
      (error "Query returned error"))
    (let ((results (gethash "results" obj)))
      (unless results
        (error "Json does not contain 'results' key"))
      (unless (equal (length results) 1)
        (error "No results or multiple results"))

      (when-let* ((pkg (aref results 0)))
        (help-setup-xref (list #'arch-pkg-aur-describe-package package)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (let ((inhibit-read-only t)
                  (width 22))
              (erase-buffer)
              (setq buffer-file-coding-system 'utf-8)

              (insert (arch-pkg--propertize (string-pad "Name: " width ?\s t)))
              (insert (gethash "Name" pkg))
              (insert " -- ")
              (arch-pkg--make-button "Install"
                                     'action #'arch-pkg-aur-install-action
                                     'package-name package)
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "Version: " width ?\s t)))
              (insert (gethash "Version" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Description: " width ?\s t)))
              (insert (gethash "Description" pkg) "\n")

              (insert (arch-pkg--propertize (string-pad "Upstream URL: " width ?\s t)))
              (let ((url (gethash "URL" pkg)))
                (help-insert-xref-button url 'help-url url))
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "AUR URL: " width ?\s t)))
              (let ((url (concat "https://aur.archlinux.org/packages/"
                                 (gethash "Name" pkg))))
                (help-insert-xref-button url 'help-url url))
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "Git Clone URL: " width ?\s t)))
              (let ((url (concat "https://aur.archlinux.org/"
                                 (gethash "Name" pkg))))
                (help-insert-xref-button url 'help-url url))
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "Licenses: " width ?\s t)))
              (insert (string-join (gethash "License" pkg) ", ") "\n")

              (insert (arch-pkg--propertize (string-pad "Dependencies: " width ?\s t)))
              (if-let* ((deps (sort (gethash "Depends" pkg))))
                  (progn
                    (unless arch-pkg-db
                      (arch-pkg--create-db))
                    (mapc (lambda (dep)
                            (help-insert-xref-button dep (if (arch-pkg--dep-satisfied-p dep)
                                                             'help-aur-package-installed
                                                           'help-aur-package)
                                                     dep)
                            (insert " "))
                          deps)
                    (delete-char -1))
                (insert "None"))
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "Build Dependencies: " width ?\s t)))
              (if-let* ((deps (sort (gethash "MakeDepends" pkg))))
                  (progn
                    (unless arch-pkg-db
                      (arch-pkg--create-db))
                    (mapc
                     (lambda (dep)
                       (help-insert-xref-button dep (if (arch-pkg--dep-satisfied-p dep)
                                                        'help-aur-package-installed
                                                      'help-aur-package)
                                                dep)
                       (insert " "))
                     deps))
                (insert "None"))
              (insert "\n")

              (insert (arch-pkg--propertize (string-pad "Number of Votes: " width ?\s t)))
              (insert (number-to-string (gethash "NumVotes" pkg)) "\n")

              (insert (arch-pkg--propertize (string-pad "First Submitted: " width ?\s t)))
              (insert (arch-pkg--format-date (gethash "FirstSubmitted" pkg)) "\n")

              (insert (arch-pkg--propertize (string-pad "Last Updated: " width ?\s t)))
              (insert (arch-pkg--format-date (gethash "LastModified" pkg)) "\n")

              (insert (arch-pkg--propertize (string-pad "Maintainer: " width ?\s t)))
              (let ((maintainer (gethash "Maintainer" pkg)))
                (if (eq maintainer :null)
                    (insert "None")
                  (insert (gethash "Maintainer" pkg) "\n"))))))))))

(defun arch-pkg-aur-list-describe-package (&optional button)
  "Describe package given in BUTTON.
To be used by `arch-pkg-aur-list-mode'."
  (interactive nil arch-pkg-aur-list-mode)
  (let ((pkg-name (if button (button-get button 'package-name)
                    (tabulated-list-get-id))))
    (if pkg-name
        (arch-pkg-aur-describe-package pkg-name)
      (user-error "No package here"))))

;;;###autoload
(defun arch-pkg-aur-describe-package (&optional package)
  "Describe AUR PACKAGE (string) details."
  (interactive)

  (unless package
    (setq package (read-from-minibuffer "Package Name: ")))

  (let ((pkg (arch-pkg--extract-package-name package)))
    ;; check db first
    (if (arch-pkg--get-desc pkg)
        (arch-pkg-describe-package pkg)
      ;; query AUR for package
      (let ((url (format arch-pkg-aur-info-url pkg)))
        (url-retrieve url #'arch-pkg--aur-info-cb (list package) t)))))

(defun arch-pkg--aur-search-cb (status query)
  "Callback of `url-retrieve' for AUR search.  Evaluates STATUS and QUERY."
  (let ((err (plist-get status :error)))
    (when err
      (error "Fetch failed")
      (pp err)))

  (goto-char (point-min))
  ;; skip mime headers
  (forward-paragraph)

  (let ((obj (ignore-errors (json-parse-buffer))))
    (unless obj
      (error "Json parsing failed"))

    (when (string= (gethash "type" obj)
                   "error")
      (error "Query returned error"))

    (let ((aur-list '())
          (results (gethash "results" obj)))

      (unless results
        (error "Json does not contain 'results' key"))

      (setq arch-pkg-aur-db (sort results
                                  :lessp (lambda (p1 p2) (< (gethash "NumVotes" p1)
                                                            (gethash "NumVotes" p2)))
                                  :in-place t))
      (mapc (lambda (pkg)
              (push (list (gethash "Name" pkg)
                          (vector (cons (gethash "Name" pkg)
                                        (list
                                         'action
                                         (lambda (but)
                                           (arch-pkg-aur-describe-package
                                            (button-label but)))))
                                  (gethash "Version" pkg)
                                  (format "%d" (gethash "NumVotes" pkg))
                                  (format "%f" (gethash "Popularity" pkg))
                                  (arch-pkg--format-date (gethash "LastModified" pkg))
                                  (let ((desc (gethash "Description" pkg)))
                                    (if (eq desc :null) "" desc))))
                    aur-list))
            arch-pkg-aur-db)

      (let ((buf (get-buffer-create (format "*AUR Search Results: %s (%d)*"
                                            query
                                            (gethash "resultcount" obj)))))
        (pop-to-buffer-same-window buf)
        (arch-pkg-aur-list-mode)
        (setq tabulated-list-entries aur-list)
        (tabulated-list-init-header)
        (tabulated-list-print)))))

;;;###autoload
(defun arch-pkg-aur-search (query)
  "Search AUR repository for given QUERY."
  (interactive "sEnter query: ")
  (let ((url (format arch-pkg-aur-search-url (url-hexify-string query))))
    (url-retrieve url #'arch-pkg--aur-search-cb (list query) t)))


(provide 'arch-pkg)
;;; arch-pkg.el ends here
