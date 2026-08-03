;;; arch-pkg-test.el --- Tests for arch-pkg  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Hikmet Altıntaş

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)

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

;;; Code:

(require 'ert)
(require 'seq)
(require 'arch-pkg)

(ert-deftest arch-pkg-test-parse-depends-str ()
  (should (equal (arch-pkg--parse-depends-str "libfreetype.so=6-64")
                 '("libfreetype.so" "=" "6-64")))
  (should (equal (arch-pkg--parse-depends-str "jre26-openjdk-headless=26.0.2.u10-1")
                 '("jre26-openjdk-headless" "=" "26.0.2.u10-1")))
  (should (equal (arch-pkg--parse-depends-str "java-runtime-common>=3")
                 '("java-runtime-common" ">=" "3")))
  (should (equal (arch-pkg--parse-depends-str "glibc")
                 '("glibc" "" ""))))

(ert-deftest arch-pkg-test-get-desc ()
  ;; create db if needed
  (unless arch-pkg-db
    (arch-pkg--create-db))
  (should (arch-pkg-desc-p (arch-pkg--get-desc "glibc")))
  (should (arch-pkg-desc-p (arch-pkg--get-desc 'glibc)))
  (should (arch-pkg-desc-p (arch-pkg--get-desc (assq 'glibc arch-pkg-db))))
  (should (arch-pkg-desc-p (arch-pkg--get-desc (cdr (assq 'glibc arch-pkg-db))))))

(ert-deftest arch-pkg-test-read-desc-file ()
  (let* ((desc-file (file-name-concat
                     (seq-random-elt
                      (seq-drop (directory-files arch-pkg-local-db-path t)
                                2))
                     "desc"))
         (desc (arch-pkg--read-desc-file desc-file)))
    (should (not (null desc)))
    (should (stringp (arch-pkg-desc-name desc)))
    (should (stringp (arch-pkg-desc-version desc)))
    (should (stringp (arch-pkg-desc-desc desc)))
    (should (listp (arch-pkg-desc-licenses desc)))))
