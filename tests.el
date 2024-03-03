(arch-pkg--create-db)

(let ((buf (get-buffer-create "*provided by*")))
  (with-current-buffer buf
    (erase-buffer)
    (insert "* arch-pkg-providedby* \n")
    (insert "* provided by: (package1 package2)\n")
    (maphash (lambda (k v)
               (insert (format "%s: %s\n" k v)))
             arch-pkg-providedby)
    (text-mode)
    (pop-to-buffer buf)))

(let ((buf (get-buffer-create "*depends*")))
  (with-current-buffer buf
    (erase-buffer)
    (insert "* dependencies* \n\n")
    (maphash (lambda (name pkg)
               (let ((deps (gethash "DEPENDS" pkg)))
                 (insert (format "%s: %s\n" name deps))))
             arch-pkg-db)
    (text-mode)
    (pop-to-buffer buf)))

(dolist (d (gethash "REQUIREDBY" (gethash 'gpgme arch-pkg-db)))
  (insert (format "%s\n" d)))


(gethash "REQUIREDBY" (gethash 'cl-clx arch-pkg-db))
(gethash "REQUIREDBY" (gethash 'clisp arch-pkg-db))
(symbolp (car (gethash "REQUIREDBY" (gethash 'cl-clx arch-pkg-db))))
