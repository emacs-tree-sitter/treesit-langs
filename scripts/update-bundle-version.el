;;; update-bundle-version.el --- Update the bundle version  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")

(let ((ver (or (getenv "BUNDLE_VER")
               "0.1.0")))
  (with-current-buffer (find-file "treesit-langs.el")
    (goto-char (point-min))
    (when (search-forward "(defcustom treesit-langs-bundle-version \"" nil t)
      (let ((start (point))
            (end   (save-excursion (forward-sexp) (point))))
        (delete-region start end)
        (insert ver)
        (save-buffer))
      (message "[INFO] Updated the bundle version to `%s`" ver))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; update-bundle-version.el ends here
