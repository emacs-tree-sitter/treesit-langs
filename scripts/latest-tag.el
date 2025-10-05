;;; latest-tag.el --- Print latest tag  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(load-file "./scripts/_prepare.el")

(princ (get-latest-tag))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; latest-tag.el ends here
