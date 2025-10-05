;;; _prepare.el --- Prepration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'elenv)

(defun get-latest-tag ()
  "Return the latest tag (not including pre-release)."
  (require 'github-tags)
  (if-let* ((repo "emacs-tree-sitter/tree-sitter-langs")
            (response (cdr (github-tags repo)))
            (tags (plist-get response :names))
            (latest (nth 1 tags)))  ; Skip the first one since it's the pre-release!
      latest
    (user-error "[ERROR] Latest tag not found in repository: %s" repo)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; _prepare.el ends here
