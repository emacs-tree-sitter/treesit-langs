;; -*- mode: eask; lexical-binding: t -*-

(package "treesit-langs"
         "0.1.0"
         "Language bundle for Emacs's treesit.el")

(website-url "https://github.com/emacs-tree-sitter/treesit-langs")
(keywords "languages" "tools" "parsers" "tree-sitter")

(package-file "treesit-langs.el")

(script "test" "echo \"Error: no test specified\" && exit 1")
(script "latest-tag" "eask load scripts/latest-tag.el")
(script "update-bundle-version" "eask load scripts/update-bundle-version.el")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(depends-on "emacs" "29.1")

(development
 (depends-on "elenv")
 (depends-on "github-tags"))
