;;; treesit-langs.el --- Language bundle for Emacs's treesit.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-tree-sitter/treesit-langs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (tree-sitter-langs "0.12.18"))
;; Keywords: languages tools parsers tree-sitter

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Language bundle for Emacs's treesit.el.
;;

;;; Code:

(require 'cl-lib)
(require 'treesit)

(require 'tree-sitter-langs-build)

(defgroup treesit-langs nil
  "Grammar bundle for `treesit.el'."
  :group 'tree-sitter)

(defcustom treesit-langs-major-mode-alist
  '((actionscript-mode      . actionscript)
    (ada-mode               . ada)
    (agda-mode              . agda)
    (agda2-mode             . agda)
    (arduino-mode           . arduino)
    (astro-mode             . astro)
    (fish-mode              . fish)
    (asm-mode               . asm)
    (fasm-mode              . asm)
    (masm-mode              . asm)
    (nasm-mode              . asm)
    (gas-mode               . asm)
    (sh-mode                . bash)
    (beancount-mode         . beancount)
    (bibtex-mode            . bibtex)
    (c-mode                 . c)
    (caml-mode              . ocaml)
    (clojure-mode           . clojure)
    (lisp-mode              . commonlisp)
    (lisp-interaction-mode  . commonlisp)
    (csharp-mode            . c-sharp)
    (c++-mode               . cpp)
    (cmake-mode             . cmake)
    (d-mode                 . d)
    (dart-mode              . dart)
    (dockerfile-mode        . dockerfile)
    (css-mode               . css)
    (csv-mode               . csv)
    (elm-mode               . elm)
    (elixir-mode            . elixir)
    (emacs-lisp-mode        . elisp)
    (erlang-mode            . erlang)
    (ess-r-mode             . r)
    (fennel-mode            . fennel)
    (f90-mode               . fortran)
    (fortran-mode           . fortran)
    (gdscript-mode          . gdscript)
    (git-commit-mode        . gitcommit)
    (git-rebase-mode        . git-rebase)
    (gitattributes-mode     . gitattributes)
    (gitignore-mode         . gitignore)
    (gleam-mode             . gleam)
    (glsl-mode              . glsl)
    (go-mode                . go)
    (groovy-mode            . groovy)
    (jenkinsfile-mode       . groovy)
    (haskell-mode           . haskell)
    (haxe-mode              . haxe)
    (hcl-mode               . hcl)
    (terraform-mode         . hcl)
    (heex-mode              . heex)
    (hlsl-mode              . hlsl)
    (html-mode              . html)
    (markdown-mode          . markdown)
    (mhtml-mode             . html)
    (nix-mode               . nix)
    (jai-mode               . jai)
    (java-mode              . java)
    (javascript-mode        . javascript)
    (js-mode                . javascript)
    (js2-mode               . javascript)
    (js3-mode               . javascript)
    (json-mode              . json)
    (jsonc-mode             . json)
    (jsonnet-mode           . jsonnet)
    (julia-mode             . julia)
    (kotlin-mode            . kotlin)
    (latex-mode             . latex)
    (LaTeX-mode             . latex)
    (llvm-mode              . llvm)
    (llvm-mir-mode          . llvm-mir)
    (lua-mode               . lua)
    (magik-mode             . magik)
    (makefile-mode          . make)
    (makefile-automake-mode . make)
    (makefile-gmake-mode    . make)
    (makefile-makepp-mode   . make)
    (makefile-bsdmake-mode  . make)
    (makefile-imake-mode    . make)
    (matlab-mode            . matlab)
    (mermaid-mode           . mermaid)
    (meson-mode             . meson)
    (ninja-mode             . ninja)
    (noir-mode              . noir)
    (ocaml-mode             . ocaml)
    (org-mode               . org)
    (pascal-mode            . pascal)
    (perl-mode              . perl)
    (purescript-mode        . purescript)
    (cperl-mode             . perl)
    (php-mode               . php)
    (qss-mode               . css)
    (prisma-mode            . prisma)
    (python-mode            . python)
    (pygn-mode              . pgn)
    (racket-mode            . racket)
    (rjsx-mode              . javascript)
    (rst-mode               . rst)
    (ruby-mode              . ruby)
    (rust-mode              . rust)
    (rustic-mode            . rust)
    (scala-mode             . scala)
    (scheme-mode            . scheme)
    (solidity-mode          . solidity)
    (smithy-mode            . smithy)
    (sql-mode               . sql)
    (svelte-mode            . svelte)
    (swift-mode             . swift)
    (tablegen-mode          . tablegen)
    (toml-mode              . toml)
    (conf-toml-mode         . toml)
    (tcl-mode               . tcl)
    (tuareg-mode            . ocaml)
    (twig-mode              . twig)
    (typescript-mode        . typescript)
    (typescript-tsx-mode    . tsx)
    (typst-mode             . typst)
    (verilog-mode           . verilog)
    (vhdl-mode              . vhdl)
    (nxml-mode              . xml)
    (yaml-mode              . yaml)
    (k8s-mode               . yaml)
    (zig-mode               . zig))
  "Mapping from major-mode to the language parser."
  :type 'alist
  :group 'treesit-langs)

(defcustom treesit-langs-major-mode-setup-hook nil
  "Hook run after major mode is setup."
  :type 'hook
  :group 'treesit-langs)

(defun treesit-langs--grammar-files (new-bin)
  "Return grammar files from NEW-BIN."
  (cl-remove-if-not (lambda (file)
                      (or (string-match-p ".so"    file)
                          (string-match-p ".dylib" file)
                          (string-match-p ".dll"   file)))
                    (directory-files new-bin t)))

;;;###autoload
(defun treesit-langs-install-grammars ()
  "Install grammars to treesit.el library location."
  (interactive)
  (let ((bin (tree-sitter-langs--bin-dir))
        (new-bin (locate-user-emacs-file "tree-sitter")))
    (when (and (file-directory-p bin)
               (not (file-directory-p new-bin)))
      (copy-directory bin new-bin nil t t)
      (dolist (filename (treesit-langs--grammar-files new-bin))
        (let* ((dir   (file-name-directory filename))
               (file  (file-name-nondirectory filename))
               (lang  (file-name-sans-extension file))
               (soext (car dynamic-library-suffixes))
               (new-file (expand-file-name (concat "libtree-sitter-" lang soext)
                                           dir)))
          (rename-file filename new-file))))))

;; Install only once.
(treesit-langs-install-grammars)

(defun treesit-langs-major-mode-setup ()
  "Activate tree-sitter to power major-mode features."
  (when-let* ((lang (alist-get major-mode treesit-langs-major-mode-alist))
              ((treesit-language-available-p lang))
              ((ignore-errors (treesit-parser-create lang))))
    (treesit-major-mode-setup)
    (run-hooks 'treesit-langs-major-mode-setup-hook)))

(provide 'treesit-langs)
;;; treesit-langs.el ends here
