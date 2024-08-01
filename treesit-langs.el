;;; treesit-langs.el --- Language bundle for Emacs's treesit.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-tree-sitter/treesit-langs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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

(defcustom treesit-langs-bundle-version "0.12.208"
  "Version of the grammar bundle.

Ideally, we want this value to be same as `tree-sitter-langs--bundle-version'
from `tree-sitter-langs' package."
  :type 'string
  :group 'treesit-langs)

(defconst treesit-langs--bundle-version-file "BUNDLE-VERSION")

(defconst treesit-langs--suffixes '(".dylib" ".dll" ".so")
  "List of suffixes for shared libraries that define tree-sitter languages.")

(defconst treesit-langs--os
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('android "linux")
    ('berkeley-unix "freebsd")
    ('windows-nt "windows")
    (_ (error "Unsupported system-type %s" system-type))))

(defvar treesit-langs--out nil)

;;
;;; Externals

(declare-function dired-omit-mode "dired-x" (&optional arg))

;;
;;; Uitl

;;; TODO: Use (maybe make) an async library, with a proper event loop, instead
;;; of busy-waiting.
(defun treesit-langs--call (program &rest args)
  "Call PROGRAM with ARGS, using BUFFER as stdout+stderr.
If BUFFER is nil, `princ' is used to forward its stdout+stderr."
  (let* ((command `(,program . ,args))
         (_ (message "[treesit-langs] Running %s in %s" command default-directory))
         (base `(:name ,program :command ,command))
         (output (if treesit-langs--out
                     `(:buffer ,treesit-langs--out)
                   `(:filter (lambda (proc string)
                               (princ string)))))
         (proc (apply #'make-process (append base output)))
         (exit-code (progn
                      (while (not (memq (process-status proc)
                                        '(exit failed signal)))
                        (sleep-for 0.1))
                      (process-exit-status proc)))
         ;; Flush buffered output. Not doing this caused
         ;; `tree-sitter-langs-git-dir' to be set incorrectly, and
         ;; `tree-sitter-langs-create-bundle's output to be unordered.
         (_ (accept-process-output proc)))
    (unless (= exit-code 0)
      (error "Error calling %s, exit code is %s" command exit-code))))

;;
;;; Download

(defun treesit-langs--bundle-file (&optional ext version os)
  "Return the grammar bundle file's name, with optional EXT.

If VERSION and OS are not spcified, use the defaults of
`treesit-langs-bundle-version' and `treesit-langs--os'."
  (setq os (or os treesit-langs--os)
        version (or version treesit-langs-bundle-version)
        ext (or ext ""))
  (format "tree-sitter-grammars.%s.v%s.tar%s"
          ;; FIX: Implement this correctly, refactoring 'OS' -> 'platform'.
          (pcase os
            ("windows" "x86_64-pc-windows-msvc")
            ("linux"   "x86_64-unknown-linux-gnu")
            ("freebsd" "x86_64-unknown-freebsd")
            ("macos"   (if (string-prefix-p "aarch64" system-configuration)
                           "aarch64-apple-darwin"
                         "x86_64-apple-darwin")))
          version ext))

(defun treesit-langs--bundle-url (&optional version os)
  "Return the URL to download the grammar bundle.
If VERSION and OS are not specified, use the defaults of
`treesit-langs-bundle-version' and `treesit-langs--os'."
  (format "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/%s/%s"
          version
          (treesit-langs--bundle-file ".gz" version os)))

(defun treesit-langs--bin-dir ()
  "Return the directory to stored grammar binaries."
  (locate-user-emacs-file "tree-sitter"))

;;;###autoload
(defun treesit-langs-install-grammars (&optional skip-if-installed version os keep-bundle)
  "Download and install the specified VERSION of the language grammar bundle.
If VERSION or OS is not specified, use the default of
`treesit-langs-bundle-version' and `treesit-langs--os'.

This installs the grammar bundle even if the same version was already installed,
unless SKIP-IF-INSTALLED is non-nil.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil."
  (interactive (list
                nil
                (read-string "Bundle version: " treesit-langs-bundle-version)
                treesit-langs--os
                nil))
  (let* ((bin-dir (treesit-langs--bin-dir))
         (_ (unless (unless (file-directory-p bin-dir)
                      (make-directory bin-dir))))
         (version (or version treesit-langs-bundle-version))
         (default-directory bin-dir)
         (bundle-file (treesit-langs--bundle-file ".gz" version os))
         (current-version (when (file-exists-p
                                 treesit-langs--bundle-version-file)
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents
                                 treesit-langs--bundle-version-file)
                                (string-trim (buffer-string)))))))
    (cl-block nil
      (if (string= version current-version)
          (if skip-if-installed
              (progn (message "treesit-langs: Grammar bundle v%s was already installed; skipped" version)
                     (cl-return))
            (message "treesit-langs: Grammar bundle v%s was already installed; reinstalling" version))
        (message "treesit-langs: Installing grammar bundle v%s (was v%s)" version current-version))
      ;; FIX: Handle HTTP errors properly.
      (url-copy-file (treesit-langs--bundle-url version os)
                     bundle-file 'ok-if-already-exists)
      (treesit-langs--call "tar" "-xvzf" bundle-file)
      ;; FIX: This should be a metadata file in the bundle itself.
      (with-temp-file treesit-langs--bundle-version-file
        (let ((coding-system-for-write 'utf-8))
          (insert version)))
      (unless keep-bundle
        (delete-file bundle-file 'trash))
      (when (and (called-interactively-p 'any)
                 (y-or-n-p (format "Show installed grammars in %s? " bin-dir)))
        (with-current-buffer (find-file bin-dir)
          (when (bound-and-true-p dired-omit-mode)
            (dired-omit-mode -1))))
      (treesit-langs--rename))))

;; Install only once.
(treesit-langs-install-grammars :skip-if-installed)

;;
;;; Rename

(defun treesit-langs--grammar-files (bin)
  "Return grammar files from BIN."
  (cl-remove-if-not (lambda (file)
                      (cl-some (lambda (suffix)
                                 (string-suffix-p suffix file))
                               treesit-langs--suffixes))
                    (directory-files bin t)))

(defun treesit-langs--rename ()
  "Rename installed grammars to `treesit.el' compatible cnaming convention."
  (interactive)
  (when-let* ((bin (treesit-langs--bin-dir))
              ((file-directory-p bin)))
    (dolist (filename (treesit-langs--grammar-files bin))
      (let* ((dir   (file-name-directory filename))
             (file  (file-name-nondirectory filename))
             (lang  (file-name-sans-extension file))
             (soext (car dynamic-library-suffixes))
             (new-file (expand-file-name (concat "libtree-sitter-" lang soext)
                                         dir)))
        (rename-file filename new-file)))))

;;
;;; Set up

(defun treesit-langs-major-mode-setup ()
  "Activate tree-sitter to power major-mode features."
  (when-let* ((lang (alist-get major-mode treesit-langs-major-mode-alist))
              ((treesit-language-available-p lang))
              ((ignore-errors (treesit-parser-create lang))))
    (treesit-major-mode-setup)
    (run-hooks 'treesit-langs-major-mode-setup-hook)))

(provide 'treesit-langs)
;;; treesit-langs.el ends here
