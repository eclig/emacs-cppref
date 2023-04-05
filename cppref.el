;;; cppref.el --- A Simple cppreference.com Browser

;; Copyright (C) 2009 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi, <kentarok@gmail.com>
;; Keywords: C++

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * Description

;; This library provides a simple interface to cppreference.com's offline
;; archives.

;; * Usage

;; Add the lines below into your .emacs:
;;
;;   (require 'cppref)
;;
;; File viewing is handled by `browse-url'.  You can customize the browser used
;; by setting the variable `browse-url-browser-function' appropriately.
;;
;; You also need a copy of cppreference.com's offline archives (the "HTML book").
;; It can be  downloaded from https://en.cppreference.com/w/Cppreference:Archives.
;; Unpack the archive in a directory of your choice.  The default location for
;; the cppreference.com archive is
;;
;;   /usr/share/doc/cppreference
;;
;; This can be customized using the variable `cppref-doc-dir'.
;;
;; Then run `cppref' command and type something like "vector::begin", or
;; "io::fopen".

;;; Acknowledgments:

;; cppref.el is the Emacs version of Kazuho Oku's cppref command
;; http://search.cpan.org/dist/cppref/

;;; Code:

(require 'thingatpt)
(require 'cl-lib)

(defvar cppref-doc-dir "/usr/share/doc/cppreference"
  "*Location of the cppreference.com offline archive.
The archive can be downloaded from https://en.cppreference.com/w/Cppreference:Archives.")

(defun cppref--read-keyword ()
  (let* ((sap (symbol-at-point))
         (default (and sap (symbol-name sap))))
    (read-string (format-prompt "Keyword" default) nil nil default)))

(defun cppref--remove-dir-prefix (dir fn)
  (substring fn (length (file-name-as-directory dir)) nil))

(defun cppref--select-match (prompt matches)
  (let ((items (mapcar (lambda (fn) (cppref--remove-dir-prefix cppref-doc-dir fn)) matches)))
    (file-name-concat cppref-doc-dir (completing-read prompt items nil t))))

(defun cppref (keyword)
  "Search cppreference.com archive for KEYWORD."
  (interactive (list (cppref--read-keyword)))
  (cppref-check-doc-dir)
  (let ((matches (cppref-find-reference cppref-doc-dir keyword)))
    (cond
     ((null matches)
      (error "Nothing found for \"%s\"" keyword))
     ((length= matches 1)
      (cppref-visit-reference (car matches)))
     (t
      (let ((reference (cppref--select-match (format "cppref matches for \"%s\": " keyword) matches)))
        (cppref-visit-reference reference))))))

(defun cppref-check-doc-dir ()
  (unless (and cppref-doc-dir (file-directory-p cppref-doc-dir))
    (error "Directory `%s' does not exist" cppref-doc-dir)))

(defun cppref-find-reference (dir keyword)
  ;; Handle qualified names like "vector::begin" by first searching for "begin"
  ;; and later refining the match.
  (let* ((components (split-string keyword "::"))
         (base (car (last components)))
         (qualifiers (nbutlast components))
         (regexp (if qualifiers
                     (rx-to-string `(seq string-start ,base (* anychar) ".html" string-end))
                   (rx-to-string `(seq ,base (* anychar) ".html" string-end))))
         (matches (directory-files-recursively dir regexp)))
    ;; If the symbol name is qualified we try to return a match as exact as
    ;; possible.  Otherwise return all matches found.  For example, we return a
    ;; single match for "vector::swap" but in case of "swap" (unqualified) also
    ;; return matches for "swap2".
    (if qualifiers
        (let* ((refined
                (cl-remove-if-not
                 (lambda (fn)
                   (string-match-p (rx-to-string `(seq "/" ,(replace-regexp-in-string "::" "/" keyword))) fn))
                 matches))
               (exact
                (cl-remove-if-not
                 (lambda (fn)
                   (string-match-p (rx-to-string `(seq "/" ,(replace-regexp-in-string "::" "/" keyword) ".html" string-end)) fn))
                 refined)))
          (if (length> exact 0)
              exact
            refined))
      matches)))

(defun cppref-visit-reference (reference)
  (browse-url reference))

(provide 'cppref)
;;; cppref.el ends here
