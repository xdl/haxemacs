;;; haxe-completion.el --- a simple package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Xiao Di Li

;; Author: Xiao Di Li <eddie@xiaodi.li>
;; Keywords: lisp haxe
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Completions for Haxe

;;; Code:

(require 'xml)
(defconst haxe-root-marker ".haxe-root")
(defconst haxe-port 6000)

(defvar haxe-root->hxml-cache (make-hash-table))

(defun locate-haxe-root (from-buffer-path)
  "Find .haxe-root path upwards from FROM-BUFFER-PATH.  nil if nothing found."
  (locate-dominating-file from-buffer-path haxe-root-marker))

(defun get-hxml-fragment (haxe-root)
  "Get HAXE-ROOT from cache if there; otherwise get and put it there."
  (if (gethash haxe-root haxe-root->hxml-cache)
      (gethash haxe-root haxe-root->hxml-cache)
    (let ((hxml-path (with-temp-buffer
             (insert-file-contents (concat haxe-root haxe-root-marker))
             (buffer-string))))
      (puthash haxe-root hxml-path haxe-root->hxml-cache)
      (gethash haxe-root haxe-root->hxml-cache))))

(defun construct-completion-command (filename byte-position)
  "Construct Haxe command from FILENAME and BYTE-POSITION for completion server."
  (let* ((haxe-root (locate-haxe-root filename))
         (hxml-fragment (get-hxml-fragment haxe-root)))
    (format "haxe --cwd %s --connect %d --display %s@%d %s"
            haxe-root
            haxe-port
            filename
            byte-position
            hxml-fragment)))

(defun process-doc-text (doc-text)
  (let* ((doc-split (mapcar 'string-trim (split-string doc-text "\n"))))
    (string-trim (string-join doc-split " "))))

(defun candidate->text-annotation (candidate)
  (let* ((attributes (xml-node-attributes candidate))
         (name (cdr (assq 'n attributes)))
         (k (cdr (assq 'k attributes)))
         (type (car (xml-get-children candidate 't)))
         (type-text (car (xml-node-children type)))
         (doc (car (xml-get-children candidate 'd)))
         (maybe-doc-text (car (xml-node-children doc)))
         (doc-text (if maybe-doc-text
                       (process-doc-text maybe-doc-text)
                     nil)))
    (propertize name :annotation type-text :candidate k :meta doc-text)))

(defun parse-completion-list (list)
  "Parse <i> nodes of LIST <list>."
  (let ((candidates (xml-get-children list 'i)))
    (mapcar 'candidate->text-annotation candidates)))

(defun xml->completions (xml)
  (let* ((root-node (car xml))
         (root-node-name (xml-node-name root-node)))
    (cond ((string= root-node-name "list") (parse-completion-list root-node))
          (t '(#("company: Error parsing xml from Haxe completion server."))))))

(provide 'haxe-completion)
;;; haxe-completion.el ends here
