;;; haxe-mode.el --- a simple package                     -*- lexical-binding: t; -*-

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

;; Syntax for Haxe

;;; Code:

(setq haxe-namespace '("import" "package"))
(setq haxe-class-def '("class" "interface" "enum" "typedef"))
(setq haxe-keywords '("for" "if" "switch" "while" "try" "catch" "do" "else" "case" "default"))
(setq haxe-scope-modifiers '("static" "public" "private" "override" "get" "set" "inline" "extern"))
(setq haxe-sub-keywords '("break" "continue" "return" "new" "in" "extends" "implements" "var" "function"))
(setq haxe-constant-expressions '("false" "true" "null"))

(setq regex-haxe-namespace (regexp-opt haxe-namespace 'words))
(setq regex-haxe-class-def (regexp-opt haxe-class-def 'words))
(setq regex-haxe-keywords (regexp-opt haxe-keywords 'words))
(setq regex-haxe-sub-keywords (regexp-opt haxe-sub-keywords 'words))
(setq regex-haxe-scope-modifiers (regexp-opt haxe-scope-modifiers 'words))
(setq regex-haxe-constant-expressions (regexp-opt haxe-constant-expressions 'words))

(setq haxe-mode-font-lock-keywords
      `((,regex-haxe-keywords . font-lock-keyword-face)
        (,regex-haxe-sub-keywords . font-lock-keyword-face)
        (,regex-haxe-class-def . font-lock-keyword-face)
        (,regex-haxe-namespace . font-lock-keyword-face)
        (,regex-haxe-scope-modifiers . font-lock-keyword-face)
        (,regex-haxe-constant-expressions . font-lock-constant-face)))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
;; https://www.emacswiki.org/emacs/ModeTutorial
(defvar haxe-mode-syntax-table
  (let ((haxe-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" haxe-mode-syntax-table)
    (modify-syntax-entry ?* ". 23b" haxe-mode-syntax-table)
    (modify-syntax-entry ?\n ">" haxe-mode-syntax-table)
    haxe-mode-syntax-table))

(defvar haxe-mode-map
  (let ((map (make-keymap)))
    map))

(defvar haxe-mode-hook nil)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defvar haxe-indent-offset 2)
(defun haxe-indent-line()
  "Indentation for Haxe."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      ;; https://stackoverflow.com/questions/4158216/emacs-custom-indentation
      (save-excursion
        (beginning-of-line)
        (condition-case nil
            (while t
              (backward-up-list 1)
              (when (looking-at "[[{(]")
                (setq indent-col (+ indent-col haxe-indent-offset))))
          (error nil)))
      ;; case where the line starts with }])
      (save-excursion
        (back-to-indentation)
        (when (and (looking-at "[])}]") (>= indent-col haxe-indent-offset))
          (setq indent-col (- indent-col haxe-indent-offset))))
      (indent-line-to indent-col))
    ;; Electric return is actually working; cursor just needs to move to the end
    (if (current-line-empty-p)
        (end-of-line))
    ))


(define-derived-mode haxe-mode prog-mode
  :syntax-table haxe-mode-syntax-table
  (setq-local font-lock-defaults '(haxe-mode-font-lock-keywords))
  (setq-local indent-line-function 'haxe-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (setq major-mode 'haxe-mode
        mode-name "Haxe"))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(provide 'haxe-mode)
;;; haxe-mode.el ends here
