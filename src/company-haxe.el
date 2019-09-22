;;; company-haxe.el --- a simple package                     -*- lexical-binding: t; -*-

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

;; Company Integration for Haxe

;;; Code:
(require 'cl-lib)
(require 'company)
(require 'haxe-completion)

(defun check-should-save (prefix)
  "Probably should save everytime , but I think we can just do it whenever PREFIX is empty."
  (when (and (= (length prefix) 0)
             (buffer-modified-p))
    (basic-save-buffer)))

(defun filter-candidates (candidates prefix)
  "Just checking if PREFIX is in CANDIDATES."
  (remove-if-not
   (lambda (candidate)
     (string-prefix-p prefix candidate))
   candidates))

;; https://github.com/deepakg/emacs/blob/master/perlysense/async-shell-command-to-string.el
(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background, calling CALLBACK with result."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun output->candidates (str-xml)
  "Formats output of shell STR-XML into xml."
  (xml->completions (with-temp-buffer
                        (insert str-xml)
                        (xml-parse-region))))

(defun company-haxe-candidates (callback prefix)
  "Get the completion candidates (async CALLBACK)."
  (check-should-save prefix)
  (let* ((byte-position (- (position-bytes (point)) 1 (length prefix)))
         (filename (buffer-file-name))
         (haxe-command (construct-completion-command filename byte-position))
         (on-shell-return
          (lambda (str-xml)
            (funcall callback (filter-candidates (output->candidates str-xml) prefix)))))
    (async-shell-command-to-string haxe-command on-shell-return)))

(defun company-haxe-annotation (candidate)
  (get-text-property 0 :annotation candidate))

(defun company-haxe-meta (candidate)
  (get-text-property 0 :meta candidate))

;; This needs to account for:
;; .|
;; (|
;; asd|
;; import x.|
;; import x.x.|
;; using x.|
(defun haxe-completion-prefix ()
  "Trigger on either . or 3 chars."
  (or
   (company-grab-symbol-cons "\\.")
   (company-grab-symbol)))

(defun company-haxe-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-haxe-backend))
    (prefix (and (eq major-mode 'haxe-mode)
                 (haxe-completion-prefix)))
    (candidates (cons :async
                      (lambda (callback)
                        (company-haxe-candidates callback arg))))
    (annotation (company-haxe-annotation arg))
    (meta (company-haxe-meta arg))
    (no-cache 't)))

(add-to-list 'company-backends 'company-haxe-backend)

(provide 'company-haxe)
