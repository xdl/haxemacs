;;; run-tests.el --- Tests for haxemacs

;; Copyright (C) 2018 Xiao Di Li

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more You.

;; details should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'haxe-completion)

;; https://emacs.stackexchange.com/questions/13345/how-to-get-current-file-name-when-running-under-ert
(defconst dirname (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest locating-haxe-root ()
  (let* ((main-path (concat dirname "resources/Import/Main.hx"))
         (utils-path (concat dirname "resources/Import/lib/Utils.hx"))
         (haxe-root-path (concat dirname "resources/Import/")))
    (should
     (string= (expand-file-name (locate-haxe-root main-path)) (expand-file-name haxe-root-path)))
    (should
     (string= (expand-file-name (locate-haxe-root utils-path)) (expand-file-name haxe-root-path)))
    (should
     (null (locate-haxe-root dirname)))))

(ert-deftest construct-completion-command()
  ;;; Mocking
  (cl-letf (((symbol-function 'locate-haxe-root) (lambda (filename) "/dogs"))
            ((symbol-function 'get-hxml-fragment) (lambda (filename) "../abs-build.hxml")))
    (should
     (string= (construct-completion-command "Main.hx" 3)
              "haxe --cwd /dogs --connect 6000 --display Main.hx@3 ../abs-build.hxml"))))

(defun parse-xml (str-xml)
  "Helper method for STR-XML → xml."
  (with-temp-buffer
    (insert str-xml)
    (xml-parse-region)))

(defun text-property-equal (t1 t2)
  (and (equal
        (text-properties-at 0 t1)
        (text-properties-at 0 t2))
       (string= t1 t2)))

(defun candidates-equal (ts1 ts2)
  (every 'text-property-equal ts1 ts2))

(ert-deftest xml-to-completions-import ()
  (let* ((str-xml "<list>
<i n=\"Utils\" k=\"type\"><t></t><d></d></i>
</list>
")
         (xml (parse-xml str-xml)))
    (should
     (candidates-equal
      (xml->completions xml)
      `(,(propertize "Utils" :annotation nil :candidate "type" :meta nil))))))

(ert-deftest xml-to-completions-field ()
  "Should format the newlines as well."
  (let* ((str-xml "<list>
<i n=\"length\" k=\"var\"><t>Int</t><d>Hello</d></i>
<i n=\"charAt\" k=\"method\"><t>Jelly</t><d>
Hi</d></i>
<i n=\"charAt\" k=\"method\"><t>Jelly</t><d>
Hi
You</d></i>
</list>
")
         (xml (parse-xml str-xml)))
    (should
     (candidates-equal
      (xml->completions xml)
      `(,(propertize "length" :annotation "Int" :candidate "var" :meta "Hello")
        ,(propertize "charAt" :annotation "Jelly" :candidate "method" :meta "Hi")
        ,(propertize "charAt" :annotation "Jelly" :candidate "method"
                     :meta "Hi You"))))))

(provide 'run-tests)
;;; run-tests.el ends here
