;;; haxe-eldoc.el --- Summary
;;; Commentary:
;;; Code:
(require 'eldoc)
(require 'haxe-completion)

(defun haxe-eldoc-check-should-save ()
  "Save the file if need be."
  (when (buffer-modified-p)
    (basic-save-buffer)))

(defun haxe-eldoc-parse-arguments-string (arguments-node)
  "Return asd from <list>asd</list>."
  (let* ((arg-string (car (xml-node-children arguments-node)))
         (arg-string-trimmed (string-trim arg-string)))
    arg-string-trimmed))

(defun haxe-eldoc-process-payload (payload)
  "Take PAYLOAD from Haxe completion server and parse out the completions."
  (let* ((parsed-xml (with-temp-buffer
                       (insert payload)
                       (xml-parse-region)))
         (root-node (car parsed-xml))
         (root-node-name (xml-node-name root-node)))
    (if (string= root-node-name "type")
        (haxe-eldoc-parse-arguments-string root-node)
      "eldoc: Error passing xml from Haxe completion server.")))

(defun haxe-eldoc-proceed-to-completions ()
  "Save the buffer if necessary, then fetch function parameters."
  (haxe-eldoc-check-should-save)
  (let* ((byte-position (- (position-bytes (point)) 1))
         (filename (buffer-file-name))
         (completion-command (construct-completion-command filename byte-position))
         (payload (shell-command-to-string completion-command))
         (processed-payload (haxe-eldoc-process-payload payload)))
    processed-payload))

(defun haxe-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (let ((haxe-eldoc-in-method (haxe-eldoc-method-call-p)))
    (if haxe-eldoc-in-method
        (haxe-eldoc-proceed-to-completions)
      nil)))

;; This doesn't differentiate between specific pairs (e.g. parens, curlys, squares); using the Tide method instead.
(defun haxe-eldoc-point-in-parens ()
  "Return whether point is inside parens."
  (let ((sppss (syntax-ppss)))
    (and (> (nth 0 sppss) 0) ; in matching parens
         (not (nth 4 sppss)) ; not in comment
         (not (nth 3 sppss))))) ; not in string

;; From https://github.com/ananthakumaran/tide/blob/9556b440c84b3ae05c6aab768c7d83cdc74e5493/tide.el#L964
(defun haxe-eldoc-method-call-p ()
  (or (looking-at "[(,]") (and (not (looking-at "\\sw")) (looking-back "[(,]\n?\\s-*"))))

(defun trigger-haxe-eldoc ()
  "Manually trigger eldoc for now."
  (interactive)
  (message (haxe-eldoc-function)))


(defun setup-haxe-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function)
       'haxe-eldoc-function))

(add-hook 'haxe-mode-hook 'setup-haxe-eldoc)

(provide 'haxe-eldoc)
;;; haxe-eldoc.el ends here
