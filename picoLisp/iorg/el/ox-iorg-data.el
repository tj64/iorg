;;; ox-iorg-data.el --- iOrg Data Back-End for Org Export Engine
;;;; License and Copyright

;; Copyright (C) 2013  Thorsten Jolitz

;; Author: Thorsten Jolitz <tjolitz@gmail.com>
;; Keywords: org, picolisp

;; This library is not (yet) part of Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary

;; This library implements a iOrg Data back-end for Org exporter.
;;
;; It introduces two interactive functions,
;; `org-iorg-data-export-as-iorg-data' and
;; `org-iorg-data-export-to-iorg-data', which export, respectively, to a
;; temporary buffer and to a file.
;;
;; A publishing function is also provided:
;; `org-iorg-data-publish-to-iorg-data'.

;;; Require
;; (eval-when-compile (require 'cl))
(require 'ox-org)
(require 'iorg)

;;; Define Back-End

(org-export-define-derived-backend 'iorg-data 'org
  :translate-alist ()
  ;; '((headline . org-iorg-data-headline)
  ;;   (item . org-iorg-data-item)
  ;;   (plain-list . org-iorg-data-plain-list)
  ;;   (table . org-iorg-data-table)
  ;;   (table-cell . org-iorg-data-table-cell)
  ;;   (table-row . org-iorg-data-table-row)
  ;;   (template . org-iorg-data-template))
  :export-block "IORG_DATA"
  :filters-alist '((:filter-parse-tree
		    . org-iorg-data-filter-parse-tree-function)
                   (:filter-section
		    . org-iorg-data-filter-section-function)
                   (:filter-final-output
		    . org-iorg-data-filter-final-output-function))
  :menu-entry
  '(?i "Export to iOrg"
       ((?I "As iOrg buffer" org-iorg-data-export-as-iorg)
	(?i "As iOrg file" org-iorg-data-export-to-iorg)
	(?o "As Org file and open"
	    (lambda (a s v b)
	      (if a (org-iorg-data-export-to-iorg t s v b)
		(org-open-file (org-iorg-data-export-to-iorg nil s v b)))))))
  :options-alist
  '((:iorg-export-p nil "iorg" t t)
    (:iorg-data "IORG_DATA" nil nil space)
    (:iorg-form "IORG_FORM" nil nil space)))


;;; Variables
;;;; Internal Variables
;;;; User Configurable Variables

(defgroup org-export-iorg-data nil
  "Options for exporting Org mode files to iOrg Database Objects."
  :tag "Org Export iOrg Data"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

;;; Functions
;;;; Internal Functions

;;;;; Tag Parse Tree

(defun org-iorg-data-add-ids (tree backend info)
  "Add ':elem-id' property to each element of parse TREE."
  (let ((counter 1)
        (structure 1))
    (org-element-map tree iorg-default-map-types
      (lambda (--elem)
          (org-element-put-property --elem :elem-id counter)
          (setq counter (1+ counter))
          (and (eq (org-element-type --elem) 'plain-list)
               (org-element-put-property --elem :structure-id structure)
               (setq structure (1+ structure))))))
  tree)

(defun org-iorg-data--collect-children (tree)
  "Return alist with '(elem-id . parent-id)' pairs.
The data is collected from parse TREE."
  (let (child-lst)
    (org-element-map tree 'headline
      (lambda (--headline)
        (push (cons (org-element-property :elem-id --headline)
                    (org-element-property :parent-id --headline))
              child-lst)))
    child-lst))

(defun org-iorg-data-add-children (tree backend info)
  "Add `:children' property to each headline in parse TREE.
Assumes that all headlines are tagged with an `:elem-id' property
and that the circular-list read-syntax of the `:parent' attribute
has been replaced with simple integer values (the :elem-id of the
elements parent)."
  (let ((pairs (org-iorg-data--collect-children tree)))
    (org-element-map tree 'headline
      (lambda (--elem)
        (org-element-put-property
         --elem :children
         (reverse
          (delq nil
                (mapcar
                 (lambda (--pair)
                   (and (eq (cdr --pair)
                            (org-element-property :elem-id --elem))
                        (car --pair)))
                 pairs)))))))
  tree)

(defun org-iorg-data-add-parent-ids (tree backend info)
  "Add `:parent-id' and `:parent-structure-id' to parse-tree TREE."
  (org-element-map tree iorg-all-map-types
    (lambda (--elem)
      (let ((par (org-element-property :parent --elem)))
        (and (eq (org-element-type --elem) 'item)
             (eq (org-element-type par) 'plain-list)
             (org-element-put-property
              --elem :parent-structure-id
              (org-element-property :structure-id par)))
        (org-element-put-property
         --elem :parent-id
         (if (eq (org-element-type par) 'org-data)
             0
           (org-element-property :elem-id par)))))
    nil nil nil 'WITH-AFFILIATED)
  tree)

(defun org-iorg-data-tag-org-data-element (tree backend info)
  "Add elem-id and other properties to `org-data' element of TREE.
Added Properties are taken from INFO, BACKEND is ignored. The modified
TREE is returned."
  (setcar (cdr tree)
          (list
           :parse-tree-id
           (make-temp-name
            (concat
             (file-name-nondirectory
              (file-name-sans-extension
               (or (plist-get info :input-file)
                   (plist-get info :input-buffer))))
              "_"))
           :input-file (plist-get info :input-file)
           :input-buffer (plist-get info :input-buffer)
           ;; :author (mapconcat 'identity (plist-get info :author) ", ")
           :author (car (plist-get info :author))
           :creator (plist-get info :creator)
           :email (plist-get info :email)
           :description (plist-get info :description)))
  (message "%S" tree)
  tree)

;; (defun org-iorg-data-tag-org-data-element (tree backend info)
;;   "Add elem-id and some properties to `org-data' element of TREE.
;; Added Properties are either related to parsed BUFFER or
;; environmental properties."
;;   (when (require 'ox nil 'NOERROR)
;;     (let* ((env-attr
;;             (with-current-buffer buffer
;;               (org-export-get-environment)))
;;            (buf-attr
;;             (with-current-buffer buffer
;;               (org-export--get-buffer-attributes)))
;;            (author (plist-get env-attr :author))
;;            (descr (plist-get env-attr :description))
;;            (infile-or-buf
;;             (or (plist-get buf-attr :input-file)
;;                 (and buffer
;;                      (with-current-buffer buffer
;;                        (buffer-name))))))
;;       (setcar (cdr tree)
;;               (list
;;                :parse-tree-id
;;                (and infile-or-buf
;;                     (make-temp-name
;;                      (concat
;;                       (file-name-nondirectory
;;                        (file-name-sans-extension
;;                         infile-or-buf)) "_")))
;;                ;; :elem-id 0
;;                :input-file infile-or-buf
;;                ;; :date (plist-get (cadar (plist-get env-attr :date))
;;                ;;              :raw-value)
;;                :author (when author
;;                          (substring-no-properties (car author)))
;;                ;; (substring-no-properties author))
;;                :creator (plist-get env-attr :creator)
;;                :email (plist-get env-attr :email)
;;                ;; :description descr
;;                :description (when descr
;;                               ;; (substring-no-properties (car descr))))
;;                               (substring-no-properties descr))))))
;;   tree)


;;;;; Deal with Property Lists



;; (defun iorg--fix-read-syntax-old (tree)
;;   "Returns parse TREE as string with read syntax fixed.

;; Fixed means, in this case, adjusted for the PicoLisp reader:

;;  - '#(' replaced by PicoLisp function `(hashtag '(....))'
;;  - '#1=(' replaced by PicoLisp function `(hashtag-equal '(....))'
;;  - '#1#' replaced by PicoLisp function `(enclosing-hashtags 1)'

;; These functions, when evaluated in PicoLisp, return a string with
;; the original Emacs Lisp read-syntax.

;; Furthermore, the leading ':' are removed from the keywords in the
;; parse-tree, and all keywords are converted to lowercase."
;;   (let ((txt-prop-regexp
;;          (concat
;;           ;; 1st
;;           "\\([[:space:]]\\|(\\)"
;;           ;; 2nd -> replace
;;           "\\(#(\\)"
;;           ;; 3rd
;;           "\\(.+?\n?.+?-id \\)"
;;           ;; 4th
;;           "\\([[:digit:]]+\\|nil\\)"
;;           ;; 5th
;;           "\\()+\\)"))
;;         ;; (concat
;;         ;;  ;; 1st
;;         ;;  "\\([[:space:]]\\|(\\)"
;;         ;;  ;; 2nd -> replace
;;         ;;  "\\(#(\\)"
;;         ;;  ;; 3rd
;;         ;;  "\\(.+?\n?.+?\\)"
;;         ;;  ;; 4th
;;         ;;  "\\() :parent-id[[:space:]]*\n?#?[[:digit:]]+\\)"
;;         ;;  ;; 5th
;;         ;;  "\\()+\\)"))
;;         (obj-ref-regexp
;;          (concat
;;           ;; 1st
;;           "\\(#\\)"
;;           ;; 2nd
;;           "\\([[:digit:]]+\\)"
;;           ;; 3rd
;;           "\\(=\\|#\\)"
;;           ;; 4th
;;           "\\(.*?\\n?.*?\\)"
;;           ;; 5th
;;           "\\(\s+:\\w+\\|)\s+\\|\s+(\\|\s+*$\\)"))
;;         (keyword-regexp
;;          (concat
;;           ;; 1st
;;           "\\([[:space:]]*\\)"
;;           ;; 2nd
;;           "\\(:\\)"
;;           ;; 3rd
;;           "\\([[:word:]-_]+\\)"
;;           ;; 4th
;;           "\\([[:space:]]+\\)")))
;;     ;; (with-temp-buffer
;;     (with-current-buffer "tmp<2>"
;;       (insert (prin1-to-string tree))
;;       (goto-char (point-min))
;;       (while (re-search-forward txt-prop-regexp nil 'NOERROR)
;;         (replace-match "\\1(hashtag '(\\3\\4\\5)"))
;;       (goto-char (point-min))
;;       (while (re-search-forward obj-ref-regexp nil 'NOERROR)
;;         (cond
;;          ((string-equal (match-string-no-properties 3) "=")
;;           (replace-match "(hashtag-equal '(\\2 \\4))\\5"))
;;          ((string-equal (match-string-no-properties 3) "#")
;;           (replace-match "(enclosing-hashtags \\2)\\4\\5"))
;;          (t (error "Matched subexpression not one of \"=\" or \"#\""))))
;;       (goto-char (point-min))
;;       (while (re-search-forward keyword-regexp nil 'NOERROR)
;;         ;; (let ((keyword (downcase (match-string-no-properties 3))))
;;         (replace-match
;;          ;; (concat "\\1" keyword "\\4"))))
;;          (concat "\\1\\3\\4")))
;;       (buffer-substring-no-properties (point-min) (point-max)))))


;; convenience function for testing - might be deleted
(defun org-iorg-data-tag-parse-tree (&optional buffer-or-file tree)
  "Return parse-tree TREE tagged with new properties.
These properties are `:elem-id', `:parent-id', `:structure-id',
`:parent-structure-id' and `:children'."
  (let ((ptree (cond
                (tree tree)
                ((and buffer-or-file (get-buffer buffer-or-file))
                 (with-current-buffer buffer-or-file
                   (org-element-parse-buffer)))
                (buffer-or-file
                 (with-current-buffer (find-file-existing buffer-or-file)
                   (org-element-parse-buffer)
                   (kill-buffer)))
                (t (with-current-buffer (current-buffer)
                     (org-element-parse-buffer))))))
    (org-iorg-data-add-children
     (org-iorg-data-add-parent-ids
      (org-iorg-data-add-ids ptree)))))

;; FIXME obsolete
(defun org-iorg-data-convert-plists-to-picolisp (tree)
  "Convert all elements plists in TREE to PicosLisp plists.

Assume TREE is a parse-tree produced by `org-element-parse-buffer' and turned
into a non-circular list by applying `iorg--tag-elems-with-id-attributes' and
`iorg--unwind-circular-list' on it. Return the parse-tree in PicoLisp
compliant form."
  (org-element-map
      tree
      iorg-default-map-types
      ;; iorg-default-map-types
    (lambda (--elem)
      ;; (let ((type (org-element-type elem)))
      ;;   ;; (and (not (eq type 'plain-text))
      ;;        ;; (not (eq type 'org-data))
      (let* ((plist (cadr --elem))
             (props (if (iorg--plist-p plist)
                        (org-iorg-data--collect-plist-keys plist)
                      (error "%s is not an Emacs Lisp plist"
                             plist))))
        ;; (message "%s"
        (cons (car --elem)
              (list
               (mapcar
                (lambda (--prop)
                  (cons (org-element-property --prop --elem) --prop))
                props)))))))
  ;; 'iorg--transform-elements-plist-to-picolisp-plist)
  ;; (message "%s" tree)

  ;; tree)

;; (defun iorg--transform-elements-plist-to-picolisp-plist (elem)
;;   "Convert elisp plist of ELEM into PicoLisp plist."
;;   (let ((type (org-element-type elem)))
;;     (and type
;;          (not (eq type 'plain-text))
;;          ;; (not (eq type 'org-data))
;;          (let* ((plist (cadr elem))
;;                 (props (if (iorg--plist-p plist)
;;                            (iorg--collect-plist-keys plist)
;;                          (error "%s is not an Emacs Lisp plist"
;;                                 plist))))
;;            (cons (car elem)
;;                  (mapcar
;;                   (lambda (prop)
;;                     (cons (org-element-property prop elem) prop))
;;                   props))))))


;;;; Template

(defun org-iorg-data-template (contents info)
  "Return complete document string after iOrg conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "(list '(org-data %s) %s)"
          (list
           'parse-tree-id
           (make-temp-name
            (concat
             (file-name-nondirectory
              (file-name-sans-extension
               (or (plist-get info :input-file)
                   (plist-get info :input-buffer))))
             "_"))
           'input-file (plist-get info :input-file)
           'input-buffer (plist-get info :input-buffer)
           'author (car (plist-get info :author))
           'creator (plist-get info :creator)
           'email (plist-get info :email)
           'description (plist-get info :description))
          contents))

;;;; Transcode Functions

;; (defun org-iorg-data-headline (headline contents info)
;;   "Transcode HEADLINE element back into Org syntax.
;; CONTENTS is its contents, as a string or nil.  INFO is ignored."
;; ;; (format "%s" contents))
;;   (unless (plist-get info :with-todo-keywords)
;;     (org-element-put-property headline :todo-keyword nil))
;;   (unless (plist-get info :with-tags)
;;     (org-element-put-property headline :tags nil))
;;   (unless (plist-get info :with-priority)
;;     (org-element-put-property headline :priority nil))
;;   (message "elem-id: %s\n parent-id: %s\n children: %s"
;;            (org-element-property :elem-id headline)
;;            (org-element-property :parent-id headline)
;;            (org-element-property :children headline))
;;   (org-element-headline-interpreter headline contents))

(defun org-iorg-data-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
;; (format "%s" contents))
  (unless (plist-get info :with-todo-keywords)
    (org-element-put-property headline :todo-keyword nil))
  (unless (plist-get info :with-tags)
    (org-element-put-property headline :tags nil))
  (unless (plist-get info :with-priority)
    (org-element-put-property headline :priority nil))
  (message "elem-id: %s\n parent-id: %s\n children: %s"
           (org-element-property :elem-id headline)
           (org-element-property :parent-id headline)
           (org-element-property :children headline))
  (org-element-headline-interpreter headline contents))

(defun org-iorg-data-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
(format "'(plain-list (%s))" contents))

(defun org-iorg-data-item (item contents info)
  "Transcode ITEM element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
(format "'(item (%s))" contents))

(defun org-iorg-data-table (table contents info)
  "Transcode TABLE element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
(format "%s" contents))

(defun org-iorg-data-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
(format "%s" contents))

(defun org-iorg-data-table-row (table-row contents info)
  "Transcode TABLE-ROW element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
(format "%s" contents))


;;;; Filter Functions

;; Hello,

;; Ezequiel Birman <stormwatch <at> espiga4.com.ar> writes:

;; > Is it possible to write something like this with the new exporter?
;; >
;; > #+OPTIONS: (if (and (boundp 'org-export-current-backend) (eq org-export-current-backend
;; 'e-beamer)) "H:1" "H:3")

;; There is no `org-export-current-backend' in the new exporter. Besides,
;; what you want is the default behaviour (see `org-e-beamer-frame-level'
;; variable).

;; > From what I read in org-export.el the backend is stored in a plist, not
;; > sure how to get it's value when exporting.
;; >
;; > Or, maybe I need to write a filter function to be run from
;; > org-export-before-process-hook?

;; Filters are different from hook. A function in a hook operates on an Org
;; buffer. A filter function operates either on a string in output syntax
;; or on the parse tree.

;; If, for some reason, you want to modify export options "on the fly", you
;; could create a filter function for parse tree, and modify options plist
;; from it:

;; #+begin_src emacs-lisp
;; (defun my-options-change-fun (tree backend info)
;;   (when (org-export-derived-backend-p backend 'e-beamer)
;;     (plist-put info :with-author nil))
;;   ;; Don't forget to return tree.
;;   tree)

;; (add-to-list 'org-export-filter-parse-tree-functions
;;              'my-options-change-fun)
;; #+end_src

;; Regards,

(defun org-iorg-data-filter-parse-tree-function (tree backend info)
  "Filter complete parsed TREE ignoring BACKEND and INFO."
  ;; (org-iorg-data-tag-org-data-element
   (org-iorg-data-add-children
    (org-iorg-data-add-parent-ids
     (org-iorg-data-add-ids tree backend info)
     backend info)
    backend info))
   ;; backend info))

(defun org-iorg-data-filter-section-function (section backend info)
  (format "(section %s)" section))

;; (defun org-iorg-data-filter-final-output-function (string backend info)
;;   "Prepare final output STRING ignoring BACKEND.
;; More specifically, tag `org-data' element with useful meta-data
;; extracted form INFO."
;;   (with-temp-buffer
;;     (insert string)

;;     (buffer-substring-no-properties (point-min) (point-max))))

;;;; End-user functions

;;;###autoload
(defun org-iorg-data-export-as-iorg-data (&optional async subtreep
visible-only ext-plist)
  "Export current buffer to an iOrg buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org IORG Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (if async
      (org-export-async-start
	  (lambda (output)
	    (with-current-buffer (get-buffer-create "*Org IORG Export*")
	      (erase-buffer)
	      (insert output)
	      (goto-char (point-min))
	      (org-mode)
	      (org-export-add-to-stack (current-buffer) 'iorg)))
	`(org-export-as 'iorg ,subtreep ,visible-only nil ',ext-plist))
    (let ((outbuf
	   (org-export-to-buffer
	    'iorg-data "*Org IORG_DATA Export*"
            subtreep visible-only nil ext-plist)))
      (with-current-buffer outbuf (org-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-iorg-data-export-to-iorg-data (&optional async subtreep visible-only ext-plist)
  "Export current buffer to an iorg file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".l" subtreep)))
    (if async
	(org-export-async-start
	    (lambda (f) (org-export-add-to-stack f 'iorg))
	  `(expand-file-name
	    (org-export-to-file
	     'iorg ,outfile ,subtreep ,visible-only nil ',ext-plist)))
      (org-export-to-file 'iorg outfile subtreep visible-only
      nil ext-plist))))

;;;###autoload
(defun org-iorg-data-publish-to-iorg-data (plist filename pub-dir)
  "Publish an org file to iorg.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'iorg filename ".l" plist pub-dir)
  (when (plist-get plist :htmlized-source)
    (require 'htmlize)
    (require 'ox-html)
    (let* ((org-inhibit-startup t)
	   (htmlize-output-type 'css)
	   (html-ext (concat "." (or (plist-get plist :html-extension)
				     org-html-extension "html")))
	   (visitingp (find-buffer-visiting filename))
	   (work-buffer (or visitingp (find-file filename)))
	   newbuf)
      (font-lock-fontify-buffer)
      (setq newbuf (htmlize-buffer))
      (with-current-buffer newbuf
	(when org-iorg-data-htmlized-css-url
	  (goto-char (point-min))
	  (and (re-search-forward
		"<style type=\"text/css\">[^\000]*?\n[ \t]*</style>.*" nil t)
	       (replace-match
		(format
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
		 org-iorg-data-htmlized-css-url) t t)))
	(write-file (concat pub-dir (file-name-nondirectory filename) html-ext)))
      (kill-buffer newbuf)
      (unless visitingp (kill-buffer work-buffer)))
    (set-buffer-modified-p nil)))

;;; Provide and Hooks

(provide 'ox-iorg-data)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;; ox-iorg-data.el ends here
