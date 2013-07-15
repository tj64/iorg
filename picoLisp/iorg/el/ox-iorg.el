;;; ox-iorg.el --- iOrg Back-End for Org Export Engine
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

;; This library implements a iOrg back-end for Org exporter.
;;
;; It introduces two interactive functions, `org-iorg-export-as-org'
;; and `org-iorg-export-to-org', which export, respectively, to
;; a temporary buffer and to a file.
;;
;; A publishing function is also provided: `org-iorg-publish-to-org'.

;;; Require
(require 'ox)
(declare-function htmlize-buffer "htmlize" (&optional buffer))

;;; Define Back-End

(org-export-define-backend 'iorg
  '((babel-call . identity)
    (bold . identity)
    (center-block . identity)
    (clock . identity)
    (code . identity)
    (comment . (lambda (&rest args) ""))
    (comment-block . (lambda (&rest args) ""))
    (diary-sexp . identity)
    (drawer . identity)
    (dynamic-block . identity)
    (entity . identity)
    (example-block . identity)
    (fixed-width . identity)
    (footnote-definition . identity)
    (footnote-reference . identity)
    (headline . org-iorg-headline)
    (horizontal-rule . identity)
    (inline-babel-call . identity)
    (inline-src-block . identity)
    (inlinetask . identity)
    (italic . identity)
    (item . identity)
    (keyword . identity)
    (latex-environment . identity)
    (latex-fragment . identity)
    (line-break . identity)
    (link . identity)
    (node-property . identity)
    (paragraph . org-iorg-paragraph)
    (plain-list . org-iorg-plain-list)
    (plain-text . org-iorg-plain-text)
    (planning . identity)
    (property-drawer . identity)
    (quote-block . identity)
    (quote-section . identity)
    (radio-target . org-iorg-radio-target)
    (section . org-iorg-section)
    (special-block . identity)
    (src-block . identity)
    (statistics-cookie . identity)
    (strike-through . identity)
    (subscript . identity)
    (superscript . identity)
    (table . org-iorg-table)
    (table-cell . org-iorg-table-cell)
    (table-row . org-iorg-table-row)
    (target . identity)
    (template . org-iorg-template)
    (timestamp . org-iorg-timestamp)
    (underline . identity)
    (verbatim . identity)
    (verse-block . identity))
  :export-block "IORG"
  :filters-alist '(
                   ;; convert 
                   (:filter-parse-tree
		    . (ox-iorg--translate-stuff))
                   ;; change #( read syntax
                   (:filter-plain-text
		    . (ox-iorg--translate-stuff))
                   ;; add info and elem-id to org-data
                   (:filter-org-data
		    . (ox-iorg--translate-stuff))
                   ;; nil and t to uppercase
                   (:filter-final-output
		    . (ox-iorg--translate-stuff)))
  :menu-entry
  '(?O "Export to Org"
       ((?O "As Org buffer" org-iorg-export-as-iorg)
	(?o "As Org file" org-iorg-export-to-iorg)
	(?v "As Org file and open"
	    (lambda (a s v b)
	      (if a (org-iorg-export-to-iorg t s v b)
		(org-open-file (org-iorg-export-to-iorg nil s v b))))))))


;;; Variables
;;;; Internal Variables
;;;; User Configurable Variables

(defgroup org-export-iorg nil
  "Options for exporting Org mode files to iOrg."
  :tag "Org Export iOrg"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(define-obsolete-variable-alias
  'org-export-htmlized-org-css-url 'org-iorg-htmlized-css-url "24.4")
(defcustom org-iorg-htmlized-css-url nil
  "URL pointing to the CSS defining colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer,
htmlize will create the CSS to define the font colors.  However,
this does not work when converting in batch mode, and it also can
look bad if different people with different fontification setup
work on the same website.  When this variable is non-nil,
creating an htmlized version of an Org buffer using
`org-iorg-export-as-org' will include a link to this URL if the
setting of `org-html-htmlize-output-type' is 'css."
  :group 'org-export-iorg
  :type '(choice
	  (const :tag "Don't include external stylesheet link" nil)
	  (string :tag "URL or local href")))

;;; Functions
;;;; Internal Functions
;;;; Template

(defun org-iorg-template (contents info)
  "Return complete document string after iOrg conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.")

;;;; Transcode Functions

;; (defun org-org-identity (blob contents info)
;;   "Transcode BLOB element or object back into Org syntax.
;; CONTENTS is its contents, as a string or nil.  INFO is ignored."
;;   (org-export-expand blob contents t))

(defun org-iorg-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (unless (plist-get info :with-todo-keywords)
    (org-element-put-property headline :todo-keyword nil))
  (unless (plist-get info :with-tags)
    (org-element-put-property headline :tags nil))
  (unless (plist-get info :with-priority)
    (org-element-put-property headline :priority nil))
  (org-element-headline-interpreter headline contents))


(defun org-iorg-section (headline contents info)
  "Transcode SECTION element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-paragraph (headline contents info)
  "Transcode PARAGRAPH element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-plain-list (headline contents info)
  "Transcode PLAIN-LIST element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-radio-target (headline contents info)
  "Transcode RADIO-TARGET element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-table (headline contents info)
  "Transcode TABLE element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-table-cell (headline contents info)
  "Transcode TABLE-CELL element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-table-row (headline contents info)
  "Transcode TABLE-ROW element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-timestamp (headline contents info)
  "Transcode TIMESTAMP element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

(defun org-iorg-plain-text (headline contents info)
  "Transcode PLAIN-TEXT element into iOrg syntax.
CONTENTS is its contents, as a string or nil.  INFO is ignored.")

;;;; Filter Functions

(defun org-iorg-final-function (contents backend info)
  "Filter to indent the HTML and convert HTML entities."
  (with-temp-buffer
    (insert contents)
    (set-auto-mode t)
    (if org-html-indent
	(indent-region (point-min) (point-max)))
    (when org-html-use-unicode-chars
      (require 'mm-url)
      (mm-url-decode-entities))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; End-user functions

;;;###autoload
(defun org-iorg-export-as-iorg (&optional async subtreep visible-only ext-plist)
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
	    'iorg "*Org IORG Export*"
            subtreep visible-only nil ext-plist)))
      (with-current-buffer outbuf (iorg-mode))
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window outbuf)))))

;;;###autoload
(defun org-iorg-export-to-iorg (&optional async subtreep visible-only ext-plist)
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
(defun org-iorg-publish-to-iorg (plist filename pub-dir)
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
	(when org-iorg-htmlized-css-url
	  (goto-char (point-min))
	  (and (re-search-forward
		"<style type=\"text/css\">[^\000]*?\n[ \t]*</style>.*" nil t)
	       (replace-match
		(format
		 "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
		 org-iorg-htmlized-css-url) t t)))
	(write-file (concat pub-dir (file-name-nondirectory filename) html-ext)))
      (kill-buffer newbuf)
      (unless visitingp (kill-buffer work-buffer)))
    (set-buffer-modified-p nil)))

;;; Provide and Hooks

(provide 'ox-iorg)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;; ox-iorg.el ends here
