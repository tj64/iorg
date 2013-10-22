;; * iorg.el --- interactive Org-mode
;; ** MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten_Jolitz
;;   :copyright-since: 2013
;;   :version:  0.9
;;   :licence:  GPL3+
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :git-repo: https://github.com/tj64/iorg
;;   :git-clone: git@github.com:tj64/iorg.git
;;   :authors: Thorsten_Jolitz
;;   :contact: <tjolitz@gmail.com>
;;   :inspiration:  org-mode picolisp
;;   :keywords: emacs org-mode dhtml interactive_web_applications
;;   :END:

;; ** Commentary
;; *** About iOrg
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog

;; * Requires

;; ;; get it from ELPA
;; ;; or from the [[https://github.com/nicferrier/emacs-kv][github-repo]]
;; (require 'kv)
;; (require 'paredit)
;; (require 'inferior-picolisp nil 'NOERROR)
;; (require 'outorg nil 'NOERROR)
(require 'ox)

(eval-when-compile (require 'cl))

;; * Mode and Exporter definitions

;; ** Mode definitions

;; *** iOrg Minor Mode

;; FIXME  \\[iorg-link] iorg-link ??
(define-minor-mode iorg-minor-mode
   "Minor mode for Org-mode buffers generated by outorg.
There is a mode hook, and a few commands:
\\[iorg-insert-internal-link] iorg-insert-internal-link
\\[iorg-minor-mode-map] iorg-minor-mode-map"
  :lighter " iOrg"
  :keymap '(("\C-c\C-x=" . iorg-insert-internal-link)))

(add-hook 'org-mode-hook 'iorg-minor-mode)

;; * Variables
;; ** Consts
;; ** Vars

;; (defvar iorg-circ-obj-label-regexp
;;   "\\(#\\)\\([[:digit:]]+\\)\\(#\\|=\\)"
;;   "Regexp that matches the label of an object or the
;;   reference to such a labeled in a circular list.")

(defvar iorg-default-map-types
  (append org-element-all-elements org-element-all-objects)
  "Default types to be selected by `org-element-map'.")

;; ;; FIXME all types covered?
;; (defvar iorg-all-types-no-text
;;   (append '(org-data) org-element-all-elements
;;   org-element-all-objects)
;;   "Types to be selected by `org-element-map'.")

;; FIXME all types covered?
(defvar iorg-default-map-types-plus-text
  (append org-element-all-elements org-element-all-objects
  '(plain-text))
  "Types to be selected by `org-element-map'.")

(defvar iorg-all-map-types
  (append org-element-all-elements org-element-all-objects
  '(plain-text) '(structure))
  "Types to be selected by `org-element-map'.")

;; (defvar iorg-element-shared-properties '(begin end post-blank parent
;; contents-begin contents-end post-affiliated)
;;   "Properties shared by all Org elements (with the leading colon
;;   stripped).")

;; (defvar iorg-element-properties-alist
;;   '((center-block . '(hiddenp))
;;     (drawer . '(drawer-name))
;;     (dynamic-block . '(block-name arguments))
;;     (footnote-definition . '(label))
;;     (headline . '(raw-value title alt-title pre-blank level
;;   priority tags todo-keyword todo-type scheduled deadline closed
;;   quotedp archivedp commentedp footnote-section-p))
;;     (inlinetask . '(title hiddenp  
;;     (bullet checkbox counter tag
;;   structure type info status value time )
;;   "Alist of Org element types and their possible keyword
;;   properties found in an Org-mode parse-tree (with the leading
;;   colon stripped).")

(defvar iorg-default-host-path "http://localhost:5001"
  "Default path (protocol, host, port) for iOrg server.")

;; ** Hooks
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions
;; *** Helper Functions

(defsubst iorg--elisp-plist-p (lst)
  "Return non-nil if LST is a list and its car a keyword."
  (and (listp lst) (keywordp (car lst))))

;; courtesy of Pascal Bourguignon
;; FIXME: recursion too deep -> enter debugger
(defun iorg--sexp-remove-string-properties (sexp)
   (cond
      ((stringp sexp) (substring-no-properties sexp))
      ((atom sexp) sexp)
      (t (cons (iorg--sexp-remove-string-properties (car sexp))
               (iorg--sexp-remove-string-properties (cdr sexp))))))

;; application example:
;; (prin1-to-string
;;   (iorg--sexp-remove-string-properties
;;     '(:category "tmp2" :title (#("C2 " 0 3 (:parent nil))))))
;; --> "(:category \"tmp2\" :title (\"C2 \"))"


(defun iorg--tag-org-data-element (tree buffer)
  "Add elem-id and some properties to `org-data' element of TREE.
Added Properties are either related to parsed BUFFER or
environmental properties."
  (when (require 'ox nil 'NOERROR)
    (let* ((env-attr
            (with-current-buffer buffer
              (org-export-get-environment)))
           (buf-attr
            (with-current-buffer buffer
              (org-export--get-buffer-attributes)))
           (author (plist-get env-attr :author))
           (descr (plist-get env-attr :description))
           (infile-or-buf
            (or (plist-get buf-attr :input-file)
                (with-current-buffer buffer
                  (buffer-name)))))
      (setcar (cdr tree)
              (list
               :parse-tree-id (make-temp-name
                               (concat
                                (file-name-nondirectory
                                 (file-name-sans-extension
                                  infile-or-buf)) "_"))
               ;; :elem-id 0
               :input-file infile-or-buf
               ;; :date (plist-get (cadar (plist-get env-attr :date))
               ;;              :raw-value)
               :author (when author
                         (substring-no-properties (car author)))
               ;; (substring-no-properties author))
               :creator (plist-get env-attr :creator)
               :email (plist-get env-attr :email)
               ;; :description descr
               :description (when descr
                              ;; (substring-no-properties (car descr))))
                              (substring-no-properties descr))))))
  tree)


(defun iorg--nil-and-t-to-uppercase (tree-as-string)
  "Takes a parse TREE-AS-STRING and upcases nil and t."
  (and (stringp tree-as-string)
        (replace-regexp-in-string
         "\\(\\_<t\\_>\\|(t)\\|\\_<nil\\_>\\|(nil)\\)"
        'iorg--rep-function-for-nil-and-t
        tree-as-string)))

(defun iorg--rep-function-for-nil-and-t (match)
  "Helper function for converting Elisp 'nil' an 't' to PicoLisp syntax.
MATCH is the match-string to be converted, with 'nil' becoming
'NIL' and 't' becoming 'T'."
  (cond
   ((string= match "t")
    (format "%s" "T"))
   ((string= match "nil")
    (format "%s" "NIL"))
   ((string= match "(t)")
    (format "%s" "(T)"))
   ((string= match "(nil)")
    (format "%s" "(NIL)"))))

;; ;; done before sending from PicoLisp
;; (defun iorg--NIL-and-T-to-lowercase (tree-as-string)
;;   "Downcase NIL and T in TREE-AS-STRING converted from PicoLisp."
;;   (and (stringp tree-as-string)
;;         (replace-regexp-in-string
;;          "\\(\\_<T\\_>\\|(T)\\|\\_<NIL\\_>\\|(NIL)\\)"
;;         'iorg--rep-function-for-NIL-and-T
;;         tree-as-string)))

;; (defun iorg--rep-function-for-NIL-and-T (match)
;;   "Helper function for converting PicoLisp 'NIL' an 'T' to Elisp syntax.
;; MATCH is the match-string to be converted, with 'NIL' becoming
;; 'nil' and 'T' becoming 't'."
;;   (cond
;;    ((string= match "T")
;;     (format "%s" "t"))
;;    ((string= match "NIL")
;;     (format "%s" "nil"))
;;    ((string= match "(T)")
;;     (format "%s" "(t)"))
;;    ((string= match "(NIL)")
;;     (format "%s" "(nil)"))))

(defun iorg--fix-read-syntax (tree)
  "Returns parse TREE as string with read syntax fixed.
Fixed means, in this case, adjusted for the PicoLisp reader:
backquote leading '#' characters. And, although not strictly
necessary, remove the leading ':' of keywords in the parse-tree
while on it."
  (let ((hash-regexp "#"))
    (with-temp-buffer
    ;; (with-current-buffer "tmp<2>"
      (insert (prin1-to-string tree))
      (goto-char (point-min))
      (while (re-search-forward hash-regexp nil 'NOERROR)
        (replace-match "\\\\\\&"))
      (buffer-substring-no-properties (point-min) (point-max)))))

;; TODO: check if necessary - maybe elisp reader does all this as-is?
(defun iorg--unfix-read-syntax (tree-as-string)
  "Returns parse TREE-AS-STRING with read syntax unfixed.
Unfixed means, in this case, adjusted from PicoLisp syntax for
the Emacs Lisp reader: remove backslashes and trailing blanks
from all '\#' labels."
  (message "entering iorg--unfix-read-syntax ...")
  (let ((leading-hashtag-regexp
         (concat
          ;; 1st
          "\\(\\\\\\)"
          ;; 2nd
          "\\(#\\)"
          ;; 3rd
          "\\( \\)"
          ;; 4th
          "\\((\\)")))
    (with-temp-buffer
      ;; (with-current-buffer "tmp<2>"
      (insert tree-as-string)
      (goto-char (point-min))
      (while (re-search-forward leading-hashtag-regexp nil 'NOERROR)
        (replace-match "\\2\\4"))
      (buffer-substring-no-properties (point-min) (point-max)))))


;; *** Core Functions

;; **** Normalize Parse-Tree

 ;; preserve-nil-and-t-p)
(defun iorg-convert-parse-tree (&optional data buffer-or-file)
  "Converts an org-element parse-tree to PicoLisp syntax.

Optional argument DATA should be part of or an entire parse-tree
as returned by `org-element-parse-buffer', optional argument
BUFFER-OR-FILE is either the name of an existing Org-mode buffer
or the name of an Org-mode file."
  ;; If optional argument PRESERVE-NIL-AND-T-P is non-nil, nil and t
  ;; are not converted to uppercase forms NIL and T."
  ;; get data and arguments
  (let* ((buf (or (and buffer-or-file
                       (or (get-buffer buffer-or-file)
                           (if (and
                                (file-exists-p buffer-or-file)
                                (string=
                                 (file-name-extension buffer-or-file) "org"))
                               (find-file-noselect buffer-or-file)
                             (error "File %s is not a valid Org file"
                                    buffer-or-file))))
                  (current-buffer)))
         (print-circle t)
         (print-escape-newlines t)
         (dat (or data
                  (with-current-buffer buf
                    (org-element-parse-buffer 'object)))))
    ;; (converted-parse-tree-as-string
    ;;  (iorg--fix-read-syntax
    (iorg--tag-org-data-element dat buf)))


    ;; ;; upcase nil and t?
    ;; (if preserve-nil-and-t-p
    ;;     converted-parse-tree-as-string
    ;;   (iorg--nil-and-t-to-uppercase converted-parse-tree-as-string))))

(defun iorg-wrap-parse-tree (tree-as-string)
  "Wrap TREE-AS-STRING in PicoLisp function `processParseTree'."
  (concat "(processParseTree " tree-as-string " )"))

;; (defun iorg-reconvert-parse-tree (tree-as-string)
;;   "Reconvert a parse TREE-AS-STRING stored in a PicoLisp database.
;; Convert PicoLisp compliant syntax back to Emacs Lisp syntax."
;;   (read-from-string
;;    (iorg--unfix-read-syntax tree-as-string)))

(defun iorg-interpret-data (tree-as-string)
  "Return the textual interpretation of TREE-AS-STRING.
Call `org-element-interpret-data' to do the real work."
  (message "entering iorg-interpret-data ...")
  (org-element-interpret-data 
   (car
    (read-from-string
     (iorg--unfix-read-syntax tree-as-string)))))

;; **** Query Database

(defun iorg-retrieve-url(path &optional LISP-P hostpath &rest args)
  "Generic function for calling the iOrg server from Emacs.

The PicoLisp application server uses a slightly specialized syntax when
communicating URLs to and from a client. The PATH part of an URL - which
remains when

- the preceding protocol, host and port specifications (HOSTPATH)

- and the trailing question mark plus arguments (ARGS)

are stripped off - is interpreted according so some rules. The most prominent
ones are:

- If a path starts with an exclamation-mark ('!'), the rest (without the '!')
  is taken as the name of a Lisp function to be called. All arguments
  following the question mark are passed to that function.

- If a path ends with \".l\" (a dot and a lower case 'L'), it is taken as a
  Lisp source file name to be (load)ed. This is the most common case, and we
  use it in our example \"project.l\".

- If the extension of a file name matches an entry in the global mime type
  table *Mimes, the file is sent to the client with mime-type and max-age
  values taken from that table.

- Otherwise, the file is sent to the client with a mime-type of
  \"application/octet-stream\" and a max-age of 1 second.

An application is free to extend or modify the *Mimes table with the mime
function. For example

#+begin_src picolisp
 (mime \"doc\" \"application/msword\" 60)
#+end_src

defines a new mime type with a max-age of one minute.

Argument values (ARGS) in URLs, following the path and the
question mark, are encoded in such a way that Lisp data types are
preserved:

- An internal symbol starts with a dollar sign ('$')

- A number starts with a plus sign ('+')

- An external (database) symbol starts with dash ('-')

- A list (one level only) is encoded with underscores ('_')

- Otherwise, it is a transient symbol (a plain string)

In that way, high-level data types can be directly passed to
functions encoded in the URL, or assigned to global variables
before a file is loaded. The PicoLisp application-framework uses
a somewhat specialised syntax when communicating URLs.

If LISP-P is non-nil, it is assumed that the buffer-content of
the URL buffer is valid Emacs Lisp that can be processed by
`read-from-string' and can be returned as a lisp
object (typically a list), otherwise the buffer-content is
returned 'as-is' as buffer-string without properties."
  (let* ((base-url (or hostpath iorg-default-host-path))
         (url-no-args
          (concat base-url "/" path))
         (url (if args
                  (concat url-no-args
                          "?"
                          (mapconcat 'identity args "&"))
                url-no-args))
         (buf (with-current-buffer
                  (url-retrieve-synchronously url)
                (buffer-substring-no-properties
                 (point-min) (point-max)))))
    (if LISP-P
        (car (read-from-string buf))
      buf)))

;; **** Edit Database Objects

;; ** Commands


(defun iorg-set-default-host-path (path)
  "Change `iorg-default-host-path' temporarily.

The new PATH will remain valid until set again or until `iorg.el'
is loaded again. In the latter case it will be reset to
\"http://localhost:5000\"."
 (interactive "sURL (e.g. http://localhost:5000): ")
 (setq iorg-default-host-path path))


;; TODO:
;; 1. prompt user for target (no prefix) or target and label (with prefix)
;; 2. prompt user for url (with other prefix)
;; 3. add var iorg-default-path and fun iorg-set-default-path (for url)
;; 4. offer completion for target by extracting all unique docs from url

(defun iorg-insert-internal-link (name &optional lbl)
  "Insert internal-link in PicoLisp-Wiki syntax.

Such a link can take two forms:

 1. ={target}
 2. ={target label}

where 'target' is the NAME of the wiki document linked to and
'label' (optional argument LBL) is the text that will be shown as
clickable link when the document is rendered in the wiki."
  (interactive
   (let ((query "iorg/queries/allDocNames.l"))
     (cond
      ((equal current-prefix-arg nil)
       (list
        (ido-completing-read
         "Target: " (iorg-retrieve-url query 'LISP-P))))
      (t
       (list
        (ido-completing-read
         "Target: " (iorg-retrieve-url query 'LISP-P))
        (read-string "Label: "))))))
  (insert (format "={%s%s}"
                  name
                  (if lbl (concat " " lbl) ""))))

;; (defun iorg-get-wiki-files ()
;;   "Return a list with names of all current iOrg wiki files."
;;   (mapcar
;;    #'(lambda (nm) (if (stringp nm) nm (symbol-name nm)))
;;   (iorg-retrieve-url


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys


;; * Obsolete Stuff

(defun iorg--tag-elems-with-id-attributes (tree)
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

(defun iorg--collect-children (tree)
  "Return alist with '(elem-id . parent-id)' pairs.
The data is collected from parse TREE."
  (let (child-lst)
    (org-element-map tree 'headline
      (lambda (--headline)
        (push (cons (org-element-property :elem-id --headline)
                    (org-element-property :parent --headline))
              child-lst)))
    child-lst))

(defun iorg--add-children-list (tree)
  "Add ':children' property to each headline in parse TREE.
Assumes that all headlines are tagged with an ':elem-id' property
and that the circular-list read-syntax of the ':parent' attribute
has been replaced with simple integer values (the :elem-id of the
elements parent)."
  (let ((pairs (iorg--collect-children tree)))
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

(defun iorg--unwind-circular-list (tree)
  "Replace circular links with unique ID's in parse TREE."
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


(defun iorg--collect-plist-keys (plist)
  "Return a list with all keywords in Emacs Lisp PLIST."
  (and (iorg--elisp-plist-p plist)
       (remove
        t
        (mapcar
         (lambda (--elem)
           (or (not (keywordp --elem)) --elem))
         plist))))


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

;; FIXME obsolete
(defun iorg--convert-plists-to-picolisp (tree)
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
             (props (if (iorg--elisp-plist-p plist)
                        (iorg--collect-plist-keys plist)
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
;;                 (props (if (iorg--elisp-plist-p plist)
;;                            (iorg--collect-plist-keys plist)
;;                          (error "%s is not an Emacs Lisp plist"
;;                                 plist))))
;;            (cons (car elem)
;;                  (mapcar
;;                   (lambda (prop)
;;                     (cons (org-element-property prop elem) prop))
;;                   props))))))



;; * Run hooks and provide

(provide 'iorg)

;; iorg.el ends here
