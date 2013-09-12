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
(require 'org-element)
(require 'inferior-picolisp)
(require 'outorg)

(eval-when-compile (require 'cl))

;; * Mode and Exporter definitions

;; ** Mode definitions

;; *** iOrg Minor Mode

;; FIXME  \\[iorg-link] iorg-link ??
(define-minor-mode iorg-minor-mode
   "Minor mode for Org-mode buffers generated by outorg.
There is a mode hook, and a few commands:
\\[iorg-login] iorg-login
\\[iorg-logout] iorg-logout
\\[iorg-dired] iorg-dired
\\[iorg-new] iorg-new
\\[iorg-delete] iorg-delete
\\[iorg-edit] iorg-edit
\\[iorg-done] iorg-done
\\[iorg-insert-internal-link] iorg-insert-internal-link"
  :lighter " iOrg")

;; (define-derived-mode iorg-dired-mode ...)

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

;; FIXME all types covered?
(defvar iorg-all-types-no-text
  (append '(org-data) org-element-all-elements
  org-element-all-objects)
  "Types to be selected by `org-element-map'.")

;; FIXME all types covered?
(defvar iorg-all-types
  (append '(org-data) org-element-all-elements
  org-element-all-objects '(plain-text))
  "Types to be selected by `org-element-map'.")

(defvar iorg-default-host-path "http://localhost:5000"
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

(defun iorg--add-elem-id (tree)
  "Add ':elem-id' property to each element of parse TREE."
  (let ((counter 1))
    (org-element-map tree iorg-default-map-types
      (lambda (--elem)
        (org-element-put-property --elem :elem-id counter)
        (setq counter (1+ counter)))))
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
           (infile (plist-get buf-attr :input-file)))
      (setcar (cdr tree)
              (list
               :ID (make-temp-name
                    (concat
                     (file-name-nondirectory
                      (file-name-sans-extension infile)) "_"))
               :elem-id 0
               :input-file infile
               :date (plist-get env-attr :date)
               :author (when author
                         (substring-no-properties (car author)))
               :creator (plist-get env-attr :creator)
               :email (plist-get env-attr :email)
               :description (when descr
                              (substring-no-properties (car descr))))))
    tree))

(defun iorg--unwind-circular-list (tree)
  "Replace circular links with unique ID's in parse TREE."
    (org-element-map tree iorg-all-types
      (lambda (--elem)
        (org-element-put-property
         --elem :parent
         (let ((par (org-element-property :parent --elem)))
           (if (eq (org-element-type par) 'org-data)
               0
             (org-element-property :elem-id par))))))
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

(defun iorg--nil-and-t-to-uppercase (tree-as-string)
  "Takes a parse TREE-AS-STRING and upcases nil and t."
  (and (stringp tree-as-string)
       (replace-regexp-in-string
        (concat
         "\\( t \\|(t \\| t)\\|(t)\\|"
         " nil \\|(nil \\| nil)\\|(nil)\\)")
        '(lambda (match)
           (cond
            ((string= match " t ")
             (format "%s" " T "))
            ((string= match "(t ")
             (format "%s" "(T "))
            ((string= match " t)")
             (format "%s" " T)"))
            ((string= match "(t)")
             (format "%s" "(T)"))
            ((string= match " nil ")
             (format "%s" " NIL "))
            ((string= match "(nil ")
             (format "%s" "(NIL "))
            ((string= match " nil)")
             (format "%s" " NIL)"))
            ((string= match "(nil)")
             (format "%s" "(NIL)"))))
        tree-as-string)))

(defun iorg--fix-text-properties-read-syntax (tree-as-string)
  "Returns parse TREE-AS-STRING with text-properties read syntax fixed.
Fixed means, in this case, adjusted for the PicoLisp reader, i.e. with the
PicoLisp comment character '#' replaced by function `hashtag'."
  (let ((strg tree-as-string))
    (and (stringp strg)
         (setq strg (replace-regexp-in-string
                     "#(" "(hashtag (" strg))
         ;; FIXME strg is whole parse-tree!
         (setq strg (concat strg ")")))))

;; FIXME obsolete
(defun iorg--convert-plists-to-picolisp (tree)
  "Convert all elements plists in TREE to PicosLisp plists.

Assume TREE is a parse-tree produced by `org-element-parse-buffer' and turned
into a non-circular list by applying `iorg--add-elem-id' and
`iorg--unwind-circular-list' on it. Return the parse-tree in PicoLisp
compliant form."
  (org-element-map
      tree
      iorg-all-types-no-text
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

(defun iorg-org-to-pico
  (&optional data buffer-or-file &rest args)
  "Converts an org-element parse-tree into an alist.

Optional argument DATA should be part of or an entire parse-tree
as returned by `org-element-parse-buffer', optional argument
BUFFER-OR-FILE is either the name of an existing Org-mode buffer
or the name of an Org-mode file.

ARGS, if given, can be any combination of the following key/value
pairs (here given with example values):

  :granularity 'object
  :visible-only t
  :types '(append org-element-all-elements
           org-element-all-objects)
  :fun '(lambda (L) (list (car L) (kvplist->alist (cadr L))))
  :info '(:option1 value1 :option2 value2)
  :first-match t
  :no-recursion '(headline table)
  :with-affiliated t

Since `iorg-org-to-pico' is just a convenience function built on
top of `org-element-parse-buffer' and `org-element-map', the
arguments are the same as for these two functions, so you can
consult their doc-strings for more information."
  (let* ((map? (and args
                    (or
                     (plist-get args :types)
                     (plist-get args :fun)
                     (plist-get args :info)
                     (plist-get args :first-match)
                     (plist-get args :no-recursion)
                     (plist-get args :with-affiliated))))
         (print-circle t)
         (buf (or (and buffer-or-file
                       (or (get-buffer buffer-or-file)
                           (if (and
                                (file-exists-p buffer-or-file)
                                (string=
                                 (file-name-extension buffer-or-file) "org"))
                               (find-file-noselect buffer-or-file)
                             (error "File %s is not a valid Org file"
                                    buffer-or-file))))
                  (current-buffer)))
         (gran (or (and args (plist-member args :granularity)
                        (plist-get args :granularity))
                   'object))
         (vis (and args (plist-get args :visible-only)))
         (dat (or data
                  (with-current-buffer buf
                    (org-element-parse-buffer gran vis))))
         (typ (or (and args (plist-get args :types))
                  iorg-all-types))
         (fun (or (and args (plist-get args :fun)) 'identity))
         (inf (and args (plist-get args :info)))
         (1st-match (and args (plist-get args :first-match)))
         (no-recur (and args (plist-get args :no-recursion)))
         (with-affil (and args (plist-get args :with-affiliated))))
    (iorg--nil-and-t-to-uppercase
     (iorg--fix-text-properties-read-syntax
      (format
       "%s"
       (iorg--tag-org-data-element
        (iorg--add-children-list
         ;; (iorg--convert-plists-to-picolisp
         (iorg--unwind-circular-list
          (iorg--add-elem-id
           (if map?
               (org-element-map
                   dat typ fun inf 1st-match no-recur with-affil)
             dat))))
        buf))))))

;; (defun iorg-pico-to-org ()
;;   "")

;; *** Query Database

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

;; *** Edit Database Objects

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

;; (defun iorg-insert-internal-link ()
;;   "Insert internal-link in PicoLisp-Wiki syntax.

;; Such a link can take two forms:

;;  1. ={target}
;;  2. ={target label}

;; where 'target' is the name of the wiki document linked to and
;; 'label' is the text that will be shown as clickable link when the
;; document is rendered in the wiki."
;;   (interactive)
;;   )


;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys

;;   (let ((map iorg-XXX-mode-map))
;;     (define-key map "\C-c\C-c" nil)
;;     (define-key map "\C-c\C-g"
;;       'comint-interrupt-subjob)
;;     (define-key map "\C-c\C-cd"
;;       'iorg-scrape-display)
;;     (define-key map "\C-c\C-cf"
;;       'iorg-scrape-display-fields)
;;     (define-key map "\C-c\C-ca"
;;       'iorg-scrape-display-all)
;;     (define-key map "\C-c\C-cx"
;;       'iorg-scrape-expect)
;;     (define-key map "\C-c\C-cc"
;;       'iorg-scrape-click)
;;     (define-key map "\C-c\C-cp"
;;       'iorg-scrape-press)
;;     (define-key map "\C-c\C-cv"
;;       'iorg-scrape-value)
;;     (define-key map "\C-c\C-cm"
;;       'iorg-scrape-enter)
;;     (define-key map "\C-c\C-ci"
;;       'iorg-dired)
;;     (define-key map "\C-c\C-ce"
;;       'iorg-edit)
;;     (define-key map "\C-c\C-c\C-d"
;;       'iorg-scrape-display)
;;     (define-key map "\C-c\C-c\C-f"
;;       'iorg-scrape-display-fields)
;;     (define-key map "\C-c\C-c\C-a"
;;       'iorg-scrape-display-all)
;;     (define-key map "\C-c\C-c\C-x"
;;       'iorg-scrape-expect)
;;     (define-key map "\C-c\C-c\C-c"
;;       'iorg-scrape-click)
;;     (define-key map "\C-c\C-c\C-p"
;;       'iorg-scrape-press)
;;     (define-key map "\C-c\C-c\C-v"
;;       'iorg-scrape-value)
;;     (define-key map "\C-c\C-c\C-m"
;;       'iorg-scrape-enter)
;;     (define-key map "\C-c\C-c\C-i"
;;       'iorg-dired)
;;     (define-key map "\C-c\C-c\C-e"
;;       'iorg-edit)

;;     ;; (define-key map [menu-bar iorg-scrape]
;;     ;;   (cons (purecopy "iOrg-Scrape") iorg-scrape-menu-map))
;;     map)

;;   (let ((map iorg-quick-XXX-mode-map))
;;     (suppress-keymap map)
;;     (define-key map "d"
;;       'iorg-scrape-display)
;;     (define-key map "f"
;;       'iorg-scrape-display-fields)
;;     (define-key map "a"
;;       'iorg-scrape-display-all)
;;     (define-key map "x"
;;        'iorg-scrape-expect)
;;     (define-key map "c"
;;       'iorg-scrape-click)
;;     (define-key map "p"
;;       'iorg-scrape-press)
;;     (define-key map "v"
;;       'iorg-scrape-value)
;;     (define-key map "m"
;;       'iorg-scrape-enter)
;;     (define-key map "i"
;;       'iorg-dired)
;;     (define-key map "e"
;;       'iorg-edit)
;;     ;; (define-key map [menu-bar iorg-quick-scrape]
;;     ;;   (cons (purecopy "Quick-Scrape") iorg-quick-0
;;     ;;         scrape-menu-map))
;;     map)

;; ;; (unless iorg-quick-scrape-mode-map
;; ;;   (setq iorg-scrape-mode-map (make-keymap))
;; ;;   (supress-keymap iorg-quick-scrape-mode-map)


;; * Run hooks and provide

;; iorg.el ends here
