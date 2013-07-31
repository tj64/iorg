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

;; *** iOrg Scrape Mode

(define-derived-mode iorg-scrape-mode
  inferior-picolisp-mode "iOrg Scrape"
  "Major-mode for GUI-scripting via an inferior PicoLisp process.

The following commands are available:
\\{iorg-scrape-mode-map}
\\{iorg-scrape-run}
\\{iorg-scrape-expect}
\\{iorg-scrape-click}
\\{iorg-scrape-press}
\\{iorg-scrape-value}
\\{iorg-scrape-enter}
\\{iorg-scrape-display}
\\{iorg-scrape-display-fields}
\\{iorg-scrape-display-all}

See docstring of `inferior-picolisp-mode' for more information.")
  ;; Customize in inferior-iorg-scrape-mode-hook
  ;; (setq comint-input-filter (function iorg-scrape-input-filter)))

;; *** Quick Scrape Mode

(define-derived-mode iorg-quick-scrape-mode
  iorg-scrape-mode "Quick Scrape"
  ;; inferior-picolisp-mode "Quick Scrape"
  "Major-mode for quick GUI-scripting via inferior PicoLisp process.

The following commands are available:
\\{iorg-quick-scrape-mode-map}
\\{iorg-scrape-run}
\\{iorg-scrape-expect}
\\{iorg-scrape-click}
\\{iorg-scrape-press}
\\{iorg-scrape-value}
\\{iorg-scrape-enter}
\\{iorg-scrape-display}
\\{iorg-scrape-display-fields}
\\{iorg-scrape-display-all}

See docstring of `inferior-picolisp-mode' for more information.")
  ;; Customize in inferior-iorg-scrape-mode-hook
  ;; (setq comint-input-filter (function iorg-quick-scrape-input-filter)))
(put 'quick-scrape-mode 'mode-class 'special)

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

;; ** Exporter backend definitions

;; (org-export-define-derived-backend 'ihtml 'html ...)

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

(defvar iorg-default-host-path "http://localhost:5000"
  "Default path (protocol, host, port) for iOrg server.")

;; FIXME all types covered?
(defvar iorg-all-types
  (append '(org-data) org-element-all-elements
  org-element-all-objects '(plain-text))
  "Default types to be selected by `org-element-map'.")

;; ** Hooks
;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions

(defsubst iorg--elisp-plist-p (lst)
  "Return non-nil if LST is a list and its car a keyword."
  (and (listp lst) (keywordp (car lst))))

(defun iorg-scrape-write-and-return-filter (proc string)
  "Write output STRING to process buffer of PROC an return it."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc))))))
  string)


(defun iorg-scrape-return-as-lisp-filter (proc lisp-obj-as-string)
  "Return output STRING of process PROC."
  (condition-case err
      ;; (message "PROC: %s\n STRING: %s\n"
      ;;          proc lisp-obj-as-string)
      (car (read-from-string lisp-obj-as-string))))  
    ;; (error (message
    ;;         "An error happened when reading-from-string: %s" err))))

(defun iorg-quick-scrape-input-filter ()
  "Input filter for `iorg-quick-scrape-mode'.")


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

    ;; (with-current-buffer (url-retrieve url nil)
    ;;   (buffer-string))))

;; (defun kvplist->alist (plist)
;;   "Convert PLIST to an alist.

;; The keys are expected to be :prefixed and the colons are removed.
;; The keys in the resulting alist are symbols."
;;   ;; RECURSION KLAXON
;;   (when plist
;;     (destructuring-bind (key value &rest plist) plist
;;       (cons `(,(keyword->symbol key) . ,value)
;;             (kvplist->alist plist)))))

;; (defun iorg-scrape-add-to-output-filters (filter)
;;   "Cons FILTER function to `comint-output-filter-functions'."


(defun iorg--add-elem-id (tree)
  "Add ':elem-id' property to each element of parse TREE."
  (let ((counter 1))
    (org-element-map tree (append '(org-data) iorg-default-map-types)
      (lambda (elem)
        (unless (eq (org-element-type elem) 'org-data)
          (org-element-put-property elem :elem-id counter))
        (setq counter (1+ counter)))))
  tree)

(defun iorg--unwind-circular-list (tree)
  "Replace circular links with unique ID's in parse TREE."
    (org-element-map tree iorg-all-types
      (lambda (elem)
        (org-element-put-property
         elem :parent
         (let ((par (org-element-property :parent elem)))
           (if (eq (org-element-type par) 'org-data)
               0
             (org-element-property :elem-id par))))))
    tree)

(defun iorg--elisp-plist-keys (plist)
  "Return a list with all keywords in PLIST."
  (and (iorg--elisp-plist-p plist)
       (remove
        t
        (mapcar
         (lambda (--elem)
           (or (not (keywordp --elem)) --elem))
         plist))))

(defun iorg--nil-and-t-to-uppercase (tree)
  "Takes a parse TREE as string and upcases nil and t."
  (and (stringp tree)
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
        tree)))

(defun iorg--convert-plists-to-picolisp (tree)
  "Convert all elements plists in TREE to PicosLisp plists.

Assume TREE is a parse-tree produced by `org-element-parse-buffer' and turned
into a non-circular list by applying `iorg--add-elem-id' and
`iorg--unwind-circular-list' on it. Return the parse-tree in PicoLisp
compliant form."
  (org-element-map
      tree
      iorg-all-types
      ;; iorg-default-map-types
    (lambda (elem)
      ;; (let ((type (org-element-type elem)))
      ;;   ;; (and (not (eq type 'plain-text))
      ;;        ;; (not (eq type 'org-data))
      (let* ((plist (cadr elem))
             (props (if (iorg--elisp-plist-p plist)
                        (iorg--elisp-plist-keys plist)
                      (error "%s is not an Emacs Lisp plist"
                             plist))))
        (message "%s"
        (cons (car elem)
              (list
               (mapcar
                (lambda (prop)
                  (cons (org-element-property prop elem) prop))
                props)))))))
  ;; 'iorg--transform-elements-plist-to-picolisp-plist)
  (message "%s" tree)
  tree)

;; (defun iorg--transform-elements-plist-to-picolisp-plist (elem)
;;   "Convert elisp plist of ELEM into PicoLisp plist."
;;   (let ((type (org-element-type elem)))
;;     (and type
;;          (not (eq type 'plain-text))
;;          ;; (not (eq type 'org-data))
;;          (let* ((plist (cadr elem))
;;                 (props (if (iorg--elisp-plist-p plist)
;;                            (iorg--elisp-plist-keys plist)
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

Optional argument DATA should should be part of or an entire
parse-tree as returned by `org-element-parse-buffer', optional
argument BUFFER-OR-FILE is either the name of an existing
Org-mode buffer or the name of an Org-mode file.

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

Since `iorg-parsetree-to-alist' is just a convenience function
built on top of `org-element-parse-buffer' and `org-element-map',
the arguments are the same as for these two functions, so you can
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
     (format "%s"
     (iorg--convert-plists-to-picolisp
      (iorg--unwind-circular-list
       (iorg--add-elem-id
        (if map?
            (org-element-map
                dat typ fun inf 1st-match no-recur with-affil)
          dat))))))))

;; (defun iorg-pico-to-org ()
;;   "")


;; ** Commands

;; *** iOrg Scrape Mode

(defun iorg-scrape-run (&optional port host how local)
  "Run inferior Picolisp and setup process for GUI-scripting.

The _XXX_ separators in some of the PicoLisp command-line
arguments will be replaced later with blanks while being
processed in the `run-picolisp' commands. More precisely, the
replacement takes place after the formatted command-string has
been splitted by blanks. Otherwise, multi-word arguments like
-'scrape \"localhost\" 5000' would be splitted into several
undefined individual arguments that cause errors when given to
PicoLisp. This is a hack necessary because of the way the
`run-picolisp' functions are implemented in
`inferior-picolisp.el'."
  (interactive
   (cond
    ((equal current-prefix-arg nil) nil)
    ((equal current-prefix-arg '(4))
     (list
      (read-number "Port: ")))
    ((equal current-prefix-arg '(16))
     (list
      (read-number "Port: ")
      (read-string "Host: ")))
    ((equal current-prefix-arg '(32))
     (list
      (read-number "Port: ")
      (read-string "Host: ")
      (read-string "How: ")))
    (t
     (list
      (read-number "Port: ")
      (read-string "Host: ")
      (read-string "How: ")
      (read-string "Local: ")))))
  (let* ((hst (or host "localhost"))
         (prt (or port 5000))
         (cmd (format
               "%s %s %s %s %s"
               (if local "./pil" "pil")
               "@lib/http.l"
               "@lib/scrape.l"
               (format "%s_XXX_%S_XXX_%s_XXX_%S"
                       "-scrape"
                       hst
                       prt
                       (or how ""))
               "+")))
    (run-picolisp-new cmd 'IORG-SCRAPE-MODE-P)))

(defun iorg-scrape-get-labels (&optional type proc-buf)
  "Return list of TYPE-labels for currently visited iOrg page.
 If PROC-BUF is nil, current-buffer is used as process buffer. TYPE is either
 'click' (for links) or 'press' (for buttons)."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list (ido-completing-read "Label Type: "
                            '("click" "press"))))
    (t
     (list
      (ido-completing-read "Label Type: "
                       '("click" "press"))
      (ido-read-buffer "Process Buffer: " "*iorg-scrape*")))))
  (let ((process-buffer (or proc-buf (current-buffer))))
    (mapcar
     'identity
     (split-string
      (car 
       (with-current-buffer process-buffer
         (comint-redirect-results-list
          "(display)"
          (concat
           "\\(^" type " \\)"
           "\\(.*$\\)")
          2)))
      "\" ?\"?" 'OMIT-NULLS))))
      ;; " ?\" ?" 'OMIT-NULLS))))

;; (defun iorg-scrape-get-labels (&optional type proc-buf)
;;   "Return list of TYPE-labels for currently visited iOrg page.
;; If PROC-BUF is nil, current-buffer is used as process buffer. TYPE is either
;; 'click' (for links) or 'press' (for buttons)."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)
;;      (list (ido-completing-read  "Label Type: " '("*Links" "*Buttons"))))
;;     ((equal current-prefix-arg '(4))
;;      (list (ido-completing-read  "Label Type: " '("*Links" "*Buttons"))
;;            (ido-read-buffer "Process Buffer: " "*iorg-scrape*")))))
;;   (let* ((process-buffer (or proc-buf (current-buffer)))
;;          (proc (get-buffer-process process-buffer))
;;          ;; save old process-filter
;;          (old-process-filter (process-filter proc))
;;          label-list)
;;     (and (process-live-p proc)
;;          ;; set new process-filter that returns output as lisp object
;;          (unwind-protect
;;              (progn
;;                (set-process-filter proc 'iorg-scrape-return-as-lisp-filter)
;;                (setq label-list (comint-simple-send
;;                                  process-buffer
;;                                  (format "(mapcar car %s)" type))))
;;            ;; restore old process-filter
;;            (set-process-filter proc old-process-filter))
;;          (message "%s" label-list))))

    ;;       (mapcar
    ;;                   'identity
    ;;                    ;; (lambda (--lbl)
    ;;                    ;;    (format "%S" --lbl))
    ;;                   (split-string
    ;;                    (car
    ;;                     (with-current-buffer process-buffer
    ;;                       ;; TODO fixme
    ;;                       (comint-redirect-results-list-1
    ;;                        "(display)"
    ;;                        (concat
    ;;                         "\\(^" type " \\)"
    ;;                         "\\(.*$\\)")
    ;;                        2)))
    ;;                    "\" ?\"?" 'OMIT-NULLS)))
    ;;                    ;; " " 'OMIT-NULLS))))
    ;; (list process-buffer type label-list)))

;; (defun iorg-scrape-get-link-labels (&optional proc-buf)
;;   "Get link labels of current page."
;;   (interactive
;;    (unless (equal current-prefix-arg nil)
;;      (list
;;       (read-buffer "Process Buffer: " nil t))))
;;   (condition-case err
;;       (let* ((proc (if (and proc-buf (not (string= proc-buf "")))
;;                        proc-buf
;;                      (current-buffer)))
;;              (lbls (iorg-scrape-get-labels "click" proc)))
;;         (cons proc lbls))
;;     (error (error "Could not get link labels: %s" err))))

;; (defun iorg-scrape-get-button-labels (&optional proc-buf)
;;   "Get button labels of current page."
;;   (interactive
;;    (unless (equal current-prefix-arg nil)
;;      (list
;;       (read-buffer "Process Buffer: "))))
;;   (condition-case err
;;       (let* ((proc (or proc-buf (current-buffer)))
;;              (lbls (iorg-scrape-get-labels "press" proc)))
;;         (list lbls proc))
;;     (error (error "Could not get button labels: %s" err))))

(defun iorg-scrape-expect (cons-cell &optional proc-buf)
  "Send `expect' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-string "Cons Cell: ")))
    (t
     (list
      (read-string "Cons Cell: ")
      (read-buffer "Process Buffer: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "(expect %s)" cons-cell))))

(defun iorg-scrape-click-or-press (&optional cnt)
  ;; (lbl &optional proc-buf cnt)
  "Send `click' or `press' to inferior PicoLisp process."
  (interactive "P")
  (let* ((lst ;(let ((current-prefix-arg '(4)))
                (call-interactively 'iorg-scrape-get-labels));)
         (proc (and lst (car lst)))
         (type (and lst (cadr lst)))
         (lbls (and lst (caddr lst)))
         ;; TODO better mini-buffer completion
         (lbl (ido-completing-read
               (cond
                ((string-equal type "click")
                   "Link Label: ")
                ((string-equal type "press")
                   "Button Label: ")
                (t (error "No valid label type!")))
                 lbls))
         (cnt (and (equal current-prefix-arg '(16))
                   (read-number "Count: "))))
    (comint-simple-send
     proc
     (format "(%s %s%s)"
           (if (string-equal type "click") "click" "press")
           (or lbl "")
           (if cnt (concat " " cnt) "")))))


(defun iorg-scrape-click (lbl &optional proc-buf cnt)
  "Send `click' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-string "Label: ")))
    ((equal current-prefix-arg '(4))
     (list
      (read-string "Label: ")
      (read-buffer "Process Buffer: ")))
    (t
     (list
      (read-string "Label: ")
      (read-buffer "Process Buffer: ")
      (read-number "Count: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "(click %s %s)" (or lbl "") (or cnt "")))))


(defun iorg-scrape-press (lbl &optional proc-buf cnt)
  "Send `press' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-string "Label: ")))
    ((equal current-prefix-arg '(4))
     (list
      (read-string "Label: ")
      (read-buffer "Process Buffer: ")))
    (t
     (list
      (read-string "Label: ")
      (read-buffer "Process Buffer: ")
      (read-number "Count: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "(press %s %s)" (or lbl "") (or cnt "")))))

(defun iorg-scrape-value (cnt &optional proc-buf fld)
  "Send `value' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-number "Count: ")))
    ((equal current-prefix-arg '(4))
     (list
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")))
    (t
     (list
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")
      (read-string "Field: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "(value %s %s)" (or fld "") (or cnt "")))))

(defun iorg-scrape-enter (str cnt &optional proc-buf fld)
  "Send `enter' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-string "String: ")
      (read-number "Count: ")))
    ((equal current-prefix-arg '(4))
     (list
      (read-string "String: ")
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")))
    (t
     (list
      (read-string "String: ")
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")
      (read-string "Field: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "(click %s %s %s)" (or fld "") str (or cnt "")))))

(defun iorg-scrape-display (&optional proc)
  "Send `display' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil) nil)
    (t (list (read-buffer "Process Buffer: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "%s" '(display)))))

(defun iorg-scrape-display-all (&optional proc)
  "Send `displayAll' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil) nil)
    (t (list (read-buffer "Process Buffer: ")))))
  (let ((process (if proc-buf
                     (get-buffer-process proc-buf)
                   (get-buffer-process (current-buffer)))))
    (comint-simple-send
     process
     (format "%s" '(displayAll)))))

;; (defun iorg-scrape--target (lst lbl cnt))

;; (defun iorg-scrape--field (fld cnt))


(defun iorg-set-default-host-path (path)
  "Change `iorg-default-host-path' temporarily.

The new PATH will remain valid until set again or until `iorg.el'
is loaded again. In the latter case it will be reset to
\"http://localhost:5000\"."
 (interactive "sURL (e.g. http://localhost:5000): ")
 (setq iorg-default-host-path path))

;; (defun iorg-login ()
;;   "")

;; (defun iorg-logout ()
;;   "")

;; (defun iorg-dired ()
;;   "")

;; (defun iorg-new ()
;;   "")

;; (defun iorg-delete ()
;;   "")

;; (defun iorg-edit ()
;;   "")

;; (defun iorg-done ()
;;   "")

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

  (let ((map iorg-scrape-mode-map))
    (define-key map "\C-c\C-c" nil)
    (define-key map "\C-c\C-g"
      'comint-interrupt-subjob)
    (define-key map "\C-c\C-cd"
      'iorg-scrape-display)
    (define-key map "\C-c\C-cf"
      'iorg-scrape-display-fields)
    (define-key map "\C-c\C-ca"
      'iorg-scrape-display-all)
    (define-key map "\C-c\C-cx"
      'iorg-scrape-expect)
    (define-key map "\C-c\C-cc"
      'iorg-scrape-click)
    (define-key map "\C-c\C-cp"
      'iorg-scrape-press)
    (define-key map "\C-c\C-cv"
      'iorg-scrape-value)
    (define-key map "\C-c\C-cm"
      'iorg-scrape-enter)
    (define-key map "\C-c\C-ci"
      'iorg-dired)
    (define-key map "\C-c\C-ce"
      'iorg-edit)
    (define-key map "\C-c\C-c\C-d"
      'iorg-scrape-display)
    (define-key map "\C-c\C-c\C-f"
      'iorg-scrape-display-fields)
    (define-key map "\C-c\C-c\C-a"
      'iorg-scrape-display-all)
    (define-key map "\C-c\C-c\C-x"
      'iorg-scrape-expect)
    (define-key map "\C-c\C-c\C-c"
      'iorg-scrape-click)
    (define-key map "\C-c\C-c\C-p"
      'iorg-scrape-press)
    (define-key map "\C-c\C-c\C-v"
      'iorg-scrape-value)
    (define-key map "\C-c\C-c\C-m"
      'iorg-scrape-enter)
    (define-key map "\C-c\C-c\C-i"
      'iorg-dired)
    (define-key map "\C-c\C-c\C-e"
      'iorg-edit)

    ;; (define-key map [menu-bar iorg-scrape]
    ;;   (cons (purecopy "iOrg-Scrape") iorg-scrape-menu-map))
    map)

  (let ((map iorg-quick-scrape-mode-map))
    (suppress-keymap map)
    (define-key map "d"
      'iorg-scrape-display)
    (define-key map "f"
      'iorg-scrape-display-fields)
    (define-key map "a"
      'iorg-scrape-display-all)
    (define-key map "x"
       'iorg-scrape-expect)
    (define-key map "c"
      'iorg-scrape-click)
    (define-key map "p"
      'iorg-scrape-press)
    (define-key map "v"
      'iorg-scrape-value)
    (define-key map "m"
      'iorg-scrape-enter)
    (define-key map "i"
      'iorg-dired)
    (define-key map "e"
      'iorg-edit)
    ;; (define-key map [menu-bar iorg-quick-scrape]
    ;;   (cons (purecopy "Quick-Scrape") iorg-quick-0
    ;;         scrape-menu-map))
    map)

;; (unless iorg-quick-scrape-mode-map
;;   (setq iorg-scrape-mode-map (make-keymap))
;;   (supress-keymap iorg-quick-scrape-mode-map)


;; * Run hooks and provide

;; iorg.el ends here
