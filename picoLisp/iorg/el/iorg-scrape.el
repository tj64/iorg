;; * iorg-scrape.el --- elisp glue code for `picoLisp/lib/scrape.l'
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
;;   :keywords: emacs org-mode picolisp iorg scrape
;;   :END:

;; ** Commentary
;; *** About iOrg
;; *** Installation
;; *** Bugs and Shortcomings
;; *** Emacs Version
;; ** ChangeLog

;; * Requires

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

See docstring of `inferior-picolisp-mode' for more information."
  ;; Customize in iorg-scrape-mode-hook
  ;; (setq comint-input-filter (function iorg-scrape-input-filter)))
  ;; (setq comint-prompt-regexp "^[^\n:?!]*[?!:]+ *"))
  (setq comint-prompt-regexp "^[?!:]+ *"))


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
  ;; Customize in iorg-scrape-mode-hook
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

;; * Variables
;; ** Hooks
;; ** Vars

(defvar page-links nil
  "Stores a list with labels from page links.")

(defvar page-buttons nil
  "Stores a list with labels from page buttons.")

(defvar page-value-fields nil
  "Stores a list with labels from page fields with values.")

(defvar page-enter-fields nil
  "Stores a list with labels from page fields that accept input.")

;; ** Customs
;; *** Custom Groups
;; *** Custom Vars
;; * Functions
;; ** Non-interactive Functions

;; *** Label/Field-Count Based Functions

(defun iorg-scrape-generic (cmd cnt &optional strg proc)
  "Generic workhorse function for iOrg scrape.
CMD is the function from `scrape.l' to be called, CNT is the
number of the label/field to be addressed, STRG is the string to
enter in a field, and PROC is the PicoLisp process to use."
  (let ((process (or proc (get-buffer-process (current-buffer))))
        (fld (or (string= cmd "enter") (string= cmd "value"))))
    (if (or
         (and strg (not (string= cmd "enter")))
         (and (not strg) (string= cmd "enter")))
        (user-error
         (concat
          "When command is \"enter\" - and only then - "
          "a string to enter must be given."))
      (comint-simple-send
       process
       (format "(%s %s %s %s)"
               cmd
               (if fld cnt "NIL")
               (or (concat "\"" strg "\"") "")
               (if fld "" cnt)))
      (unless
          (or
           (string= cmd "value")
           (string= cmd "displayAll"))
        (comint-simple-send
         process
         (format "%s" '(displayAll)))))))


;; ;; *** Label/Field-Name Based Functions

;; (defun iorg-scrape--split-label-string (label-string)
;;   "Split LABEL-STRING and return a list of labels."
;;   (and label-string
;;        (stringp label-string)
;;        (not (string= label-string ""))
;;        (mapcar 'identity (split-string
;;                             label-string
;;                             "\" ?\"?" 'OMIT-NULLS))))

;; (defun iorg-scrape-write-and-return-filter (proc string)
;;   "Write output STRING to process buffer of PROC an return it."
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (= (point) (process-mark proc))))
;;         (save-excursion
;;           ;; Insert the text, advancing the process marker.
;;           (goto-char (process-mark proc))
;;           (insert string)
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc))))))
;;   string)

;; (defun iorg-scrape-return-as-lisp-filter (proc lisp-obj-as-string)
;;   "Return output STRING of process PROC."
;;   (condition-case err
;;       ;; (message "PROC: %s\n STRING: %s\n"
;;       ;;          proc lisp-obj-as-string)
;;       (car (read-from-string lisp-obj-as-string))))  
;;     ;; (error (message
;;     ;;         "An error happened when reading-from-string: %s" err))))

;; (defun iorg-quick-scrape-input-filter ()
;;   "Input filter for `iorg-quick-scrape-mode'.")


;; ** Commands

;; *** Start Scrape Process

;; Start a REPL
(defun iorg-scrape-repl (&optional port host how local)
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

;; start a non-interactive TCP process
(defun iorg-scrape-run (&optional port host how local)
  "Start minimal PicoLisp server that allows iOrg web scraping.")

;; *** Label/Field-Count Based Commands

(defun iorg-scrape-display-all (&optional proc)
  "Send `displayAll' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil) nil)
    (t (list (read-buffer "Process Buffer: ")))))
  (iorg-scrape-generic "displayAll" nil nil proc))

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


(defun iorg-scrape-click (cnt &optional proc-buf)
  "Send `click' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-number "Count: ")))
    ((numberp current-prefix-arg)
     (list
      (abs current-prefix-arg)))
    (t
     (list
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")))))
  (iorg-scrape-generic "click" cnt nil proc-buf))

(defun iorg-scrape-press (cnt &optional proc-buf)
  "Send `press' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-number "Count: ")))
    ((numberp current-prefix-arg)
     (list
      (abs current-prefix-arg)))
    (t
     (list
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")))))
  (iorg-scrape-generic "press" cnt nil proc-buf))

(defun iorg-scrape-value (cnt &optional proc-buf)
  "Send `value' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-number "Count: ")))
    ((numberp current-prefix-arg)
     (list
      (abs current-prefix-arg)))
    (t
     (list
      (read-number "Count: ")
      (read-buffer "Process Buffer: ")))))
  (iorg-scrape-generic "value" cnt nil proc-buf))

(defun iorg-scrape-enter (cnt strg &optional proc-buf)
  "Send `enter' to inferior PicoLisp process."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list
      (read-number "Count: ")
      (read-string "String: ")))
    ((numberp current-prefix-arg)
     (list
      (abs current-prefix-arg)
      (read-string "String: ")))
    (t
     (list
      (read-number "Count: ")
      (read-string "String: ")
      (read-buffer "Process Buffer: ")))))
  (iorg-scrape-generic "enter" cnt strg proc-buf))

;; *** Label/Field-Name Based Commands

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
         (lbl (and lbls
                   (ido-completing-read
                    (cond
                     ((string-equal type "click")
                      "Link Label: ")
                     ((string-equal type "press")
                      "Button Label: ")
                     (t (error "No valid label type!")))
                    lbls)))
         (cnt (and (equal current-prefix-arg '(16))
                   (read-number "Count: "))))
    (if (or lbl cnt)
      (condition-case err
          (comint-simple-send
           proc
           (format "(%s %S%s)"
                   (if (string-equal type "click") "click" "press")
                   (or lbl "")
                   (if cnt (concat " " cnt) "")))
        (error (message "An error happened when doing \"%s\": %s"
                        type err)))
      (message "Nothing to \"%s\" found." type))))

(defun iorg-scrape-get-labels (&optional type proc-buf output-string)
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
  ;; (let ((process-buffer (or proc-buf (current-buffer))))
  (let* ((process-buffer (or proc-buf (current-buffer)))
         (proc (get-buffer-process process-buffer))
         (label-strg (if output-string
                         ()
                       (car 
                        (with-current-buffer process-buffer
                          (comint-redirect-results-list
                           "(display)"
                           (concat
                            "\\(^" type " \\)"
                            "\\(.*$\\)")
                           2)))))
         (label-list (and label-strg
                          (mapcar
                           'identity
                           (split-string
                            label-strg
                            "\" ?\"?" 'OMIT-NULLS)))))
    (list proc type label-list)))


(defun iorg-scrape--set-labels (output-string)
  "Assign lists with field, button and link labels to variables.
To be added to the `comint-preoutput-filter-functions', with the
set variables to be used by `ido-completing-read'. Recieves the
output string of `displayAll' and parses the contained labels
into four different categories, storing them as lists in these
four related global variables:

 - click :: page-links 
 - press :: page-buttons
 - value :: page-value-fields
 - enter :: page-enter-fields

Returns the original unchanged output-string."
  ;; (let* (;; (process-buffer (current-buffer))
  ;;        ;; (proc (get-buffer-process process-buffer))
  ;;        (label-list
  ;;         (iorg-scrape--split-label-string output-string)))
  (string-match
    "\\(^\\(click: \\|press: \\|value: \\|enter: \\)\\)\\(.*$\\)"
     output-string)
  (message "sub1: %s sub2: %s"
           (match-string 1 output-string)
           (match-string 2 output-string)))

;; "\(^\(click: \|press: \|value: \|enter: \)\)\(.*$\)\(^$\)"
  ;;   (list proc type label-list)))
  ;; )

;; (add-hook 'comint-preoutput-filter-functions
;;           'iorg-scrape-set-completing-read-choices)

;; (remove-hook 'comint-preoutput-filter-functions
;;           'iorg-scrape-set-completing-read-choices)

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


;; (defun iorg-scrape-click (lbl &optional proc-buf cnt)
;;   "Send `click' to inferior PicoLisp process."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)
;;      (list
;;       (read-string "Label: ")))
;;     ((equal current-prefix-arg '(4))
;;      (list
;;       (read-string "Label: ")
;;       (read-buffer "Process Buffer: ")))
;;     (t
;;      (list
;;       (read-string "Label: ")
;;       (read-buffer "Process Buffer: ")
;;       (read-number "Count: ")))))
;;   (let ((process (if proc-buf
;;                      (get-buffer-process proc-buf)
;;                    (get-buffer-process (current-buffer)))))
;;     (comint-simple-send
;;      process
;;      (format "(click %s %s)" (or lbl "") (or cnt "")))))


;; (defun iorg-scrape-press (lbl &optional proc-buf cnt)
;;   "Send `press' to inferior PicoLisp process."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)
;;      (list
;;       (read-string "Label: ")))
;;     ((equal current-prefix-arg '(4))
;;      (list
;;       (read-string "Label: ")
;;       (read-buffer "Process Buffer: ")))
;;     (t
;;      (list
;;       (read-string "Label: ")
;;       (read-buffer "Process Buffer: ")
;;       (read-number "Count: ")))))
;;   (let ((process (if proc-buf
;;                      (get-buffer-process proc-buf)
;;                    (get-buffer-process (current-buffer)))))
;;     (comint-simple-send
;;      process
;;      (format "(press %s %s)" (or lbl "") (or cnt "")))))

;; (defun iorg-scrape-value (cnt &optional proc-buf fld)
;;   "Send `value' to inferior PicoLisp process."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)
;;      (list
;;       (read-number "Count: ")))
;;     ((equal current-prefix-arg '(4))
;;      (list
;;       (read-number "Count: ")
;;       (read-buffer "Process Buffer: ")))
;;     (t
;;      (list
;;       (read-number "Count: ")
;;       (read-buffer "Process Buffer: ")
;;       (read-string "Field: ")))))
;;   (let ((process (if proc-buf
;;                      (get-buffer-process proc-buf)
;;                    (get-buffer-process (current-buffer)))))
;;     (comint-simple-send
;;      process
;;      (format "(value %s %s)" (or fld "") (or cnt "")))))

;; (defun iorg-scrape-enter (str cnt &optional proc-buf fld)
;;   "Send `enter' to inferior PicoLisp process."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)
;;      (list
;;       (read-string "String: ")
;;       (read-number "Count: ")))
;;     ((equal current-prefix-arg '(4))
;;      (list
;;       (read-string "String: ")
;;       (read-number "Count: ")
;;       (read-buffer "Process Buffer: ")))
;;     (t
;;      (list
;;       (read-string "String: ")
;;       (read-number "Count: ")
;;       (read-buffer "Process Buffer: ")
;;       (read-string "Field: ")))))
;;   (let ((process (if proc-buf
;;                      (get-buffer-process proc-buf)
;;                    (get-buffer-process (current-buffer)))))
;;     (comint-simple-send
;;      process
;;      (format "(click %s %s %s)" (or fld "") str (or cnt "")))))

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


(defun iorg-set-default-host-path (path)
  "Change `iorg-default-host-path' temporarily.

The new PATH will remain valid until set again or until `iorg-scrape.el'
is loaded again. In the latter case it will be reset to
\"http://localhost:5000\"."
 (interactive "sURL (e.g. http://localhost:5000): ")
 (setq iorg-default-host-path path))


;; ;; *** Higher Level Commands

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

;; * Menus and Keys
;; ** Menus
;; ** Keys
;; *** Mode Keys

  (let ((map iorg-scrape-mode-map))
    (define-key map "\C-c\C-c" nil)
    (define-key map "\C-c\C-g"
      'comint-interrupt-subjob)
    (define-key map "\C-c\C-cd"
      'iorg-scrape-display-all)
    ;; (define-key map "\C-c\C-cf"
    ;;   'iorg-scrape-display-fields)
    ;; (define-key map "\C-c\C-ca"
    ;;   'iorg-scrape-display-all)
    (define-key map "\C-c\C-cx"
      'iorg-scrape-expect)
    (define-key map "\C-c\C-cc"
      'iorg-scrape-click)
    (define-key map "\C-c\C-cp"
      'iorg-scrape-press)
    (define-key map "\C-c\C-cv"
      'iorg-scrape-value)
    (define-key map "\C-c\C-ce"
      'iorg-scrape-enter)
    ;; (define-key map "\C-c\C-ci"
    ;;   'iorg-dired)
    (define-key map "\C-c\C-cE"
      'iorg-edit)
    (define-key map "\C-c\C-cQ"
      'iorg-quick-scrape-mode)
    (define-key map "\C-c\C-c\C-d"
      'iorg-scrape-display-all)
    ;; (define-key map "\C-c\C-c\C-f"
    ;;   'iorg-scrape-display-fields)
    ;; (define-key map "\C-c\C-c\C-a"
    ;;   'iorg-scrape-display-all)
    (define-key map "\C-c\C-c\C-x"
      'iorg-scrape-expect)
    (define-key map "\C-c\C-c\C-c"
      'iorg-scrape-click)
    (define-key map "\C-c\C-c\C-p"
      'iorg-scrape-press)
    (define-key map "\C-c\C-c\C-v"
      'iorg-scrape-value)
    (define-key map "\C-c\C-c\C-e"
      'iorg-scrape-enter)
    ;; (define-key map "\C-c\C-c\C-i"
    ;;   'iorg-dired)
    (define-key map "\C-c\C-c\C-E"
      'iorg-edit)
    (define-key map "\C-c\C-c\C-Q"
      'iorg-quick-scrape-mode)
    ;; (define-key map [menu-bar iorg-scrape]
    ;;   (cons (purecopy "iOrg-Scrape") iorg-scrape-menu-map))
    map)

  (let ((map iorg-quick-scrape-mode-map))
    (suppress-keymap map)
    (define-key map "d"
      'iorg-scrape-display-all)
    (define-key map "f"
      'iorg-scrape-display-fields)
    ;; (define-key map "a"
    ;;   'iorg-scrape-display-all)
    (define-key map "x"
       'iorg-scrape-expect)
    (define-key map "c"
      'iorg-scrape-click)
    (define-key map "p"
      'iorg-scrape-press)
    (define-key map "v"
      'iorg-scrape-value)
    (define-key map "e"
      'iorg-scrape-enter)
    ;; (define-key map "i"
    ;;   'iorg-dired)
    (define-key map "E"
      'iorg-edit)
    (define-key map "S"
      'iorg-scrape-mode)
    ;; (define-key map [menu-bar iorg-quick-scrape]
    ;;   (cons (purecopy "Quick-Scrape") iorg-quick-0
    ;;         scrape-menu-map))
    map)

;; (unless iorg-quick-scrape-mode-map
;;   (setq iorg-scrape-mode-map (make-keymap))
;;   (supress-keymap iorg-quick-scrape-mode-map)


;; * Run hooks and provide

;; iorg-scrape.el ends here
