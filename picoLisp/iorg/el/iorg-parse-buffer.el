(require 'org-element)

(funcall
 'print
 (with-current-buffer
    (find-file-noselect
     "/home/tj/git/iorg/picoLisp/iorg/sandbox/simple-test.org")
   (org-element-map
       (org-element-parse-buffer)
       'table
     '(lambda (tab)
        (org-element-property :begin tab)) nil t)))



;; : (pipe (emx "(message \"%s mal Helllo\" (+ 5 3))"))
;; -> 11
;; : 8 mal Helllo
;; (in (pipe (emx "(message \"%s mal Helllo\" (+ 5 3))")) (setq EX @))
;; -> 11
;; : 8 mal Helllo
;; EX
;; -> 11
