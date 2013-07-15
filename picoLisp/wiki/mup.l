# 20aug12abu
# (c) Software Lab. Alexander Burger

(must "Markup" Wiki)

(menu "Markup"
   (<h3> NIL "Markup")
   (form NIL
      (<h3> NIL
         (<id> (datStr (: d)) " " (tim$ (: t) T))
         " -- "
         (<$> (: obj u nm) (: obj u)) )
      (editButton '(may System))
      (<grid> 3
         "Document"
         (choDoc 0)
         (gui '(+E/R +Obj +TextField) '(doc : home obj) '(nm +Doc) 40)
         "User"
         (choUser 0)
         (gui '(+E/R +Obj +TextField) '(u : home obj) '(nm +User) 40)
         "Date" NIL
         (gui '(+E/R +DateField) '(d : home obj) 10)
         "Time" NIL
         (gui '(+E/R +TimeField) '(t : home obj) 10) )
      (<spread>
         (with (: obj <)
            (<$> (pack "Previous " (nm> This)) This) )
         (with (: obj >)
            (<$> (pack "Next " (nm> This)) This) ) )
      (gui '(+BlobField) '(txt : home obj) 72 20)
      (--)
      (gui '(+E/R +TextField) '(sum : home obj) 40 "Summary")
      (<nbsp> 3)
      (gui '(+E/R +Checkbox) '(min : home obj) "This is a minor edit")
      (<hr>)
      (<grid> 3
         (prog
            (gui '(+E/R +Chart) '(bin : home obj) 3
               '((This) (list (: nm)))
               '((L D)
                  (and D (put!> D 'nm (car L)))
                  D ) )
            (<table> NIL "Binaries" '((NIL "Name"))
               (do 6
                  (<row> NIL
                     (gui 1 '(+Able +TextField) '(curr) 30)  # Binary name
                     (gui 2 '(+DelRowButton))
                     (gui 3 '(+BubbleButton)) ) ) )
            (scroll 6 T) )
         (<nbsp> 3)
         (prog
            (gui '(+E/R +Chart) '(rpl : home obj) 5
               '((This) (list (: mup u) This (: mup t)))
               cadr )
            (<table> NIL "Replies"
               '((NIL "User") (NIL "Version"))
               (do 6
                  (<row> NIL
                     (gui 1 '(+ObjView +TextField) '(: nm) 20)
                     (gui 2 '(+ObjView +DateField) '(: mup d) 10)
                     (gui 3 '(+TimeField) 10)
                     (gui 4 '(+DelRowButton))
                     (gui 5 '(+BubbleButton)) ) ) )
            (scroll 6 T) ) )
      (<hr>)
      (<spread> NIL (editButton '(may System))) ) )

# vi:et:ts=3:sw=3
