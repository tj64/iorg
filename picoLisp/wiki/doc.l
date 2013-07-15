# 28may13abu
# (c) Software Lab. Alexander Burger

# *MupTxt

(must "Document" Wiki)

(de _newMup ()
   (apply 'newMup> (: home mup) (: home obj))
   (init> (: home hist)) )

(menu "Document"
   (ifn *ID
      (prog
         (<h3> NIL "Select Document")
         (form 'dialog (choDoc)) )
      (form NIL
         (<h3> NIL "Document: " (<id> (: nm)))
         (gui 'hist '(+QueryChart) 6
            '(goal '(@Doc (: home obj) (history @Doc @@)))
            6
            '((This) (list This (and (: min) "Minor edit") (: u) (: d) (: t) This)) )
         (<table> 'chart "Revision History"
            '((NIL "Summary") NIL (NIL "User") (NIL "Date") (NIL "Time"))
            (do 6
               (<row> (alternating)
                  (gui 1 '(+ObjView +TextField) '(: sum))
                  (gui 2 '(+TextField))
                  (gui 3 '(+ObjView +TextField) '(: nm))
                  (gui 4 '(+DateField))
                  (gui 5 '(+TimeField))
                  (gui 6 '(+ObjView +TextField) 'diff>
                     '(with (: <)
                        (pack "Difference to " (nm> This)) ) ) ) ) )
         (scroll 6)
         (----)
         (when *MupTxt
            (<hr>)
            (pipe (prinl (: mup 1))
               (<div> NIL (render (: mup 4))) )
            (<hr>)
            (off *MupTxt) )
         (panel '(may Wiki) (pack "Document '@1'")
            '(may Delete)
            '(choDoc)
            'nm '+Doc
            NIL NIL
            '(_newMup) )
         (gui '(+E/R +Val +Cue +TextField)
            '(nm : home obj)
            fold
            "Document" 40 "Name" )
         (----)
         (docFields)
         (<spread> NIL (editButton '(may Wiki) '(_newMup))) ) ) )

# vi:et:ts=3:sw=3
