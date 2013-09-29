## Collect all Docs in Wiki, sorted by name.

## (print (collect 'nm '+Doc NIL T 'nm))
## (print (mapcar stamp (collect 'nm '+Doc NIL T 'mup 'd)))
## (print (mapcar get (collect 'nm '+Doc NIL T 'mup 'u) 'name))

(iter (tree 'nm '+Doc)
   '((D) (println
            (let inf (info (blob (get D 'mup) 'txt))
               (stamp (cadr inf) (cddr inf)) )
            (get (get (get D 'mup) 'u) 'nm)
            (car (info (blob (get D 'mup) 'txt)))
            (get D 'nm) ) ) )
