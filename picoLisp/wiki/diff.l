# 18may12abu
# (c) Software Lab. Alexander Burger

(must "Document" Wiki)

(menu "Difference"
   (form NIL
      (<h2> NIL (<id> "Difference"))
      (<$> (nm> (: obj <)) (: obj <))
      "  ->  "
      (<$> (nm> (: obj)) (: obj))
      (<pre> NIL
         (in (list 'diff (blob (: obj <) 'txt) (blob (: obj) 'txt))
            (ht:Prin (till NIL T)) ) ) ) )

# vi:et:ts=3:sw=3
