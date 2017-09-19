

parse (show form1)
parse (show form2)
parse (show form3)

parse (show (Equiv form1 form2))
parse (show (Impl form1 form2))
parse (show (Cnj [form1,form2,form3]))
parse (show (Dsj[form1,form2,form3]))

Impl form2 form3
Equiv form2 form3

Cnj [form1,form2,form3]
Dsj[form1,form2,form3]
