# Base Library

The `(scheme base)` library exports many of the procedures and syntax bindings that are traditionally associated with Scheme.

- [`cons-source`](#cons-source)
- [`syntax-rules`](#syntax-rules)
- [`letrec*`](#letrec*)
- [`guard`](#guard)
- [`guard-aux`](#guard-aux)
- [`define-record-type`](#define-record-type)
- [`record?`](#record?)
- [`register-simple-type`](#register-simple-type)
- [`make-type-predicate`](#make-type-predicate)
- [`make-constructor`](#make-constructor)
- [`make-getter`](#make-getter)
- [`make-setter`](#make-setter)
- [`slot-set!`](#slot-set!)
- [`type-slot-offset`](#type-slot-offset)
- [`receive`](#receive)
- [`abs`](#abs)
- [`max`](#max)
- [`min`](#min)
- [`modulo`](#modulo)
- [`floor-remainder`](#floor-remainder)
- [`even?`](#even?)
- [`exact-integer?`](#exact-integer?)
- [`exact?`](#exact?)
- [`inexact?`](#inexact?)
- [`odd?`](#odd?)
- [`complex?`](#complex?)
- [`rational?`](#rational?)
- [`gcd`](#gcd)
- [`lcm`](#lcm)
- [`quotient`](#quotient)
- [`remainder`](#remainder)
- [`truncate-quotient `](#truncate-quotient )
- [`truncate-remainder `](#truncate-remainder )
- [`truncate/ `](#truncate/ )
- [`floor-quotient `](#floor-quotient )
- [`floor-remainder `](#floor-remainder )
- [`floor/ `](#floor/ )
- [`square`](#square)
- [`expt`](#expt)
- [`call-with-current-continuation`](#call-with-current-continuation)
- [`call/cc`](#call/cc)
- [`call-with-values`](#call-with-values)
- [`dynamic-wind`](#dynamic-wind)
- [`values`](#values)
- [`char=?`](#char=?)
- [`char<?`](#char<?)
- [`char>?`](#char>?)
- [`char<=?`](#char<=?)
- [`char>=?`](#char>=?)
- [`string=?`](#string=?)
- [`string<?`](#string<?)
- [`string<=?`](#string<=?)
- [`string>?`](#string>?)
- [`string>=?`](#string>=?)
- [`foldl`](#foldl)
- [`foldr`](#foldr)
- [`not`](#not)
- [`list?`](#list?)
- [`zero?`](#zero?)
- [`positive?`](#positive?)
- [`negative?`](#negative?)
- [`append`](#append)
- [`list`](#list)
- [`make-list`](#make-list)
- [`list-copy`](#list-copy)
- [`map`](#map)
- [`for-each`](#for-each)
- [`list-tail`](#list-tail)
- [`list-ref`](#list-ref)
- [`list-set!`](#list-set!)
- [`reverse`](#reverse)
- [`boolean=?`](#boolean=?)
- [`symbol=?`](#symbol=?)
- [`Cyc-obj=?`](#Cyc-obj=?)
- [`vector`](#vector)
- [`vector-append`](#vector-append)
- [`vector-copy`](#vector-copy)
- [`vector-copy!`](#vector-copy!)
- [`vector-fill!`](#vector-fill!)
- [`vector->list`](#vector->list)
- [`vector->string`](#vector->string)
- [`vector-map`](#vector-map)
- [`vector-for-each`](#vector-for-each)
- [`make-string`](#make-string)
- [`string`](#string)
- [`string-copy`](#string-copy)
- [`string-copy!`](#string-copy!)
- [`string-fill!`](#string-fill!)
- [`string->list`](#string->list)
- [`string->vector`](#string->vector)
- [`string-map`](#string-map)
- [`string-for-each`](#string-for-each)
- [`make-parameter`](#make-parameter)
- [`current-output-port`](#current-output-port)
- [`current-input-port`](#current-input-port)
- [`current-error-port`](#current-error-port)
- [`call-with-port`](#call-with-port)
- [`error`](#error)
- [`raise`](#raise)
- [`raise-continuable`](#raise-continuable)
- [`with-exception-handler`](#with-exception-handler)
- [`Cyc-add-exception-handler`](#Cyc-add-exception-handler)
- [`Cyc-remove-exception-handler`](#Cyc-remove-exception-handler)
- [`newline`](#newline)
- [`write-char`](#write-char)
- [`write-string`](#write-string)
- [`flush-output-port`](#flush-output-port)
- [`read-line`](#read-line)
- [`read-string`](#read-string)
- [`input-port?`](#input-port?)
- [`output-port?`](#output-port?)
- [`input-port-open?`](#input-port-open?)
- [`output-port-open?`](#output-port-open?)
- [`features`](#features)
- [`Cyc-version`](#Cyc-version)
- [`any`](#any)
- [`every`](#every)
- [`and`](#and)
- [`or`](#or)
- [`let`](#let)
- [`let*`](#let*)
- [`letrec`](#letrec)
- [`begin`](#begin)
- [`case`](#case)
- [`cond`](#cond)
- [`cond-expand`](#cond-expand)
- [`do`](#do)
- [`when`](#when)
- [`unless`](#unless)
- [`quasiquote`](#quasiquote)
- [`floor`](#floor)
- [`ceiling`](#ceiling)
- [`truncate`](#truncate)
- [`round`](#round)
- [`exact`](#exact)
- [`inexact`](#inexact)
- [`eof-object`](#eof-object)
- [`syntax-error`](#syntax-error)
- [`bytevector-copy`](#bytevector-copy)
- [`bytevector-copy!`](#bytevector-copy!)
- [`utf8->string`](#utf8->string)
- [`string->utf8`](#string->utf8)
- [`denominator`](#denominator)
- [`numerator`](#numerator)
