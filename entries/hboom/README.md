I'm building a library to manage attributes while drawing pictures.  Attibutes will be things like architectural style, sizes, colours, and so forth.  I'm usinf association lists to mnage tyle informtion, but I suspect that plain old association lists as present in the original Lisp 1.5 may be too simple.  But I won't find out in what way they are oversimplified until I try them out ans see where it gets awkward.

There are a lot of functions that explicitly manipulate association lists, taking asociaation lists as parameters and returning them as results.
But we'd prefer to use ones where the association list is implicit, so we don't fill user code with a lot of lambda's and a's or a-list's.

I'm currently in transition.  The user won't be compounding picts explicitly, but instead compounding functions that return picts.

I suspect the functions that explicitly mention association lists will be
* those that implement operations on association lists
* those that use pict primitives directly.

I suspect that the mechanism of having associations returning functions isn't ideal either.  Combining these functions with freezing is a way of indicating at whata level decisions are made, and having functions that return functions also gives multilevel freezing.  But I suspect I'd really want something more like probability distributions that can be combined and modified variously.

But I suspect the association lists may need other kinds of binding rather than encoding this information in the bound value's type.

As for the instance of door generation I've built so far:
Simply having width and height for a door isn't enough.
Other things have dimensions.  And things within things have context-dependent dimensions.  At the moment I'm using hc-append and vc-append to *add* dimensions bottom-up.  But I'll need to use top-down constraints as well at some point, and even have the presence or absence of subobjects depend on constraints.

Maybe I need doorwidth and doorheight.


TODO:  (K foo) is (lambda () foo)
Should foo be evaluated at call time or lambda time?  In combinatory logic it doesn't matter.  Here it might.

TODO: What's the way of producing run-time errors that abort execution and produce a traceback?  List printing a message containing the word ERROR isn't enough.
