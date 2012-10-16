RelationalAlgebra
=================

An API for applying relational algebra to solving problems 
within a programming language -- without the need to use a database.



### What's the point? ###

Relational algebra (RA) is great.  SQL is great.  Databases are great.
But all three impose severe limitations on what, how, and where your
data can be processed.

Applying relational algebra as a library, within a general purpose
programming language, allows you to not only reap the benefits of high-level,
declarative data manipulations, but also escape the restrictions that
RA, SQL, and database products impose.

I cover some of these restrictions in more detail
[on my blog](http://mfenwick100.blogspot.com/2012/10/the-limitations-of-typical-relational.html).

I hope (and intend) that this project is significant not only because it's
a powerful RA library (hopefully), but also because it can ease learning
and understanding RA, SQL, and databases, and explore ways of extending 
RA and SQL.



### What are the drawbacks? ###

Database products have been worked on by a lot of super smart people for
a long time.  People who have a much deeper understanding of the field
than I do.  Unfortunately, this means that the library is limited by my
understanding and experience! 

One particularly weak area is performance:  the code is not optimized,
and I would expect it to have huge performance problems dealing with
large data sets.

Also, Haskell developers might notice that the style, clarity, 
organization, and idiom(aticness) of the code isn't great ... I'm still
learning Haskell, suggestions are appreciated!



### Feedback is welcome ###

Please feel free to use any of github's mechanisms (issues, pull requests,
etc.) for leaving me feedback, or send me an email at 
mfenwick100 (at) gmail.com if you liked this library, know how to improve it,
find mistakes, dislike it, or just found it interesting!
