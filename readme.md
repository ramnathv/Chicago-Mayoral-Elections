We import a dataset into a statistical software package, run a procedure
to get all results, then copy and paste selected pieces in a typesetting
program, add a few descriptions and finish a report. This is a common
practice of writing statistical reports. There are obvious dangers and
disadvantages of this procedure:

1.  it is error-prone due to too much manual work;
2.  it requires lots of human efforts to do tedious jobs such as copying
    results between documents;
3.  the workflow is barely recordable especially when it involves with
    GUI (Graphical User Interface) operations, therefore it is difficult
    to reproduce;
4.  a tiny change of the data source in the future will require the
    author(s) to go through the same procedure again, which can take
    nearly the same amount of time and efforts;
5.  the analysis and writing are mixed together, so attention has to be
    paid to the synchronization of the two parts;

A dynamic report is a report generated dynamically from computer code.
Just like a software package has its source code, a dynamic report also
has its source as a mixture of computer code and normal writings. When
we compile the source report, the code in it is executed and replaced
with the output; we get a final report by mixing the code output with
the original writings. Because we only manage source code, we are free
of all the possible problems above. For example, we can change a single
parameter in the source code, and get a different report on the fly.

This book is written for those who write reports regularly; the reports
here can range from student homework or project reports, exams, books to
virtually any documents related to data analysis or statistical graphics
and computing.

The main tools we introduce in this book are the R language and the
**knitr** package, with which this book is written. We will show how to
improve our efficiency in writing reports, fine tune every aspect of a
report and go from R output to publication quality reports.

Introduction
============

The big idea is from literate programming, an approach introduced by
Donald Knuth. The original idea was mainly for writing software: mix the
source code and documentation together; we can either extract the source
code out (called *tangle*) or execute the code to get the compiled
results (called *weave*). A report is not that different from a computer
program: for a report, we need to run software packages to compile our
ideas (like source code) into numeric or graphical output, and insert
the output into our literal writing (like documentation).

We explain the idea with a trivial example: suppose we need to write the
value of $2\pi$ into a report; of course, we can directly write the
number $6.28$. Now if I change my mind and I want $6\pi$ instead, I may
have to find a calculator, erase the previous value and fill in the new
one. Since it is extremely easy for the computer to calculate $6\pi$,
why not leave this job to the computer completely and free the man of
the labor work? What we need to do is to leave the source code in the
document instead of a hard-coded value, and tell the computer how to
find out the source code. Usually we use special markups for computer
code in the source report, e.g. we can write
`'the value is {% 6 * pi %}'`, in which `{%` and `%}` is a pair of marks
that tell the computer `6 * pi` is source code and should be executed.

If you know a dynamic web language such as PHP (can mix HTML and PHP
code together), the above idea should look familiar. The above example
shows the *inline* code output, which means source code is mixed inline
with a sentence. The other type of output is the *chunk* output, which
gives the results from a whole block of code. The chunk output has much
more flexibility; for example, we can produce graphics or tables from a
code chunk. Here is a plot created with **ggplot2** dynamically:

~~~~ {.r}
library(ggplot2)
qplot(hp, mpg, data = mtcars) + geom_smooth()
~~~~

![A sample plot in a dynamic report.](http://i.imgur.com/lBDza.png)

And here is a table created with the **xtable** package:

~~~~ {.r}
library(xtable)
xtable(head(mtcars[, 1:5]))
~~~~

<!-- html table generated in R 2.14.1 by xtable 1.7-0 package -->
<!-- Wed Mar 21 10:08:59 2012 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> 
mpg
</TH> <TH> 
cyl
</TH> <TH> 
disp
</TH> <TH> 
hp
</TH> <TH> 
drat
</TH>  </TR>
  <TR> <TD align="right"> 
Mazda RX4
</TD> <TD align="right"> 
21.00
</TD> <TD align="right"> 
6.00
</TD> <TD align="right"> 
160.00
</TD> <TD align="right"> 
110.00
</TD> <TD align="right"> 
3.90
</TD> </TR>
  <TR> <TD align="right"> 
Mazda RX4 Wag
</TD> <TD align="right"> 
21.00
</TD> <TD align="right"> 
6.00
</TD> <TD align="right"> 
160.00
</TD> <TD align="right"> 
110.00
</TD> <TD align="right"> 
3.90
</TD> </TR>
  <TR> <TD align="right"> 
Datsun 710
</TD> <TD align="right"> 
22.80
</TD> <TD align="right"> 
4.00
</TD> <TD align="right"> 
108.00
</TD> <TD align="right"> 
93.00
</TD> <TD align="right"> 
3.85
</TD> </TR>
  <TR> <TD align="right"> 
Hornet 4 Drive
</TD> <TD align="right"> 
21.40
</TD> <TD align="right"> 
6.00
</TD> <TD align="right"> 
258.00
</TD> <TD align="right"> 
110.00
</TD> <TD align="right"> 
3.08
</TD> </TR>
  <TR> <TD align="right"> 
Hornet Sportabout
</TD> <TD align="right"> 
18.70
</TD> <TD align="right"> 
8.00
</TD> <TD align="right"> 
360.00
</TD> <TD align="right"> 
175.00
</TD> <TD align="right"> 
3.15
</TD> </TR>
  <TR> <TD align="right"> 
Valiant
</TD> <TD align="right"> 
18.10
</TD> <TD align="right"> 
6.00
</TD> <TD align="right"> 
225.00
</TD> <TD align="right"> 
105.00
</TD> <TD align="right"> 
2.76
</TD> </TR>
   </TABLE>



TODO: Sweave. An overview of the following chapters.

Reproducible Research
=====================

What is RR?

Good practices of RR.

-   manage all source files under the same directory
-   always compile the documents in a clean R session
-   avoid environment variables for data analysis
-   attach sessionInfo() and instructions on how to compile this
    document

A First Look
============

Simple examples to get a taste of **knitr**.

Editors
=======

We need editors mainly because of LaTeX. For markdown, a simple text
editor is often enough.

An editor often can integrate many steps into one command/click.

RStudio
-------

Official support to Sweave and **knitr**.

cool features on error navigation and PDF/Rnw sync

LyX
---

unbeatable LaTeX front-end

Texmaker
--------

has simple support for Sweave; also can define custom user commands

Eclipse
-------

do not know much about it yet

WinEdt
------

proprietary software; have not used it for a few years since I came to
Ubuntu, but is configurable too

Document Formats
================

We have a few document formats, but the big picture is more or less the
same. We need to read an input, parse it, execute the code, and write
the output.

Input Syntax
------------

patterns: regular expressions to identify code blocks and other elements

-   markdown
-   LaTeX
-   HTML

Output Renderers
----------------

output hooks: how to wrap up output

Text Output
===========

fine tune text output

Inline output
-------------

scientific notation

Chunk output
------------

-   code reformatting
-   highlighting
-   echo
-   results
-   eval
-   warning/error/message
-   split
-   include
-   background
-   tables

Graphics
========

fine control of graphics output

Cross Reference
===============

chunk reference

code externalization

child/parent documents

Cache
=====

speed up compilation by skipping chunks which have already been executed
and have not changed ever since

Hooks
=====

Chunk Hooks
-----------

tasks attached on chunk options

Output Hooks
------------

how to write output

Themes
======

go beyond the default color theme

Publishing Results
==================

there is not much to do with LaTeX, but markdown can be fun

how to build a website quickly?

-   single HTML pages
-   pandoc
-   Jekyll

Applications
============

significant applications and real examples

Other Tools
===========

Sweave, pgfSweave, cacheSweave, brew, ...

dexy
W