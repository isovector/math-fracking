# math-fracking

## Dedication

> I bought a new pipeline for my birthday.
>
> Justin Trudeau


## Overview

If you're writing a book in Latex like I am that has lots of math in it but
you're trying to automatically convert this book into an epub without using
pandoc because you're a masochist you're going to run into problems like I have
been so I wrote you a little job that rips math out of Latex and replaces it
with a helpful `\input` command.

I'm very tired.


## Usage

```bash
$ math-frack frack-me-baby book.tex > fracked-book.tex
frack-me-baby0.tex frack-me-baby1.tex frack-me-baby2.tex
```

You can also pass in two parameters before the file if you want your fracked
math to have a different file name than the `\input`s. Like if you wanted to run
them through `mjpage` to statically compile the math for example...

