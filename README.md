# Diagrams

Diagrams is a library (written by Brent Yorgey of CIS194 fame) for producing
diagrams in a declarative way. i.e. You write Haskell code to specify how to
draw a diagram, and when you compile it, you get a program that will produce
that diagram (with various command line options built in).

## Installation

Installing it is easy. On Ubuntu 14.10:

```bash
$ cabal install diagrams --constraint "arithmoi -llvm"
```

(Ubuntu's current LLVM libraries are ahead of cabal's LLVM offerings, so we turn
off LLVM support in arithmoi. Don't ask me why arithmoi uses LLVM in the first
place...)

And you might also like to add:

```bash
$ cabal install alex
$ cabal install happy
$ cabal install gtk2hs-buildtools
$ cabal install diagrams-cairo
```

(alex, happy, and gtk2hs-buildtools are prerequisites for diagrams-cairo)

## Basic shapes

The most basic diagrams are shapes. Circles ellipses, arcs, regular (and
irregular) polygons, squares and rectangles. The code is as trivial as you'd expect.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

diag :: Diagram B R2
diag = circle 1

main :: IO ()
main = mainWith diag
```

This produces a circle of radius 1. The `import` lines pull in the Diagrams
top-level import ("Prelude") and the backend for producing SVG output.

`Diagram B R2` means a diagram where `B` is the backend (each
`Diagrams.Backend.*` provides `B`, so it's an error to `import` more than one
backend) and `R2` is the vector space of the diagram (most commonly, as here,
we're producing 2 dimensional diagrams, but 3 dimensions is also possible).

## Backends

Diagrams supports output to several backends. The default is to produce an SVG
file. Postscript is also supported. The Cairo backend supports PNG output.

## Running

Diagrams gives you a couple of handy `main` functions that give your executable
some useful built in command-line arguments.

The most basic arguments specify the output destination and size.

```bash
$ ghc Main.hs
$ ./Main -o foo.svg -w 1024
```

I often develop several diagrams in one executable, for which there is a
`multiMain` function that uses a `-s` parameter on the command line to select a
named diagram to output.

There is another really useful argument `-l` which makes the program watch its
own source file, and when it changes, recompile and re-run with the same
arguments.

This means you can work in real time, with your diagram in a viewer that
auto-updates, just hacking away in your editor. Whenever you save, your program
will automatically recompile and re-run and the diagram in the viewer will
update. Very handy!

## Basics of Diagrams

A couple of basic ideas that apply to every diagram are the *local origin* and
the *envelope*.

The local origin is a point by default in the centre of a diagram (for example,
in the centre of a `circle` or `square`).

The envelope is a generalization of a bounding box - a function that embodies
the notion of the edge of the diagram (for the purposes of putting it next to
other diagrams).

There are functions to move the local origin and change the envelope of a
diagram.

## Attributes

Plain shapes obviously get boring fast, so as you'd expect, there are plenty of
ways to change them up. Fill colours and gradients; line stroke styles, widths
and colours are some basic things you can do.

Diagrams makes extensive use of `(#)` - simply a flip of normal application -
for a nice declarative way to apply functions in a postfix way.

```haskell
(#) :: a -> (a -> b) -> b
(#) = flip ($)
```

The result is things like:

```haskell
-- circle, radius 1, filled (fc = fill colour) red, stroked (lc = line colour) yellow
circle 1 # fc red
         # lc yellow
```

It is convenient to write this way: first say what a diagram is, then what it's
like. `(#)` has a high precedence to make combining diagrams (see later) easy.

## Transformations

Diagrams supports affine transformations of course. Scaling (both uniformly and
on X/Y axes separately), reflections, rotations and translations.

```haskell
square 1 # scaleX 0.5
         # rotateBy (1/8)
```

`rotateBy` is a convenient function for rotating by fractions of a circle.
Rotation by angles is also possible with `rotate`. As usual, a positive rotation
is anticlockwise.

## Combining Diagrams

Of course, what makes Diagrams really powerful is the ability to compose
diagrams. There are 3 basic ways to compose diagrams:

```haskell
square 1 `atop` circle 1
square 1 ||| circle 1
square 1 === circle 1
```

The composition of two diagrams is itself a diagram, naturally, and the
arbitrary composability we expect from a functional library follows.

Here, the `|||` and `===` operators are special cases of the general `beside`
operator. `Diagram` is also an instance of `Monoid` with `atop` as its operation, so
`atop` is equivalent to `mappend` or `<>`. `mempty` is of course an empty
diagram with zero extent.

## Text

```haskell
diag = text "Hello!" # font "inconsolata"
```

The important thing to realize about text is that it has *zero size*. (This is
because text extents are hard to compute before actual output: there is a
possibility that the cairo backend could do this, but I haven't been able to get
it to work.)

This can be annoying: trying to align it in various ways may fail (because
diagram alignment is typically relative to local coordinate space and when your
size is zero, any multiple of that is zero).

## Arrows

In diagrams in general (but not in Diagrams!) arrows are surprisingly hard to
make look right. Diagrams can draw arrows really well and gives a great deal of
flexibility over their appearance. Arrows are the first place I met the use of
lenses to conveniently specify options.

A particularly convenient way to use arrows is in combination with naming
diagrams, to connect two subdiagrams.

```haskell
arrowOpts = (with & gaps .~ small
                  & headLength .~ small)

diag1 = circle 1 # named "c"
diag2 = square 1 # named "s"

diag = (diag1 ||| strutX 2 ||| diag2)
       # connectOutside' arrowOpts "c" "s"
```

This code also illustrates a consistent library pattern whereby `f` does
something with default parameters and `f'` allows parameters to be specified.

`strutX` is a "spacer" - a diagram with zero Y extent and the specified X
extent. You can guess that there's also a `strutY`.

## Other things

Trails and paths are a fundamental part of diagrams; funnily enough my use cases
haven't really touched on them a lot yet.

Queries allow functionality like "is a point inside a diagram?"

I really need to look into scale-invariance.

3D diagram support is under construction (generally the functions are not
exported in the prelude, and backends are in progress).

There is some rudimentary support for animated diagrams; animated GIFs are
possible with the Cairo backend. I haven't tried this yet.

Diagrams really shines for visualization of data structures. Really useful for
this are two functions, `hcat` and `vcat` which join together lists of diagrams
in horizontal and vertical fashion. This lets you eg. write a function to
visualize one element of an array, then map it over a list and hcat the results,
for a visualization of the whole array. Cool.


## Things I've used Diagrams for

I now use Diagrams to create graphics for all my "baked" (ie. PDF, not live
coding/demos) presentations. It really shines for tech talks where I often need
to visualize data structures; it's very handy to have my diagrams in source code
rather than binary form, versioned properly and easy to change and make
consistent.

I use emacs' org-mode beamer support for PDF output via LaTeX. Producing
diagrams in PostScript and converting to PDF for inclusion gives me nice vector
graphics and text independent of output resolution, so no scaling artifacts.

## Official docs

http://projects.haskell.org/diagrams/

## Brent Yorgey on Diagrams

Recorded at the ny-haskell user group meetup.

http://vimeo.com/84104226
http://vimeo.com/84249042
