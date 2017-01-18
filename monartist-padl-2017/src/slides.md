% mon-artist: A Declarative DSL for Custom Rendering of ASCII Art 
% Felix Klock (`@pnkfelix`), Mozilla
% [http://bit.ly/2jmMJ6o](http://bit.ly/2jmMJ6o) PADL 2017, January 16, Paris

# "Rendering of ASCII Art" ?  {.center}

# What? {.center}

## Demo Early, Demo Often

. . .

```
.---.
|   |  Holy
'-+-' Rounded Rectangle
  |                       Batman
  |   .----.  +-------+  .------.
  |   |    |  |       |  |      |
  '-->|    +->+       |->+      |
      |    |  |       |  |      |
      '----'  +-------+  '------'
 (Note also when lines do and do not meet edges)
```


```art
.---.
|   |  Holy
'-+-' Rounded Rectangle
  |                       Batman
  |   .----.  +-------+  .------.
  |   |    |  |       |  |      |
  '-->|    +->+       |->+      |
      |    |  |       |  |      |
      '----'  +-------+  '------'
 (Note also when lines do and do not meet edges)
```

## Why? {.center}

. . .

I want to see more diagrams in program documentation

. . .

(also want Literate Programming to succeed; that's a different talk.)

Goal: make it easy for documentation to include nice diagrams

## Programmers like Markdown

. . .

(Not sure why, but here are some ideas)

* Lightweight Markup

* Text-based; therefore integrates with dev tools (`diff`, `git blame`)

* Source matches pre-existing conventions from text-based tools

* What You See Roughly Approximates What You Get (WYSRAWYG)

## {.center}

Some diagramming tools seek a WYSIWYG interface

 * But its often onerous to use a separate tool

 * (Caveat: Maybe modern IDE's make this easy...)

. . .

ASCII Art provides a *WYSRAWYG* interface

. . .

Just need to compile it into a suitable output (I chose SVG as target)

# Sold on ASCII Art Rendering? {.center}

# Quick Review of SVG {.center}

## SVG (subset)

SVG: large feature set; tiny subset suffices for our tool.

. . .

`<svg>` XML with `<path>` or `<text>` elements; `<path>` may have `stroke`, `fill`, `d` attributes

. . .

`d` attribute: string describes path's shape
  via a compact "turtle language" (hands up if you ever used LOGO?)

. . .

--- ------------------ ---------------------------------------------------------------------------------
`M` Move               `M x,y` starts a subpath at (x,y)
`L` Line to            `L x,y` draws straight line to (x,y)
`Q` Quadratic bezier   `Q x1,y1 x2,y2` goes towards (x1,y1), but curves then ends at x2,y2
`C` Cubic bezier       `C x1,y1 x2,y2 x3,y3` like above, with separate control points for start and end
`Z` close current path `Z` draws line to first coordinate in subpath (established by Move)
--- ------------------ ---------------------------------------------------------------------------------

## SVG subset example

`<path d="M 0,8 L 10,8 Q 15,8 20,16 L 0,32"></path>`

. . .

<table>
<tr>
<td><code>M 0,8 L 10,8</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 0,8 L 10,8" stroke-width="2" stroke="green"></path></svg></td>
</tr>

<tr>
<td><code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; M 10,8 L 15,8 M 10,8 L 10,16</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 10,8 L 15,8 M 10,8 L 10,16" stroke-width="2" stroke="blue"></path></svg></td>
</tr>

<tr>
<td><code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; M 10,8 Q 15,8 10,16</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 10,8 Q 15,8 10,16" stroke-width="2" stroke="green"></path></svg></td>
</tr>

<tr>
<td><code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; M 10,16 L 10,32</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 10,16 L 10,32" stroke-width="2" stroke="blue"></path></svg></td>
</tr>

<tr>
<td><code>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; M 10,16 L &nbsp;0,32</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 10,16 L  0,32" stroke-width="2" stroke="green"></path></svg></td>
</tr>

<tr>
<td><code>M 0,8 L 10,8 Q 15,8 10,16 L &nbsp;0,32</code></td>
<td style="width:400px;overflow:visible;position:relative;">
<svg style="position:absolute;top:0px">
<path fill="none" d="M 0,8 L 10,8 Q 15,8 10,16 L 0,32" stroke-width="2" stroke="green"></path></svg></td>
</tr>

</table>

 . . .

There won't be a quiz at the end, but just ... keep above data string
and its rendering in mind for a bit.

## That's enough SVG for us to be dangerous {.center}

# Rendering Tool: Goals and Approach {.center}

## Goal: Correspondence

Output should not wildly deviate from input.

```
.------.     +----+   +----
|      |     |    |   |
|      |     |    +-+ |
|      |     |    | | |
|      |     |    | + +
|      |     +----+  \ \
|      |              \ \ .
|      |     +----+    \ / \
|      |     |    |     /   \
'------'     +----+    '-----+
```

```art
.------.     +----+   +----
|      |     |    |   |
|      |     |    +-+ |
|      |     |    | | |
|      |     |    | + +
|      |     +----+  \ \
|      |              \ \ .
|      |     +----+    \ / \
|      |     |    |     /   \
'------'     +----+    '-----+
```

Input text forms a grid of characters;
output SVG renders a grid of rectangular cells at corresponding coordinates.

## Goal: Expose underlying SVG

Some SVG attributes can be inferred from input,
but many have no natural ASCII art representation.

Solution: Allow attribute injection via markup.

. . .

```
+-----.     +-----.     +-----.
|     |[a]  |[b]  |     |     |
+-- --'     +-   -'     +-----'
                         [c]
[a]: stroke="blue" stroke-width="20"
[b]: stroke="red" stroke-width="4" stroke-dasharray="10,1,10,5"
[c]: fill="yellow"
```

```art
+-----.     +-----.     +-----.
|     |[a]  |[b]  |     |     |
+-- --'     +-   -'     +-----'
                         [c]
[a]: stroke="blue" stroke-width="20"
[b]: stroke="red" stroke-width="4" stroke-dasharray="10,1,10,5"
[c]: fill="yellow"
```

## Goal: Go Beyond ASCII

### Its 2017

. . .

We should be able to support Unicode codepoints in our pictures.

. . .

but...

... I am not going to write the rendering rules for over a million codepoints...

## Goal: Customizable Rules

Do not hard code rendering logic; allow user to add or override rules.

* (This is where the "domain specific language" comes in.)

. . .

E.g. different communities have different conventions for rounded corners:

```
                              .---.               /---\
                        (a2s) |   |               |   | (ditaa)
                              '---'               \---/
```

With appropriate rules, each of above render to:

```art
.---.
|   |
'---'
```

. . .

(*Ta-dah*: This also ~~solves~~ delegates "millions of codepoints" problem)

## Rules drive both path search and rendering

Our tool will only find paths (aka sequences of adjacent characters)
that are described via the rules.

(Hard-coding path search into tool and only exposing rendering in
rules would be another route.)

. . .

Confession: It was my experience *writing* a hard-coded path search
(and struggling to debug its special case logic) that led to revelation.

Driving the search via the DSL-encoded rules removes ability to add
special cases. Core code becomes simpler!


## Goal: Compositional Rules

Want to ease anticipation of:

 * how rules effect rendering, and

 * how new text changes diagram

# Mental model {.center}

## Naive model {.center}

Each rule dictates how to render an individual character

Tool searches for path as series of adjacent characters

Each character contributes to the `d` attribute of the constructed path

## Dive in: Rendering Template

Each rule specifies how to render a character in the context of its
position ("cell") in the grid.

Each defines a rendering string, with placeholders where coordinates
need to be substituted in.

Here is a cell, with eight compass positions and a center point.

```art
+-------------+
|NW    N    NE|
|             |
|             |
|             |
|             |
|W     C     E|
|             |
|             |
|             |
|             |
|SW    S    SE|
+-------------+
```

So we have placeholders `{NW}`, `{N}`, `{C}`, et cetera.

## Example

To draw a `┐`, we could draw a line that goes from `W` to `C` and then to `S`

---- - ---------------------------------
 `┐` : `"M {W} L {C} L {S}"`
 `┴` : `"M {W} L {C} L {N} L {C} L {E}"`
---- - ---------------------------------

```
┐
┴
```

```art
┐
┴
```

. . .


```
d="M 0,8 L 5,8 L 5,16 M 0,24 L 5,24 L 5,16 L 5,24 L 10,24"
      |     |     |      |      |      |      |       |
     W(┐)  C(┐)  S(┐)   W(┴)   C(┴)   N(┴)   C(┴)   E(┴)
```

The main interesting thing here is that `S(┐)` = `N(┴)`, because of
their relative positions on this path.

# Lets not be naive {.center}

## Problem

Example of box drawing rules like  `┐` : `"M {W} L {C} L {S}"` is naive

(Which is okay, because this model is naive)

. . .

If all we do is render each character as its own subpath, we are just
recreating a text font (poorly).

. . .

In practice we want combinations of characters to work together.

## Dive in: How to render a `.`? { data-transition="fade-out" }

```
-.
/
```

```art
-.
/
```

----- --------------- -------------- ---------
          hyphen         period      fwd slash
`d =` `"M 0,8 L 10,8` `Q 15,8 10,16` `L 0,32"`
      ` M {W} L {E} ` `Q {C}  {SW} ` `L {SW} `
----- --------------- -------------- ---------

. . .

(does that `d` string look familiar? Or the shape above it?)

## Dive in: How to render a `.`? { data-transition="fade" }

```
-.
/
```

```art
-.
/
```

----- --------------- -------------- ---------
          hyphen         period      fwd slash
`d =` `"M 0,8 L 10,8` `Q 15,8 10,16` `L 0,32"`
      ` M {W} L {E} ` `Q {C}  {SW} ` `L {SW} `
----- --------------- -------------- ---------

*Continues* path from `-` through `.` (via a `Q`) and then slash back via `/`.


## Dive in: How to render a `.`? { data-transition="fade-in" }

```
-.
/
```

```art
-.
/
```

----- --------------- -------------- ---------
          hyphen         period      fwd slash
`d =` `"M 0,8 L 10,8` `Q 15,8 10,16` `L 0,32"`
      ` M {W} L {E} ` `Q {C}  {SW} ` `L {SW} `
----- --------------- -------------- ---------


```
-.
  \
```

```art
-.
  \
```

----- --------------- -------------- ----------
          hyphen         period      back slash
`d =` `"M 0,8 L 10,8` `Q 15,8 20,16` `L 30,32"`
      ` M {W} L {E} ` `Q {C}  {SE} ` `L {SE} `
----- --------------- -------------- ----------

. . .

Template for `.` depends on neighboring characters on path!

## Mental model

~~Naive mental model: each rule dictates how a single character is rendered~~

Mental model: each rule dictates how a single character is rendered
in the *context of immediate neighbors* on a path

# The Language {.center}

## Rules Encoded via Oriented Tuples

--------------- --- --------- ------------- ------- ---------------- ------- -------------
     $⟨rule⟩$   ::= $⟨match⟩$ `draw`                $⟨rendering⟩$
    $⟨match⟩$   ::= `loop`    $⟨prev char⟩$ $⟨dir⟩$ $⟨curr char⟩$    $⟨dir⟩$ $⟨next char⟩$
                  | `step`    $⟨prev char⟩$ $⟨dir⟩$ $⟨curr char⟩$    $⟨dir⟩$ $⟨next char⟩$
                  | `start`                         $⟨curr char⟩$    $⟨dir⟩$ $⟨next char⟩$
                  | `end`     $⟨prev char⟩$ $⟨dir⟩$ $⟨curr char⟩$
--------------- --- --------- ------------- ------- ---------------- ------- -------------

$⟨dir⟩$ is a set of compass directions.

$⟨rendering⟩$ is string with placeholders, where a placeholder is either:

 * compass coordinates `{N}`, `{NE}`, et cetera (as previously described),
   denoting points on the edge of the cell,
 * the center of the cell `{C}`
 * the point nearest the incoming neighbor `{I}` or outgoing `{O}`
 * the reflection of the the previous points `{RI}` and `{RO}`

## Why four cases for $⟨match⟩$? {.center}

$⟨match⟩$ has both `loop` and `start` forms that can initiate a path.

Purpose: The tool actually distinguishes closed and unclosed paths.

## Unclosed paths

Unclosed paths `start` with a character $⟨curr char⟩$ and a neighbor
$⟨next char⟩$, follow a sequence of `step`s (each of which carries a
prev/curr/next triple), and terminate at an `end` with a $⟨prev char⟩$
and $⟨curr char⟩$.

```
--+
   \
```

```art
--+
   \
```

---------------- -------- --------- ---------
  start `-`      step `-` step `+`   end `\`
`M 0,8 L 10,8`   `L 20,8` `L 25,8`  `L 40,32`
`M {RO} L {O} `  `L {O} ` `L {C}  ` `L {SE} `
---------------- -------- --------- ---------

Relevant Rules:

```
start              "-" (E,W) "-+" draw "M {RO} L {O}"
step  "-"   (E,W)  "-" (E,W) ANY  draw "L {O}"
step  "-"   (E,W)  "+" ANY   ANY  draw "L {C}"
 end  ANY    SE    "\"            draw "L {SE}"
```

## Closed paths

Closed paths start with a `loop`, followed by a sequence of `step`s
required to end with a step that includes the opening $⟨prev char⟩$
for the `loop`; a `Z` is automatically inserted afterward to close the
path.

```
+---+
 \ /
  +
```

```art
+---+
 \ /
  +
```

--------- -------- -------- -------- ---------
  `M 5,8` `L 20,8` `L 30,8` `L 40,8` `L 45,8` 
  `M {C}` `L {O}`  `L {O}`  `L {O}`  `L {C}`  
loop `+`  step `-` step `-` step `-` step `+` 
--------- -------- -------- -------- ---------



--- --------- --------- --------- ---------
... `L 30,32` `L 25,40` `L 10,16` `Z`
... `L {SW}`  `L {C}`   `L {NW}`  `Z`
...  step `/`  step `+`  step `\`  (implied)
--- --------- --------- --------- ---------


## {.center}

(Paper has more detail on the path search process itself.)

# Is that all? {.center}

## Is this really a language?

The core rule format described here is essentially a table of data.

But for practical use, it needs extensions beyond that shown so far.

* The same $⟨rendering⟩$ can often be reused for different rules.  The
  paper describes `maybe`, a way to collapse a `step` and an `end`
  into one rule.

* A $⟨rendering⟩$ can also inject other attributes onto the path;
  a local character can cause a global effect.
  Allows e.g. `-=---=---=-` to show up as a dashed line.

* Many rules often need to refer to the same set of characters.
  The DRY principle means that we should be able to identify such
  sets via a name: Variable Binding!

# Related work {.center}

## ASCII Art Renderers

Examples include `ditaa` and `a2s`. The latter heavily influenced this
work.

To my knowledge no other ASCII Art renderers allow the user to
customize the path-search and rendering process.

## Picture Description Languages

Metapost, Tikz/PGF, Graphviz, mermaid.

None of these are WYSRAWYG

## Object Modelling Languages

Message Sequence Charts, UML, Alloy

These often carry semantics, which is useful for their target domains,
but inherently limits their flexibility. My goal is to enable drawing
of semi-arbitrary figures.

# Conclusion {.center}

## What was this

User-customizable ASCII Art Rendering

```
            .---------.
\   /       | Hooray! |
 \o/        '-+-------'
  +          /
 / \    \o/
/   \    +
        / \
```

. . .

```art
            .---------.
\   /       | Hooray! |
 \o/        '-+-------'
  +          /
 / \    \o/
/   \    +
        / \
```

(hmm there's ... some glitches in this rule set)

. . .

Good idea:
Separate content from rendering rules (& from core interpreter).

Analogy: Its like HTML vs CSS (& vs the browser)

. . .

A fun project I did while I was on paternity leave.

## Where is it?

Where can you get it?

Once frontend is done, will put out a 1.0 release (fatherhood: distracting)

For now you can find it at:

[https://github.com/pnkfelix/mon-artist/][monartist repo]

[monartist repo]: https://github.com/pnkfelix/mon-artist/

. . .

Thanks for listening!
