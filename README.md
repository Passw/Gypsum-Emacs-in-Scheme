# Project Gypsum

#### A clone of Emacs and Emacs Lisp written in R7RS Scheme

## Project Goals

Although this project is still incomplete and experimental, the goals of this project are to construct a Scheme app platform similar to Emacs, not just in the UI/UX, but also to be backward compatible (to the greatest degree possible) with GNU Emacs by implementing an Emacs Lisp interpreter as well.

- written in R7RS Scheme, should run on any compliant Scheme implementation.
- should parameterize platform-specific APIs, keep model and control logic separate.
- able to run your `init.el`, run Emacs software pulled from ELPA.
- use Emacs Regression Tests (ERT) from GNU Emacs to ensure compatibility.

### Sub-goals

- contribute patches upstream to the Guile Scheme Emacs Lisp compiler
- provide a cross-platform GUI library like [Racket/GUI](https://docs.racket-lang.org/gui/ ) or [McCLIM](https://mcclim.common-lisp.dev/ )
- be able to develop Gypsum from within it's own editor, create pull requests in Git.

## How to build

As of right now, this project only runs on Guile Scheme, although
certain libraries (`lens.sld`, `match.sld`, `pretty.sld`,
`keymap.sld`) can build and run on other Schemes. The only GUI
available right now is for
[Guile-GI](https://github.com/spk121/guile-gi ), but the Editor is
designed specifically to be able to run on other Scheme platforms with
other GUI toolkits. All platform specific calls are parameterized.

### Scheme Requirements

- [Guile 3](https://www.gnu.org/software/guile )

- [Guile-GI](https://github.com/spk121/guile-gi ) must be built and
  installed in a directory path that is listed in the `%load-path`.

### C Requirements for Guile-GI

- `libglib`
- `libgio`
- `libgdk`
- `libgtk3`

[Guile 3](https://www.gnu.org/software/guile ) usually installs from
source using `autotools` on any Linux or BSD operating system provided
the above developer dependency packages are installed. Installing with
`autotools` installs all Guile modules in the site-local package
directory. If you install it this way, the modules are always
available to your Guile runtime without needing to set the Guile
`%load-path`.

### (Optional) use Guix

The `manifest_guile-gi.scm` file is provided to install development
dependencies into your local Guix installation. Use it in one of two
ways.

 1. The first way is to use the `guix shell` command, which keeps
    package dependencies locally and temporarily (until you run the
    Guix garbage collector). The full command is this:

    ```sh
      guix shell -m ./manifest_guile-gi.scm -- guile --r7rs
    ```

 2. The second way is to installing packages in a local profile
    directory.  Python programmers may find this workflow more
    familiar, as it is similar to using a Python `venv` or "virtual
    environment", except the package directory is `./.guix-profile`
    rather than `.venv`.

    ```sh
      guix package -i -p ./.guix-profile -m ./manfiest_guile-gi.scm
      guix shell -p ./.guix-profile -- guile --r7rs
    ```

    Now the `guix shell` command can use the profile directory, and
    the package dependencies installed into the profile directory will
    survive garbage collections.

### Launch the Guile REPL using `./guile.sh`

The `guile.sh` script sets environment variables and command line
parameters for the Guile runtime, it is usually easier to simply
execute this script to start the REPL.

Emacs users can set the directory-local variable `geiser-guile-binary`
to `"./guile.sh"`. The `.dir-locals.el` file in this repository does
this for you if you choose to use it.

## How to run

Once you have a Guile Scheme REPL running with the Guile-GI
dependencies available in the `%load-path`, simply load the main
program into the REPL:

```scheme
(load "./main-guile.scm")
```

This will rebuild and launch the executable. If you are using the
`./guile.sh` script to start the REPL, note that the
`--fresh-auto-compile` flag is set, and so recompilation will occur
every time `main-guile.scm` is loaded. If you are not hacking the
Gypsum source code, feel free to delete this flag from the `guile.sh`
script file so that `load` only builds the application once, and
launches the application more quickly.

## How to hack

Start a Scheme REPL as described above, but before running the main
program, run the test suite:

```scheme
(load "./run-tests.scm")
```

Whenever you make a change to the source code of a Scheme library, be
sure to run the tests for that library. The tests in `./run-tests.scm`
require your Scheme implementation to provide
[SRFI-64](https://srfi.schemers.org/srfi-64/srfi-64.html ), without
this language extension (sorry, MIT Scheme users) you will have to run
each test case by hand by copy-pasting each test form into your REPL.

Keep in mind that this project is still experimental, it is possible
some of the tests may not pass. As long as you create a pull request
with equal or fewer passing tests, your request is more likely to be
pulled into the main branch.

That said, `./gypsum/lens.scm`, `./gypsum/match.scm`, and
`./gypsum/keymap.scm` should always pass all tests, as these libraries
are most essential to the rest of the application.

### Lenses

R7RS Scheme does not standardize any Meta-Object Protocol (MOP)
implementation, not even in the R7RS "Large" standard.

Gypsum has been written such that there no need for any MOP
implementation. Rather, a "function lenses" implementation written in
pure R7RS-Small compliant code is provided as a library. Functional
lenses are inspired by Haskell, and are a way of defining getter and
setter functions that can be composed together.

Conventionally, lens definitions are prefixed with the `=>`
symbol. They may also be suffixed with `*!` or `!` whether a lens is
canonical and/or whether a lens mutates the data structure when
updating it. For example, the `(gypsum editor)` library exports
symbols such as `=>editor-buffer-table*!` or `=>buffer-local-keymap*!`.

Since it is usually much easier to use lenses rather than getters and
setters, many Gypsum libraries export their own lenses for working
with the record types provided within.

Lenses are useful enough to be separated into it's own separate source
code package. See the documentation for Functional Lenses in its own
[source code repository](https://codeberg.org/ramin_hal9001/gypsum-lens ).

### Monads

There are two libraries in this source code that use monads to perform
computation:

- `(gypsum pretty)` a pretty printer that is smaller, and easier to
  port to other Scheme implementations, than
  [SRFI-166](https://srfi.schemers.org/srfi-166/srfi-166.html ),
  although it has far fewer useful features.

- `(gypsum match)` is a monadic pattern matcher that is smaller, and
  easier to port to other Scheme implementations, than
  [SRFI-241](https://srfi.schemers.org/srfi-241/srfi-241.html ).

How monads are defined and use in the Gypsum library is explained in
more detail in the
[Monads](https://codeberg.org/ramin_hal9001/gypsum/wiki/Monads )
chapter of this wiki. In brief, our notion of "monad" is a data
structure with thunks (lazy procedures) connected to it, and an
associated "monadic evaluator" that traverses the data structure
applying these thunks to perform computational work. Monadic
"combinators" procedures (simply "monads" for short) are any code that
declares the actual structure of these monadic data structures. There
are a few primitive combinators for each monad which are used to
declare the larger combinators that do useful work in this
application.

The `(gypsum pretty)` moandic evaluator is `pretty`, the `(gypsum
match)` monadic evaluator is `run-matcher`. Read the comments in the
source code for these procedures for more details on how they should
be used.


### Keep platform-specific code separate

Any platform-specific code, i.e. code not spcified by the R7RS
standard, **must** be parameterized. Parameters are defined in source
files with the `*-impl.sld` suffix, the `./gypsum/editor-impl.scm`
library is a good example of this.

Platform independent code will import these `*-impl.sld` libraries and
call procedures parameterized by these parameters. The main program
that launches Gypsum is platform specific as well, (for example,
`./main-guile.scm`). This main program must import the
platform-specific libraries, parameterize all parameters in every
relevant `*-impl.sld` library, and then initialize the editor.  The
`./gypsum/backend/guile-gi/gtk3-init.scm` program provides an example
of this in the `with-gtk3-backend` procedure.

Once all parameters are set, the `new-editor` procedure from the
`(gypsum editor)` library can be called. Reading the source code for
`./gypsum/editor.scm` you can see examples of how platform-dependent
code is called via parameters.

One thing to note is that parameter symbols always have an asterisk
`*` suffix, for example `self-insert-command*`. When these parameters
are imported by the platform-independent code, usually it is imported
like so:

```scheme
(import
  (prefix (gypsum editor-impl) *impl/)
  ...)
```

This tells the `import` statement to import and rename all symbols
such that they are prefixed with `*impl/`. So when actually applying
arguments to a procedure in a parameter like `self-insert-command*`,
you must call it like so:

```scheme
((*impl/self-insert-command*) window string)
```

### Platform-independent record types

Record types that contain platform specific information are usually
exported as lenses, and are usually named `=>*-view`, for example
`=>buffer-view` and `=>window-view`. These are fields that contain
references to platform-specific data needed by the platform in order
to render the view of a "buffer" or "window."

## Other resources

- [Presentation of this project at EmacsConf 2024](https://emacsconf.org/2024/talks/gypsum/ )

## Contributors welcome!

This is a large and ambitious project, but there seems to be a lot of
interest in both the Emacs and Scheme communities for an Emacs clone
written in Scheme. Our job is to coordinate everyone's efforts, and to
make it as easy as possible for anyone to contribute. Please feel free
to get in touch with us, we want to help you contribute code.
