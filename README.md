
# Jana

An interpreter for Janus, the reversible programming language.

## Build and Installation

To build Jana run

    cabal configure
    cabal build

and to install

    cabal install

The program can also be run (assuming all dependencies are installed) by cd'ing
to `src` and then running

    runhaskell Main.hs

For more info about `cabal` see http://www.haskell.org/cabal/.

For building and installation using stack, run

    stack init
    stack install

and the interpreter can then be invoked like this:

    stack exec jana

However, that installation is only local to that folder; to make Jana available
globally, edit your global config in `~/.stack/global-project/stack.yaml`
and add the directory in the `packages` array like so:

    packages:
    - /home/user/path/Jana-JanusInterp

and install it globally, outside the project directory, using

    cd
    stack install path/Jana-JanusInterp

Or, to run it directly using stack, use

    cd src/
    stack exec -- runhaskell Main.hs

## Running the symmetry-checker
To run the symmetry-checker on a procedure, on must pass the argument 
`-s=symmetricProcedure` to the `jana` executable, along with the file containing the procedure to check.
e.g. to check the procedure `reverse` in `examples/reverse.ja` run
`cabal run -- jana -s=reverse examples/reverse.ja` which gives the following output: "Procedure reverse is time symmetric."
