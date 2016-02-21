This program requires OCaml 3.12 (or above)

## Before compiling
# In the main directory "flag/"
$ mkdir compile
# For each type of flag "flagtype" (flagtype = "graphs", "digraph", trianglefree) you want tu use :
$ mkdir save/"flagtype"

## Commands

# Compiling in bytecode
$ make

# Compiling in native (execution 4 to 10 times faster)
make opt

# Compiling (in bytecode) and excuting the code in main.ml
$ make launch

# Compiling in native and excuting the code in main.ml
$ make optlaunch

# Running tests
$ make test

# Generating documentation in an ocamldoc style:
# ( Requires ocamldoc )
$ mkdir doc
$ make doc
$ firefox doc/index.html (for instance)

## Customize the program
# To run a particular piece of code, the simplest is to rename it as src/main.ml
