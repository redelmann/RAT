RAT
===

_RAT_ is a toy programming language that has closures, lists, tuples, integers, floats and booleans.

_RAT_ comes with a compiler in Haskell, called _ratc_, and a virtual machine in C, called _ratvm_, that implements carbage collection. Tail call reduction is also performed.

How to build
============

Building the compiler
=====================

Once you have installed the Haskell platform (http://www.haskell.org/platform/), you can simply call from the root of the repository:

	mkdir build
	ghc Main.hs -o build/ratc -hidir build -odir build

Once done, the compiler, _ratc_, will be available in the _build_ directory. Note that if some package are missing, the package manager of the Haskell platform, _cabal_, can be used to install them, as follows:

	cabal install <missing package>

Building the VM
===============

Again, from the root of the repository:

	gcc ratvm.c -o build/ratvm

This will make the virtual machine, _ratvm_, available in the _build_ directory.

Running programs
================

Once the compiler and the virtual machine are here, you can test programs!

To do so, simply type:

	./build/ratc <path/to/the/program>.rat
	./build/ratvm <path/to/the/program>.ratc

Example programs
================

Some examples can be found in the _examples_ directory.

To build them and test them, you can do, for instance:

	./build/ratc examples/average.rat
	./build/ratvm examples/average.ratc

This particular example will prompt the user for floats until he gives a negative one, returning the average of the numbers.
	



