# python-pickle

python-pickle is a Haskell library to serialize and deserialize Python objects
using the Python Pickle format.

It offers a `Value` data type to represent Python objects, and two functions,
`pickle` and `unpickle`, to serialize and deserialize such values.

# Implementation detail

In practice the Python Pickle format is a list of serialized opcodes. By
deserializing those opcodes then interpreting them, one can reconstruct the
original object. Interpreting the opcodes is done using a simple stack-based
Pickle machine.

If you want to learn about the Pickle format, the standard Python `pickletools`
library source is a good place to start.

# Install

Installing from Hackage with Cabal is straightforward:

    > cabal update ; cabal install python-pickle

The development version can be installed by cloning the Git repository and
using Cabal:

    > git clone git://github.com/noteed/python-pickle.git
    > cd python-pickle && cabal install

# Command-line tool

A `pickle` executable is provided. It can be used to inspect a pickle file
(i.e. a file containing a pickled Python object).

# Limitation

Not all opcodes are implemented.

The PROTO opcode is not represented.

    > python2.7 -c 'import pickletools ; print len(pickletools.opcodes)'
    53
    > ghc -package python-pickle -e 'length Language.Python.Pickle.opcodes'
    52

