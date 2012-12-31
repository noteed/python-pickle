# python-pickle

python-pickle is a Haskell library to serialize and deserialize Python objects
using the Python Pickle format.

It offers a `Value` data type to represent Python objects, and two functions,
`pickle` and `unpickle`, to serialize and deserialize such values.

# Implementation detail

In practice the Python Pickle format is a list of serialized op codes. By
deserializing those op codes then interpreting them, one can reconstruct the
original object. Interpreting the op codes is done using a simple stack-based
Pickle machine.

# Install

The development version can be installed by cloning the Git repository and
using cabal:

    > git clone git://github.com/noteed/python-pickle.git
    > cd python-pickle && cabal install

# Limitation

Only 17 of the 53 op codes reported by `pickletools` are implemented.

    > python2.7 -c 'import pickletools ; print len(pickletools.opcodes)'
    53
    > ghc -package python-pickle -e 'length Language.Python.Pickle.opcodes'
    17

