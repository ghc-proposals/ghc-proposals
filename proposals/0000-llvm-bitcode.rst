.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

LLVM Bitcode
=============

GHC currently uses the textual intermediate representation when compiling to llvm, which chains us to a specific llvm version due to the unstable nature of the textual ir, changeing this to llvm bitcode would allow more llvm version freedom.

Motivation
----------

The llvm backend is quite high maintenance and does not allow using various llvm versions, but only a specific one. This is due to the nature of the textual ir, which changes with almost every release. The llvm project however provides a binary bitcode format, which is much more stable (e.g. the upcoming llvm 4.0 will be able to read bitcode produced as early as of llvm 3.0).

Proposed Change
---------------

This would change the way the current llvm backend works, in that it is esentially replaced by a different backend.

The proposed new backend would be structured as follows:

- `data-bitcode <https://github.com/angerman/data-bitcode>`_

  A pure haskell package to read/write the bitcode format.

- `data-bitcode-llvm <https://github.com/angerman/data-bitcode-llvm>`_

  A pure haskell package that can read/write a higher level llvm module
  representation using the data-bitcode package.

- `data-bitcode-edsl <https://github.com/angerman/data-bitcode-edsl>`_

  A pure haskell package that provides a simple interface to generating
  llvm modules, which can subsequently be turned into bitcode though the
  data-bitcode-llvm package.

- `data-bitcode-plugin <https://github.com/angerman/data-bitcode-plugin>`_

  A ghc plugin that turns cmm into bitcode.

NOTE: The current llvm bitcode backend is being developed as a ghc plugin, and needs
a slightly larger plugin interface. The relevant wip patch is in `D535 <https://phabricator.haskell.org/D535>`_

Drawbacks
---------

Tools that depend on the textual representation would need to call the llvm-dis tool to produce the textual ir from the bitcode ir, which however might not look idential to the current generated textual ir.

