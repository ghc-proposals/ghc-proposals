let
  np = import <nixpkgs> {};
in np.mkShell { buildInputs = [ np.python312Packages.sphinx
                                np.python312Packages.myst-parser
                              ];
              }
