let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz";
    sha256 = "sha256:1lr1h35prqkd1mkmzriwlpvxcb34kmhc9dnr48gkm8hh089hifmx";
  };

  np = import nixpkgs {};
in np.mkShell { buildInputs = [ np.python312Packages.sphinx
                                np.python312Packages.myst-parser
                              ];
              }
