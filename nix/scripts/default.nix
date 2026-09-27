{ pkgs, p2prcDefault }:
pkgs.writeShellApplication {
  name = "initHaskellProject";
  runtimeInputs = with pkgs; [
    ghc
    cabal2nix
    cabal-install
    git
    p2prcDefault
  ];
  text = ''
    clear

    if [ "$#" -eq 0 ]; then
      echo "No arguments provided."
      echo "Please provide the name of your project"
      echo "nix run git+https://github:akilan1999/p2p-rendering-computation#initHaskellProject -- <NAME-PROJECT>"
      exit 1;
    fi

    echo "RUNNING script"
  '';

  /*
    PROJECT_DIR="$1"

    mkdir "$PROJECT_DIR"

    cd "$PROJECT_DIR"

    git init .
    clear

    cabal init --exe --simple

    sed -i 's/base.*$/base, p2prc/' "$PROJECT_DIR".cabal

    cabal2nix . > ./cabal.nix;

    git add .

    clear

    echo -e "run the following commands to finish nix development and production environment:\n\n"

    echo -e "cd $PROJECT_DIR"
    echo -e "nix flake init -t github:akilan1999/p2p-rendering-computation#haskell"
    echo -e "nix develop"
    echo -e "nix run"
    echo -e "\n\n"
  */
}
