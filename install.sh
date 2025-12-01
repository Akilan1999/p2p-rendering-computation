# This script setups up the project P2PRC
# Call: sh install.sh <name of binary>
echo '# Add the following paths to .bashrc or .zshrc based on the configuration you have set'
echo export P2PRC=$PWD
echo export PATH=$PWD:\${PATH}
export P2PRC=${PWD}
export PATH=${PWD}:${PATH}

# Checks if rust is passed in the argument
# for the build.
if [[ "$*" == *"rust"* ]]; then
  cd rustlibrary/p2prc_rust_module
  cargo build --release
  # Copy loaded .a (https://en.wikipedia.org/wiki/Library_%28computing%29#Static_libraries) file
  cp target/release/libp2prc_loaded.a ../../
  # go back to the project root directory
  cd ../../
  # Generate golang code to bind with the .a file
  rust2go-cli --src rustlibrary/p2prctest/src/user.rs --dst rustlibrary/gen.go --without-main
  # build with the "rust" tag to allow the rust module to link with the go program
  go build -tags rust -o ${1}
else
  # Expects an argument of the name of the binary
  go build -o ${1}
fi

# sets up default configuration
./${1} --dc