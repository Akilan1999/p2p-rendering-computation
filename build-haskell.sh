# This bash script consists of the manaul build script of haskell 
go build .

# Unset p2prc path
unset p2prc 

# Enter the haskell project 
cd Bindings/haskell
# Remove exsisting export build
rm -rf exports
# Create exports directory
mkdir exports
# Copy current haskell project into the folder
cp -rf . exports/
# Copy p2prc binary inside haskell folder
cp ../../p2p-rendering-computation exports/

cd exports

# Set p2prc variable
echo export P2PRC=$PWD

export P2PRC=$PWD

./p2p-rendering-computation --dc

echo "Run the following commands in the directory: ${PWD}"
echo "To build: cabal build"
echo "To run sample program: cabal run"
