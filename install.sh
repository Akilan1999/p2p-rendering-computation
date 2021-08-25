# This script setups up the project P2PRC
# Call: sh install.sh <name of binary>
echo '# Add the following paths to .bashrc or .zshrc based on the configuration you have set'
echo export P2PRC=$PWD
echo export PATH=$PWD:\${PATH}
export P2PRC=${PWD}
export PATH=${PWD}:${PATH}

# Expects an argument of the name of the binary
go build -o ${1}
./p2prc --dc