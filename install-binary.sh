# This script setups up the project P2PRC
echo '# Add the following paths to .bashrc or .zshrc based on the configuration you have set'
echo export P2PRC=$PWD
echo export PATH=$PWD:\${PATH}
export P2PRC=${PWD}
export PATH=${PWD}:${PATH}

./p2p-rendering-computation --dc