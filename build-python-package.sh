#!/usr/bin/bash

# Create export directory for python
mkdir Bindings/python/export

# Creating SharedObjects directory for python
mkdir Bindings/python/export/SharedObjects

# Builds p2prc.h and p2prc.so file
# as apart of Go FFI to interacted
# with later on.
sh build-bindings.sh

# Copy the shared object files as well to ensure 
# that python can interact with the latest snapshot
# of P2PRC 
cp Bindings/p2prc.h Bindings/python/export/SharedObjects/
cp Bindings/p2prc.so Bindings/python/export/SharedObjects/

# Copy python library and tests as export as well 
cp Bindings/python/* Bindings/python/export/

echo "Output is in the Directory Bindings/python/export/"

# Architectures for Linux
#archs=(amd64 arm64)
#
#for arch in ${archs[@]}
#do
#  mkdir Bindings/python/export/SharedObjects/linux-${arch}
#  cd Bindings/
#	env GOOS=linux GOARCH=${arch} go build -buildmode=c-shared -o python/export/SharedObjects/linux-${arch}/p2prc.so
#	echo "GOOS=linux GOARCH=${arch} go build -buildmode=c-shared -o python/export/SharedObjects/linux-${arch}/p2prc.so"
#	cd ..
#done
