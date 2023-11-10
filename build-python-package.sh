#!/usr/bin/bash

# Create export directory for python
mkdir Bindings/python/export

# Creating SharedObjects directory for python
mkdir Bindings/python/export/SharedObjects

sh build-bindings.sh

cp Bindings/p2prc.h Bindings/python/export/SharedObjects/
cp Bindings/p2prc.so Bindings/python/export/SharedObjects/

cp Bindings/python/p2prc.py Bindings/python/export/

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