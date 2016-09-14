#!/bin/bash

set -e
# Requires GOPATH to be set and will use it

OLDPATH=`pwd`
cd $GOPATH

go get github.com/digitalrebar/digitalrebar/go
rm -f bin/go
go build -o build-rebar github.com/digitalrebar/digitalrebar/go
./build-rebar 

cd $OLDPATH

