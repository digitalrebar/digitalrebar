#!/usr/bin/env bash

set -e
# Requires GOPATH to be set and will use it

if [[ $PWD != */src/github.com/digitalrebar/digitalrebar/go ]]; then
    cd "${GOPATH%%:*}"
    go get -d github.com/digitalrebar/digitalrebar/go
    cd src/github.com/digitalrebar/digitalrebar/go
fi

DR_TAG=${DR_TAG:-latest}
if [[ $DR_TAG != "latest" ]] ; then
    git fetch --all
    git checkout $DR_TAG
else
    echo "Assuming master/active branch is already checked out"
fi

mkdir -p vendor_src
(cd vendor_src && ln -sf ../vendor src)

glide i

binversion="$(go run version.go)"

arches=("amd64")
oses=("linux" "darwin")
packages=("github.com/digitalrebar/digitalrebar/go/certificates/sign-it"
	  "github.com/digitalrebar/digitalrebar/go/certificates/trust-me"
	  "github.com/digitalrebar/digitalrebar/go/rebar-dhcp"
	  "github.com/digitalrebar/digitalrebar/go/rebar-dns-mgmt"
	  "github.com/digitalrebar/digitalrebar/go/rule-engine"
	  "github.com/digitalrebar/digitalrebar/go/rebar-rev-proxy"
	  "github.com/digitalrebar/digitalrebar/go/rebar-api/rebar"
	  "github.com/digitalrebar/digitalrebar/go/forwarder"
	  "github.com/digitalrebar/digitalrebar/go/provisioner-mgmt")

for arch in "${arches[@]}"; do
    for os in "${oses[@]}"; do
        binpath="bin/$binversion/$os/$arch"
        mkdir -p "$binpath"
        for pkg in "${packages[@]}"; do
            GOOS="$os" GOARCH="$arch" go build -o "${binpath}/${pkg##*/}" "$pkg"
        done
    done
done

cd "bin/$binversion"
if [[ $(uname -s) == Darwin ]] ; then
    find . -type f -perm -u=x |xargs shasum -a 256 >sha256sums
else
    find . -type f -perm -u=x |xargs sha256sum >sha256sums
fi
cd -

if [[ $DR_TAG == "latest" ]] ; then
    cp -r "bin/$binversion" bin/latest
fi

