# ocb-dns-mgmt


# Build

* Clone the tree
* Set up Go PATH
```
export GOPATH=`pwd`
```
* Go dependencies
```
go get github.com/ant0ine/go-json-rest/rest
go get code.google.com/p/gcfg
```
* Change to source directory
```
cd src/github.com/galthaus/ocb-dns-mgmt
```
* Build and Install the code
```
go build ; go install ; rm ocb-dns-mgmt
```

# Running

* Change to the root of the source tree
* You will need a cert.pem and key.pem.  Use the generate_cert.go to build those.
* Edit the config.gcfg file to point to your PowerDNS install (needs to be 3.4.1 and up)
* Run:
```
./bin/ocb-dns-mgmt
```

