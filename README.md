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
* You will need a https-cert.pem and https-key.pem.
```
openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
   -keyout https-key.pem -out https-cert.pem -days 1001 \
   -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin"
```
* Edit the config.gcfg file to point to your PowerDNS install (needs to be 3.4.1 and up)
* Run:
```
./bin/ocb-dns-mgmt
```

