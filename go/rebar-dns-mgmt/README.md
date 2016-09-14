# rebar-dns-mgmt

This is a micro-service that provides an API for DigitalRebar to use to
configure DNS systems.  The service can manage an DigitalRebar installed
BIND server or a PowerDNS server through its HTTP API in version 3.4.5 or
higher.  When managing PowerDNS, it assumes that all domains (including
reverse domains) are already created.

The micro-service listens on both IPv4 and IPv6 interfaces.

# Api

The service responses, by default, to https requests on 6754.  The following URLs and
formats are used.  The service is controlled by a username and password.

## List Zones

Url: https://user:password@127.0.0.1:6754/zones
Returns: a json array of zone objects

```
[
  {
    "name": "name.of.zone",
    "records": [
      {
        "changetype": "",
        "content": "1.1.1.1",
        "name": "namein.name.of.zone",
        "type": "TypeOfRecord",
      }
      ...
    ]
  }
]
```

For Bind, list and show return the same thing full zone objects.
For PowerDNS, the list only returns names.  The Show will return the records.

### Show Zone

Url: https://user:password@127.0.0.1:6754/zones/name.of.zone
Returns: a json zone object like the element in the list with records
Errors: 404 if not found.

PowerDNS returns all records types include NS and SOA for the zone.

### Patch Zone

Url: https://user:password@127.0.0.1:6754/zones/name.of.zone
Data: json record to add or remove.
Returns: a json zone object like the element in list with records

Data is in the format:
```
{
  "changetype": "ADD or REMOVE",
  "content": "Address IPv4 or IPv6",
  "name": "FQDN to associate with",
  "type": "A or AAAA"
}
```

# Build

go get -u github.com/galthaus/rebar-dns-mgmt
go install github.com/galthaus/rebar-dns-mgmt

# Installing

The following things need to be done to run the micro-service.
From the source directory, do the following.

* mkdir /etc/dns-mgmt.d
* cp *.tmpl /etc/dns-mgmt.d
* You will need a https-cert.pem and https-key.pem.
```
openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
   -keyout https-key.pem -out https-cert.pem -days 1001 \
   -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin"
```
* cp https-key.pem /etc/dns-mgmt-https-key.pem
* cp https-cert.pem /etc/dns-mgmt-https-cert.pem

NOTE: Sometimes certs need addition configuration to deal with names or IPs.

# Running

```
$GOPATH/bin/rebar-dns-mgmt
```
