# rebar-dhcp

This is a Simple DHCP server that provides enough for what DigitalRebar
needs to operate an environment.  It also provides a managemenet interface
that allows for the alteration of leases and mapping data on the fly.
This requires no-restarts of the server.

# Api

The service responses, by default, to https requests on 6755.  The following URLs and
formats are used.  The service is controlled by a username and password.

## List Subnets

Url: https://user:password@127.0.0.1:6755/subnets  
Method: Get  
Returns: a json array of subnet objects

```
[
  {
    "name": "192.168.124.0",
    "subnet": "192.168.124.0/24",
    "next_server": [[]]
    "active_start": "192.168.124.22",
    "active_end": "192.168.124.92",
    "active_lease_time": 300,
    "reserved_lease_time": 7200,
    "leases": [
      {
        "ip": "192.168.124.23",
        "mac": "52:54:77:4e:00:02",
        "valid": true,
        "expire_time": "2015-07-18T03:55:48.210556397Z"
      },
      {
        "ip": "192.168.124.24",
        "mac": "52:54:77:4e:00:00",
        "valid": true,
        "expire_time": "2015-07-18T03:55:32.331706048Z"
      },
      {
        "ip": "192.168.124.25",
        "mac": "52:54:77:4e:00:01",
        "valid": true,
        "expire_time": "2015-07-18T03:55:40.329631313Z"
      }
    ],
    "bindings": [
      {
        "ip": "192.168.124.22",
        "mac": "aa:bb:cc:dd:ee:ff"
      }
    ],
    "options": [
      {
        "id": 67,
        "value": "discovery/pxelinux.0"
      },
      {
        "id": 1,
        "value": "255.255.255.0"
      },
      {
        "id": 28,
        "value": "192.168.124.255"
      },
      {
        "id": 3,
        "value": "192.168.124.1"
      },
      {
        "id": 15,
        "value": "alneode.com"
      },
      {
        "id": 6,
        "value": "192.168.124.10"
      }
    ]
  }
]
```

### Show Subnet

Url: https://user:password@127.0.0.1:6754/subnets/192.168.124.10  
Method: Get  
Returns: a json subnet object like the element in the list with leases  
Errors: 404 if not found.

The subnet object containes leases (dynamic information), bindings
(config-based info about devices), and options that should be used 
for nodes in the subnet.

The id of an Option is the number from the RFC2132 or follow-on ones.
The value is a string representation of the type and will be converted
to a byte string before sending to clients.

### Create Subnet

Url: https://user:password@127.0.0.1:6754/subnets  
Method: Post  
Data: json Subnet object (can have bindings, leases, and options)  
options)  
Returns: a json subnet object like the element in list  
Errors: 400 if request not valid  
        409 if subnet name already in use

Data is in the format:
```
{
    "name": "192.168.124.0",
    "subnet": "192.168.124.0/24",
    "next_server": "192.168.124.10",
    "active_start": "192.168.124.22",
    "active_end": "192.168.124.92",
    "active_lease_time": 300,
    "reserved_lease_time": 7200
}
```

All fields are optional expect name and subnet.

Active start/end specifies a free range of DHCP addresses given to
anyone.  If none is specified, then only bound addresses will be given
out.

### Update Subnet

Url: https://user:password@127.0.0.1:6754/subnets/*##name##*  
Method: Put  
Data: json Subnet object (can have bindings, leases, and options)  
options)  
Returns: a json subnet object like the element in list  
Errors: 400 if request not valid  
        404 if subnet name not found

Just like create but updates an existing subnet

### Delete Subnet

Url: https://user:password@127.0.0.1:6754/subnets/*##name##*  
Method: Delete  
Returns: 200  
Errors: 404 if subnet name not found

Delete the subnet and all info with the subnet.


### Bind Mac/IP in Subnet

Url: https://user:password@127.0.0.1:6754/subnets/*##name##*/bind  
Method: Post  
Data: json Binding Object  
Returns: a json binding object  
Errors: 404 if subnet name not found  
        400 if data is not valid

This call updates or creates a binding of a mac to an ip.
The binding object may also contain options for the device.

The bind object looks like:
```
{
  "ip": "192.168.124.22",
  "mac": "aa:bb:cc:dd:ee:ff",
  "options": [
    {
      "id": 1,
      "value": "255.255.255.0"
    }
  ]
}

```

### Unbind Mac/IP in Subnet

Url: https://user:password@127.0.0.1:6754/subnets/*##name##*/bind/*##mac##*  
Method: Delete  
Returns: 200 if binding removed  
Errors: 404 if subnet name not found or mac not found

Deletes the binding of the mac/ip pair

### Set Next Server for an IP

Url: https://user:password@127.0.0.1:6754/subnets/*##name##*/next_server/*##ip##*  
Method: Put  
Data: Json next server object  
Returns: The next server object  
Errors: 404 if subnet name not found or mac not found

This sets the next server that the DHCP ACK should point to
for this IP.  All mac bindings will be updated with this value.

The next server object looks like:
```
{
  "next_server": "1.1.1.1"
}

```


# Testing

To run the unit tests:

* go get -t
* go test

To get coverage:

* go test -coverprofile=cover.out
* sed -i -e "s#.*/\(.*\.go\)#\./\\1#" cover.out 
* go tool cover -html=cover.out -o coverage.html

