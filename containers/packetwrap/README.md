# deploy-packetwrap


### How to Use

Add the provider that you want:

In this example, PT is the API Token to use.
In this example, c8cfeca9-28d4-4cee-8280-8af2f5464f37 is the packet project in the account to create nodes under.

```
rebar providers create '{
  "auth_details": {
    "project_token": PT,
    "project_id": "c8cfeca9-28d4-4cee-8280-8af2f5464f37"
  },
  "name": "RackN Packet Account",
  "type": "PacketProvider"
}'
```

Add a node from that provider:

```
rebar -U rebar -P rebar1 nodes create \
    '{"name": "packet-1.neode.net",
      "variant": "RackN Packet Account",
      "hints": {
          "use-proxy": false,
          "provider-create-hint": {}
      }
    }'
```

Optionally, provider-create-hints can be a hash of values to provide choices.
  * facility = defaults to ewr1 - see packet for others
  * plan = defaults to baremetal_1 - see packet for others
  * os = defaults to ubuntu_14_04 - see packet for others some are:
    * centos_7
    * centos_6
    * debian_8
    * debian_7
  * hostname = defaults to pw-#{date} - override here

Something like this:

```
rebar -U rebar -P rebar1 nodes create \
    '{"name": "packet-1.neode.net",
      "variant": "RackN Packet Account",
      "hints": {
          "use-proxy": false,
          "provider-create-hint": {
            "os": "centos_7",
            "hostname": "packet-1.neode.net"
          }
      }
    }'
```

