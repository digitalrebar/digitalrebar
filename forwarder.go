package main

import (
	"fmt"
	"github.com/coreos/go-iptables/iptables"
	"github.com/hashicorp/consul/api"
	"log"
	"strings"
	"time"
)

func main() {
	client, err := api.NewClient(api.DefaultConfig())
	if err != nil {
		log.Fatal("Failed to attach to conul agent: ", err)
	}

	agent := client.Agent()
	catalog := client.Catalog()
	q := api.QueryOptions{
		WaitIndex: 0,
		WaitTime:  time.Second * 10,
	}

	ipt, err := iptables.New()
	if err != nil {
		log.Fatalf("New failed:Fatalf %v", err)
	}

	// Turn on masquerading
	ipt.AppendUnique("nat", "POSTROUTING", "-j", "MASQUERADE")
	if err != nil {
		log.Printf("Add Masquerade failed: %v\n", err)
	}

	first := true
	for {
		fmt.Println("Reading services")
		services, meta, err := catalog.Services(&q)
		if err != nil {
			log.Fatal("Failed to get service catalog from conul agent: ", err)
		}

		for k, v := range services {
			fmt.Println("k: ", k, " v:", v)

			svc_data, _, err := catalog.Service(k, "", nil)
			if err != nil {
				log.Fatal("Failed to get service entry from conul agent: ", err)
			}
			for _, service := range svc_data {
				fmt.Println("  Node: ", service.Node)
				fmt.Println("  Addr: ", service.Address)
				fmt.Println("  SvID: ", service.ServiceID)
				fmt.Println("  SvNm: ", service.ServiceName)
				fmt.Println("  SvAd: ", service.ServiceAddress)
				fmt.Println("  SvPt: ", service.ServicePort)

				if strings.HasPrefix(k, "f-rebar-") && !first {
					log.Println("Skipping service: ", k)
					continue
				}

				log.Println("registering service: ", k)
				asr := api.AgentServiceRegistration{
					Name:    "f-rebar-" + service.ServiceName,
					Tags:    service.ServiceTags,
					Port:    service.ServicePort,
					Address: "192.168.124.10",
				}
				err = agent.ServiceRegister(&asr)
				if err != nil {
					log.Fatal("Failed to register service entry from conul agent: ", err)
				}

				saddrport := fmt.Sprintf("%s:%d", service.Address, service.ServicePort)
				sport := fmt.Sprintf("%d", service.ServicePort)

				// iptables -t nat -A PREROUTING -p tcp -i eth0 --dport 3000 -j DNAT --to-destination $WEB_HOST:3000
				err = ipt.AppendUnique("nat", "PREROUTING",
					"-j", "DNAT",
					"--to-destination", saddrport,
					"--dport", sport,
					"-i", "eth0",
					"-p", "tcp")
				if err != nil {
					log.Printf("Failed to add first rule: %v\n", err)
				}

				// iptables -A FORWARD -p tcp -d $WEB_HOST --dport 3000 -m state --state NEW,ESTABLISHED,RELATED -j ACCEPT
				err = ipt.AppendUnique("filter", "FORWARD",
					"-j", "ACCEPT",
					"-d", service.Address,
					"--dport", sport,
					"-m", "state",
					"--state", "NEW,ESTABLISHED,RELATED",
					"-p", "tcp")
				if err != nil {
					log.Printf("Failed to add second rule: %v\n", err)
				}

				// iptables -t nat -A PREROUTING -p udp -i eth0 --dport 3000 -j DNAT --to-destination $WEB_HOST:3000
				err = ipt.AppendUnique("nat", "PREROUTING",
					"-j", "DNAT",
					"--to-destination", saddrport,
					"--dport", sport,
					"-i", "eth0",
					"-p", "udp")
				if err != nil {
					log.Printf("Failed to add first rule: %v\n", err)
				}

				// iptables -A FORWARD -p udp -d $WEB_HOST --dport 3000 -m state --state NEW,ESTABLISHED,RELATED -j ACCEPT
				err = ipt.AppendUnique("filter", "FORWARD",
					"-j", "ACCEPT",
					"-d", service.Address,
					"--dport", sport,
					"-m", "state",
					"--state", "NEW,ESTABLISHED,RELATED",
					"-p", "udp")
				if err != nil {
					log.Printf("Failed to add second rule: %v\n", err)
				}

			}
		}

		first = false
		q.WaitIndex = meta.LastIndex
	}

}
