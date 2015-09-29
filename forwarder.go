package main

import (
	"flag"
	"fmt"
	"github.com/coreos/go-iptables/iptables"
	"github.com/hashicorp/consul/api"
	"log"
	"strings"
	"time"
)

var myIP, datacenter string

func init() {
	flag.StringVar(&myIP, "ip", "192.168.124.11", "IP to register services as")
	flag.StringVar(&datacenter, "dc", "digitalrebar", "Datacenter to use")
}

func main() {
	flag.Parse()

	config := api.DefaultConfig()
	config.Datacenter = datacenter
	client, err := api.NewClient(config)
	if err != nil {
		log.Fatal("Failed to attach to consul agent: ", err)
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
			log.Fatal("Failed to get service catalog from consul agent: ", err)
		}

		// We need to talk the services and see what has and hasn't been sent out
		todo := make([]string, 0)
		for k, _ := range services {
			if strings.HasPrefix(k, "internal-") {
				todo = append(todo, k)
			}
		}

		done := make(map[string]string, 0)
		for _, svc := range todo {
			for k, _ := range services {
				if "internal-"+k == svc {
					done["internal-"+k] = k
				}
			}
		}

		for _, k := range todo {
			fmt.Println("Working with service: ", k)

			svc_data, _, err := catalog.Service(k, "", nil)
			if err != nil {
				log.Fatal("Failed to get service entry from consul agent: ", err)
			}
			for _, service := range svc_data {
				fmt.Println("  Node: ", service.Node)
				fmt.Println("  Addr: ", service.Address)
				fmt.Println("  SvID: ", service.ServiceID)
				fmt.Println("  SvNm: ", service.ServiceName)
				fmt.Println("  SvAd: ", service.ServiceAddress)
				fmt.Println("  SvPt: ", service.ServicePort)

				if !first && done[k] == strings.TrimPrefix(k, "internal-") {
					log.Println("Skipping service: ", k)
					continue
				}

				log.Println("registering service: ", k)
				asr := api.AgentServiceRegistration{
					Name:    strings.TrimPrefix(service.ServiceName, "internal-"),
					Tags:    service.ServiceTags,
					Port:    service.ServicePort,
					Address: myIP,
				}
				err = agent.ServiceRegister(&asr)
				if err != nil {
					log.Fatal("Failed to register service entry from consul agent: ", err)
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
