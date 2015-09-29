package main

import (
	"flag"
	"fmt"
	"github.com/coreos/go-iptables/iptables"
	"github.com/hashicorp/consul/api"
	"log"
	"net"
	"os/exec"
	"strings"
	"time"
)

var myIPsubnet string

func init() {
	flag.StringVar(&myIPsubnet, "ip", "192.168.124.11/24", "IP to register services as")
}

func main() {
	flag.Parse()

	myIP, _, err := net.ParseCIDR(myIPsubnet)
	if err != nil {
		log.Fatal("Failed to parse ip: ", myIPsubnet, " ", err)
	}

	// Make sure IP is on the eth0 interface
	exec.Command("ip", "addr", "del", myIPsubnet, "dev", "eth0").Run()
	err = exec.Command("ip", "addr", "add", myIPsubnet, "dev", "eth0").Run()
	if err != nil {
		log.Fatal("Failed to add IP: ", myIPsubnet, " ", err)
	}

	client, err := api.NewClient(api.DefaultConfig())
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
	ipt.AppendUnique("nat", "POSTROUTING", "-o", "eth0", "-j", "MASQUERADE")
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
					Address: myIP.String(),
				}
				err = agent.ServiceRegister(&asr)
				if err != nil {
					log.Fatal("Failed to register service entry from consul agent: ", err)
				}

				sport := fmt.Sprintf("%d", service.ServicePort)

				err = ipt.AppendUnique("nat", "PREROUTING",
					"-p", "tcp",
					"-m", "tcp",
					"-i", "eth0",
					"-d", myIP.String(),
					"--dport", sport,
					"-j", "DNAT",
					"--to-destination", service.Address)
				if err != nil {
					log.Printf("Failed to add first rule: %v\n", err)
				}

				err = ipt.AppendUnique("filter", "FORWARD",
					"-p", "tcp",
					"-i", "eth0",
					"-d", service.Address,
					"--dport", sport,
					"-m", "tcp",
					"-j", "ACCEPT")
				if err != nil {
					log.Printf("Failed to add second rule: %v\n", err)
				}

				err = ipt.AppendUnique("nat", "PREROUTING",
					"-p", "udp",
					"-m", "udp",
					"-i", "eth0",
					"-d", myIP.String(),
					"--dport", sport,
					"-j", "DNAT",
					"--to-destination", service.Address)
				if err != nil {
					log.Printf("Failed to add first rule: %v\n", err)
				}

				err = ipt.AppendUnique("filter", "FORWARD",
					"-p", "udp",
					"-i", "eth0",
					"-d", service.Address,
					"--dport", sport,
					"-m", "udp",
					"-j", "ACCEPT")
				if err != nil {
					log.Printf("Failed to add second rule: %v\n", err)
				}

			}
		}

		first = false
		q.WaitIndex = meta.LastIndex
	}

}
