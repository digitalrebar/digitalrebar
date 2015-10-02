package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"os/exec"
	"strings"
	"time"

	"github.com/coreos/go-iptables/iptables"
	"github.com/hashicorp/consul/api"
)

var myIPsubnet, myIface string

func init() {
	flag.StringVar(&myIPsubnet, "ip", "192.168.124.11/24", "IP to register services as")
	flag.StringVar(&myIface, "iface", "eth0", "Network interface to use")
}

func ClearChains(ipt *iptables.IPTables) {
	chains := []string{"nat", "PREROUTING", "nat", "POSTROUTING",
		"filter", "INPUT", "filter", "OUTPUT", "filter", "FORWARD"}
	for i := 0; i < len(chains); i = i + 2 {
		if err := ipt.ClearChain(chains[i], chains[i+1]); err != nil {
			log.Fatalf("Error clearing chain %v %v\n", chains[i], chains[i+1])
		}
	}
}

func main() {
	flag.Parse()

	myIP, myIPNet, err := net.ParseCIDR(myIPsubnet)
	if err != nil {
		log.Fatal("Failed to parse ip: ", myIPsubnet, " ", err)
	}

	iface, err := net.InterfaceByName(myIface)
	if err != nil {
		log.Fatalf("Interface %s does not exist.", myIface)
	}

	addrs, err := iface.Addrs()
	if err != nil {
		log.Fatalf("Failed to retrieve addresses for %s", myIface)
	}

	haveOurIP := false
	internalAddrs := make([]string, 0, len(addrs))
	for _, addr := range addrs {
		thisIP, thisIPNet, _ := net.ParseCIDR(addr.String())
		// Only care about addresses that are not link-local.
		if !thisIP.IsGlobalUnicast() {
			continue
		}
		// Only deal with IPv4 for now.
		if thisIP.To4() == nil {
			continue
		}
		if thisIPNet.String() == myIPNet.String() {
			haveOurIP = true
		} else {
			internalAddrs = append(internalAddrs, thisIPNet.String())
		}
	}

	if !haveOurIP {
		// Make sure IP is on the proper interface
		err = exec.Command("ip", "addr", "add", myIPsubnet, "dev", myIface).Run()
		if err != nil {
			log.Fatal("Failed to add IP: ", myIPsubnet, " ", err)
		}
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
	// Clear out chains we will be messing with.
	ClearChains(ipt)

	ipt.AppendUnique("nat", "POSTROUTING", "-o", myIface, "-j", "MASQUERADE")
	// Turn on masquerading for all internal -> external connections
	// for _, addr := range internalAddrs {
	// 	ipt.AppendUnique("nat", "POSTROUTING", "-s", addr, "-j", "MASQUERADE")
	// 	if err != nil {
	// 		log.Printf("Add Masquerade failed: %v\n", err)
	// 	}
	// }

	knownServices := make(map[string]*api.CatalogService)
	for {
		fmt.Println("Reading services")
		services, meta, err := catalog.Services(&q)
		if err != nil {
			log.Fatal("Failed to get service catalog from consul agent: ", err)
		}
		wantedServices := make(map[string]*api.CatalogService)
		toRemoveServices := make(map[string]*api.CatalogService)
		untouchedServices := make(map[string]*api.CatalogService)
		for svcName := range services {
			if !strings.HasPrefix(svcName, "internal-") {
				continue
			}
			svcCatalog, _, err := catalog.Service(svcName, "", nil)
			if err != nil {
				log.Fatal("Failed to get service entry from consul agent: ", err)
			}
			if len(svcCatalog) == 0 {
				continue
			}
			svc := svcCatalog[0]
			// Bucketize the services we want to forward
			if knownSvc, ok := knownServices[svcName]; ok {
				if knownSvc.Address == svc.Address &&
					knownSvc.ServiceAddress == svc.ServiceAddress &&
					knownSvc.ServicePort == svc.ServicePort {
					// Nothing changed, it goes in the untouched bucket.
					untouchedServices[svcName] = knownSvc
				} else {
					// Something changed, it goes in the toRemove and
					// wanted buckets
					toRemoveServices[svcName] = knownSvc
					wantedServices[svcName] = knownSvc
				}
			} else {
				// New service, it goes in the wanted bucket
				wantedServices[svcName] = svc
			}
		}
		// Any known services that are not in the wanted or untouched bucket
		// need to be removed.
		for svcName, svc := range knownServices {
			if _, ok := wantedServices[svcName]; ok {
				continue
			}
			if _, ok := untouchedServices[svcName]; ok {
				continue
			}
			toRemoveServices[svcName] = svc
		}
		// Delete services we no longer care about
		for svcName, svc := range toRemoveServices {
			// Whack service registration
			agentSvcs, err := agent.Services()
			if err == nil {
				agentSvc, ok := agentSvcs[strings.Trim(svcName, "internal-")]
				if ok {
					agent.ServiceDeregister(agentSvc.ID)
				}
			}
			svcAddr := svc.ServiceAddress
			if svcAddr == "" {
				svcAddr = svc.Address
			}
			svcPort := fmt.Sprintf("%d", svc.ServicePort)
			// Whack DNAT + forwarding rules
			// We don't particularly care if they fail.
			ipt.Delete("nat", "PREROUTING",
				"-p", "udp",
				"-m", "udp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "DNAT",
				"--to-destination", svcAddr)
			ipt.Delete("nat", "PREROUTING",
				"-p", "tcp",
				"-m", "tcp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "DNAT",
				"--to-destination", svcAddr)
			ipt.Delete("filter", "FORWARD",
				"-p", "tcp",
				"-m", "tcp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "ACCEPT")
			ipt.Delete("filter", "FORWARD",
				"-p", "udp",
				"-m", "udp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "ACCEPT")
		}
		// Add new services we do care about
		for svcName, svc := range wantedServices {
			svcAddr := svc.ServiceAddress
			if svcAddr == "" {
				svcAddr = svc.Address
			}
			svcPort := fmt.Sprintf("%d", svc.ServicePort)
			// Add DNAT + forwarding rules
			ipt.AppendUnique("nat", "PREROUTING",
				"-p", "udp",
				"-m", "udp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "DNAT",
				"--to-destination", svcAddr)
			ipt.AppendUnique("nat", "PREROUTING",
				"-p", "tcp",
				"-m", "tcp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "DNAT",
				"--to-destination", svcAddr)
			ipt.AppendUnique("filter", "FORWARD",
				"-p", "tcp",
				"-m", "tcp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "ACCEPT")
			ipt.AppendUnique("filter", "FORWARD",
				"-p", "udp",
				"-m", "udp",
				"-d", myIP.String(),
				"--dport", svcPort,
				"-j", "ACCEPT")
			log.Println("registering service: ", svcName)
			asr := api.AgentServiceRegistration{
				Name:    strings.TrimPrefix(svc.ServiceName, "internal-"),
				Tags:    svc.ServiceTags,
				Port:    svc.ServicePort,
				Address: myIP.String(),
			}
			err = agent.ServiceRegister(&asr)
			if err != nil {
				log.Fatal("Failed to register service entry from consul agent: ", err)
			}
			untouchedServices[svcName] = svc
		}
		knownServices = untouchedServices
		q.WaitIndex = meta.LastIndex
	}

}
