package main

import (
	"flag"
	"log"
	"net"
	"net/http"
	"strings"
	"time"

	"github.com/digitalrebar/go-common/cert"
	"github.com/digitalrebar/go-common/service"
	"github.com/digitalrebar/go-common/store"

	"github.com/cloudflare/cfssl/api/info"
	"github.com/cloudflare/cfssl/signer"
	"github.com/cloudflare/cfssl/whitelist"
	commonClient "github.com/digitalrebar/go-common/client"

	consul "github.com/hashicorp/consul/api"
)

var (
	defaultLabel    string
	signers         = map[string]signer.Signer{}
	whitelists      = map[string]whitelist.NetACL{}
	consulKeyPrefix = "trust-me/cert-store"
	simpleStore     store.SimpleStore
)

func loadSigners() error {
	keys, err := simpleStore.Keys()
	if err != nil {
		return err
	}

	data := make(map[string]map[string][]byte, 0)
	for _, k := range keys {
		parts := strings.Split(k, "/")
		var piece map[string][]byte
		var ok bool
		if piece, ok = data[parts[0]]; !ok {
			data[parts[0]] = make(map[string][]byte, 0)
			piece = data[parts[0]]
		}

		piece[parts[1]], _ = simpleStore.Load(k)
	}

	for k, v := range data {
		certB, ok := v["cert"]
		if !ok {
			continue
		}
		keyB, ok := v["key"]
		if !ok {
			continue
		}
		templateB, ok := v["template"]
		if !ok {
			continue
		}
		authKey, ok := v["authkey"]
		if !ok {
			continue
		}
		s, err := buildSigner(k, string(authKey), certB, keyB, templateB)
		if err != nil {
			log.Printf("Failed to load: %s: %v\n", k, err)

		} else {
			signers[k] = s
		}
	}

	return nil
}

func storeSigner(label, authKey string, cert, key, template []byte) error {

	err := simpleStore.Save(label+"/key", key)
	if err != nil {
		return err
	}
	err = simpleStore.Save(label+"/cert", cert)
	if err != nil {
		simpleStore.Remove(label + "/key")
		simpleStore.Remove(label + "/cert")
		return err
	}
	err = simpleStore.Save(label+"/template", template)
	if err != nil {
		simpleStore.Remove(label + "/template")
		simpleStore.Remove(label + "/key")
		simpleStore.Remove(label + "/cert")
		return err
	}
	err = simpleStore.Save(label+"/authkey", []byte(authKey))
	if err != nil {
		simpleStore.Remove(label + "/template")
		simpleStore.Remove(label + "/key")
		simpleStore.Remove(label + "/cert")
		simpleStore.Remove(label + "/authkey")
		return err
	}

	return nil
}

func main() {
	flagAddr := flag.String("a", ":8888", "listening address")
	flagConsul := flag.Bool("c", false, "Use consul storage")
	flagLocal := flag.Bool("l", false, "Use local storage")
	flag.Parse()

	var err error
	client, err := commonClient.Consul(true)
	if err != nil {
		log.Fatalf("Error getting Consul client: %v", err)
	}
	if *flagConsul {
		simpleStore, err = store.NewSimpleConsulStore(client, consulKeyPrefix)
		if err != nil {
			log.Fatalf("%v", err)
		}
	} else if *flagLocal {
		simpleStore, err = store.NewSimpleLocalStore(".")
		if err != nil {
			log.Fatalf("%v", err)
		}
	} else {
		simpleStore = store.NewSimpleMemoryStore()
	}

	err = loadSigners()
	if err != nil {
		log.Fatalf("%v", err)
	}

	defaultLabel = "internal"
	if _, ok := signers[defaultLabel]; !ok {
		err = newRootCertificate(defaultLabel, "")
		if err != nil {
			log.Fatalf("%v", err)
		}
	}

	// Make my certs - this is customer and not library because I'm using myself to sign.
	hosts := make([]string, 0, 0)
	hosts = append(hosts, "127.0.0.1")
	// GREG: Add more of these
	s, _ := signers[defaultLabel]
	csrPem, keyB, err := cert.CreateCSR(defaultLabel, "trust-me", hosts)
	if err != nil {
		log.Fatalf("%v", err)
	}
	csrRequest := signer.SignRequest{
		Request: string(csrPem),
	}
	certB, err := s.Sign(csrRequest)
	if err != nil {
		log.Fatalf("%v", err)
	}

	initStats()

	infoHandler, err := info.NewMultiHandler(signers, defaultLabel)
	if err != nil {
		log.Fatalf("%v", err)
	}
	if err != nil {
		log.Fatalf("Failed to talk to Consul: %v", err)
	}

	var localhost = whitelist.NewBasic()
	localhost.Add(net.ParseIP("127.0.0.1"))
	localhost.Add(net.ParseIP("::1"))
	metrics, err := whitelist.NewHandlerFunc(dumpMetrics, metricsDisallowed, localhost)
	if err != nil {
		log.Fatalf("failed to set up the metrics whitelist: %v", err)
	}

	http.HandleFunc("/api/v1/cfssl/authsign", dispatchRequest)
	http.Handle("/api/v1/cfssl/info", infoHandler)
	http.Handle("/api/v1/cfssl/metrics", metrics)
	http.HandleFunc("/api/v1/cfssl/root", newRoot)

	log.Println("Now listening on https:// ", *flagAddr)
	go func() {
		log.Printf("Cert API failed for trust-me: %v",
			cert.ListenAndServeTLS(*flagAddr, certB, keyB, nil))
	}()
	reg := &consul.AgentServiceRegistration{
		Name: "trust-me-service",
		Port: 8888,
		Tags: []string{"deployment:system"},
		Check: &consul.AgentServiceCheck{
			Script:   "/usr/local/bin/test_tm_up.sh",
			Interval: "10s",
		},
	}
	if err := service.Register(client, reg, true); err != nil {
		log.Fatalf("Failed to register service with Consul: %v", err)
	}
	for {
		time.Sleep(600)
	}
}
