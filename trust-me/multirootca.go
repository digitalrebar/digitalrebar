package main

import (
	"flag"
	"log"
	"net"
	"net/http"
	"strings"

	"github.com/digitalrebar/go-common/cert"
	"github.com/digitalrebar/go-common/store"

	"github.com/cloudflare/cfssl/api/info"
	"github.com/cloudflare/cfssl/signer"
	"github.com/cloudflare/cfssl/whitelist"

	consul "github.com/hashicorp/consul/api"
)

var (
	defaultLabel    string
	signers                = map[string]signer.Signer{}
	whitelists             = map[string]whitelist.NetACL{}
	consulKeyPrefix string = "/trust-me/cert-store"
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
		s, err := buildSigner(k, certB, keyB, templateB)
		if err != nil {
			log.Printf("Failed to load: %s: %v\n", k, err)

		} else {
			signers[k] = s
		}
	}

	return nil
}

func storeSigner(label string, cert, key, template []byte) error {

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

	return nil
}

func main() {
	flagAddr := flag.String("a", ":8888", "listening address")
	flagConsul := flag.Bool("c", false, "Use consul storage")
	flagLocal := flag.Bool("l", false, "Use local storage")
	flag.Parse()

	var err error
	if *flagConsul {
		client, err := consul.NewClient(consul.DefaultConfig())
		if err != nil {
			log.Fatal("%v", err)
		}
		if _, err := client.Agent().Self(); err != nil {
			log.Fatal("%v", err)
		}
		simpleStore, err = store.NewSimpleConsulStore(client, consulKeyPrefix)
		if err != nil {
			log.Fatal("%v", err)
		}
	} else if *flagLocal {
		simpleStore, err = store.NewSimpleLocalStore(".")
		if err != nil {
			log.Fatal("%v", err)
		}
	} else {
		simpleStore = store.NewSimpleMemoryStore()
	}

	err = loadSigners()
	if err != nil {
		log.Fatal("%v", err)
	}

	defaultLabel = "internal"
	if _, ok := signers[defaultLabel]; !ok {
		err = newRootCertificate(defaultLabel)
		if err != nil {
			log.Fatal("%v", err)
		}
	}

	// Make my certs - this is customer and not library because I'm using myself to sign.
	hosts := make([]string, 0, 0)
	hosts = append(hosts, "127.0.0.1")
	// GREG: Add more of these
	s, _ := signers[defaultLabel]
	csrPem, keyB, err := cert.CreateCSR(defaultLabel, "trust-me", hosts)
	if err != nil {
		log.Fatal("%v", err)
	}
	csrRequest := signer.SignRequest{
		Request: string(csrPem),
	}
	certB, err := s.Sign(csrRequest)
	if err != nil {
		log.Fatal("%v", err)
	}

	initStats()

	infoHandler, err := info.NewMultiHandler(signers, defaultLabel)
	if err != nil {
		log.Fatal("%v", err)
	}

	var localhost = whitelist.NewBasic()
	localhost.Add(net.ParseIP("127.0.0.1"))
	localhost.Add(net.ParseIP("::1"))
	metrics, err := whitelist.NewHandlerFunc(dumpMetrics, metricsDisallowed, localhost)
	if err != nil {
		log.Fatal("failed to set up the metrics whitelist: %v", err)
	}

	http.HandleFunc("/api/v1/cfssl/authsign", dispatchRequest)
	http.Handle("/api/v1/cfssl/info", infoHandler)
	http.Handle("/api/v1/cfssl/metrics", metrics)
	http.HandleFunc("/api/v1/cfssl/root", newRoot)

	log.Println("Now listening on https:// ", *flagAddr)
	log.Fatal(cert.ListenAndServeTLS(*flagAddr, certB, keyB, nil))
}
