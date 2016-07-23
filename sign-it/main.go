package main

import (
	"bytes"
	"crypto/tls"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"strings"

	"github.com/digitalrebar/go-common/cert"
	"github.com/digitalrebar/go-common/service"
	consul "github.com/hashicorp/consul/api"
)

func getTrustMeServiceAddress() (string, error) {
	cc, err := consul.NewClient(consul.DefaultConfig())
	if err != nil {
		return "", err
	}
	if _, err := cc.Agent().Self(); err != nil {
		return "", err
	}

	svc, err := service.WaitService(cc, "trust-me-service", "")
	if err != nil {
		log.Printf("Could not get trust-me service: %v\n", err)
		return "", err
	}
	return fmt.Sprintf("https://%s:%d", svc[0].ServiceAddress, svc[0].ServicePort), nil
}

func main() {
	flagAuto := flag.Bool("A", false, "Use Consul to set address and key of trustme")
	flagAddr := flag.String("a", "https://127.0.0.1:8888", "Remote to talk to")
	flagLabel := flag.String("l", "internal", "Label to retrieve")

	flagOutput := flag.String("o", "cert", "Output base for action")

	flagMakeRoot := flag.Bool("m", false, "Should we make a root for this label")

	flagInfo := flag.Bool("i", false, "Should we get the validation cert")

	flagSign := flag.Bool("s", false, "Should we generate a signed cert")
	flagKey := flag.String("k", "", "Key to use for required signing")
	flagCN := flag.String("c", "cow", "Common Name for new signed cert")
	flagHosts := flag.String("h", "127.0.0.1", "Comma list of hosts to add to cert")

	flag.Parse()

	count := 0
	if *flagMakeRoot {
		count += 1
	}
	if *flagInfo {
		count += 1
	}
	if *flagSign {
		count += 1
	}
	if count > 1 {
		log.Fatalf("Must only specify one of Make, Sign, or Info")
	}
	if count < 1 {
		log.Fatalf("Must specify at least one of Make, Sign, or Info")
	}

	if *flagMakeRoot {
		if *flagAuto {
			address, err := getTrustMeServiceAddress()
			if err != nil {
				log.Fatal("Failed to get consul data: ", err)
			}
			*flagAddr = address
		}

		url := *flagAddr + "/api/v1/cfssl/root"
		kstring := ""
		if *flagKey != "" {
			kstring = ", \"auth_key\": \"" + *flagKey + "\""

		}
		jsonStr := "{\"label\": \"" + *flagLabel + "\"" + kstring + "}"
		jsonB := []byte(jsonStr)

		req, err := http.NewRequest("POST", url, bytes.NewBuffer(jsonB))
		tr := &http.Transport{
			TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
		}
		client := &http.Client{Transport: tr}
		resp, err := client.Do(req)
		if err != nil {
			log.Fatal("Failed: ", err)
		}
		defer resp.Body.Close()

		log.Println("Status: ", resp.Status)
	}

	if *flagInfo {
		if *flagAuto {
			address, err := getTrustMeServiceAddress()
			if err != nil {
				log.Fatal("Failed to get consul data: ", err)
			}
			*flagAddr = address
		}

		cacert, err := cert.GetCertificateForRoot(*flagAddr, *flagLabel)
		if err != nil {
			log.Printf("Failed to get Certificate for label: %s: %v\n", *flagLabel, err)
			return
		}
		if *flagOutput == "-" {
			fmt.Println(string(cacert))
		} else {
			err = ioutil.WriteFile(*flagOutput+".pem", cacert, 0600)
			if err != nil {
				log.Printf("Failed to output cert: %s: %v\n", *flagOutput+".pem", err)
			}
		}
	}

	if *flagSign {
		hosts := strings.Split(*flagHosts, ",")
		if *flagAuto {
			address, buffer, err := cert.GetTrustMeServiceInfo(*flagLabel)
			if err != nil {
				log.Fatal("Failed to get consul data: ", err)
			}
			*flagKey = string(buffer)
			*flagAddr = address
		}

		mycert, mykey, err := cert.CreateCertificate(*flagAddr, *flagKey, *flagLabel, *flagCN, hosts)
		if err != nil {
			log.Printf("Failed to get Create Certificate for label: %s\n", *flagLabel)
			log.Printf("Error: %v\n", err)
			return
		}

		if *flagOutput == "-" {
			fmt.Println(string(mycert))
			fmt.Println(string(mykey))
		} else {
			err = ioutil.WriteFile(*flagOutput+".pem", mycert, 0600)
			if err != nil {
				log.Printf("Failed to output cert: %s: %v\n", *flagOutput+".pem", err)
			}
			err = ioutil.WriteFile(*flagOutput+".key", mykey, 0600)
			if err != nil {
				log.Printf("Failed to output key: %s: %v\n", *flagOutput+".key", err)
			}
		}
	}
}
