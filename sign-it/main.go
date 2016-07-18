package main

import (
	"bytes"
	"crypto/tls"
	"flag"
	"log"
	"net/http"
	"strings"

	"github.com/digitalrebar/go-common/cert"
)

func main() {
	flagLabel := flag.String("l", "internal", "Label to retrieve")
	flagAddr := flag.String("a", "https://127.0.0.1:8888", "Remote to talk to")
	flagSign := flag.Bool("s", false, "Should we generate a signed cert")
	flagInfo := flag.Bool("i", false, "Should we get the validation cert")
	flagCN := flag.String("c", "cow", "Common Name for new signed cert")
	flagHosts := flag.String("h", "127.0.0.1", "Comma list of hosts to add to cert")
	flagMakeRoot := flag.Bool("m", false, "Should we make a root for this label")
	flagKey := flag.String("k", "", "Key to use for required signing")
	flag.Parse()

	if *flagMakeRoot {
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
		cacert, err := cert.GetCertificateForRoot(*flagAddr, *flagLabel)
		if err != nil {
			log.Printf("Failed to get Certificate for label: %s: %v\n", *flagLabel, err)
			return
		}
		log.Printf("Validation Certificate = %s\n", string(cacert))
	}

	if *flagSign {
		hosts := strings.Split(*flagHosts, ",")

		mycert, mykey, err := cert.CreateCertificate(*flagAddr, *flagKey, *flagLabel, *flagCN, hosts)
		if err != nil {
			log.Printf("Failed to get Create Certificate for label: %s\n", *flagLabel)
			log.Printf("Error: %v\n", err)
			return
		}

		log.Printf("My Certificate = %s\n", string(mycert))
		log.Printf("My Key = %s\n", string(mykey))
	}
}
