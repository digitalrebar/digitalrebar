package main

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io/ioutil"
	"log"
	"net"
	"net/http"

	"github.com/ant0ine/go-json-rest/rest"
)

type NextServer struct {
	Server string `json:"next_server"`
}

type Frontend struct {
	DhcpInfo *DataTracker
	data_dir string
	cert_pem string
	key_pem  string
	base_pem string
	cfg      Config
}

func NewFrontend(cert_pem, key_pem, base_pem string, cfg Config, store LoadSaver) *Frontend {
	fe := &Frontend{
		data_dir: data_dir,
		cert_pem: cert_pem,
		key_pem:  key_pem,
		base_pem: base_pem,
		cfg:      cfg,
		DhcpInfo: NewDataTracker(store),
	}

	fe.DhcpInfo.load_data()

	return fe
}

// List function
func (fe *Frontend) GetAllSubnets(w rest.ResponseWriter, r *rest.Request) {
	nets := make([]*Subnet, 0, len(fe.DhcpInfo.Subnets))
	for _, net := range fe.DhcpInfo.Subnets {
		nets = append(nets, net)
	}
	w.WriteJson(nets)
}

// Get function
func (fe *Frontend) GetSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")

	if subnet, found := fe.DhcpInfo.Subnets[subnetName]; found {
		w.WriteJson(subnet)
	} else {
		rest.Error(w, "Not Found", http.StatusNotFound)
	}
}

// Create function
func (fe *Frontend) CreateSubnet(w rest.ResponseWriter, r *rest.Request) {
	s := &Subnet{}
	if r.Body == nil {
		rest.Error(w, "Must have body", http.StatusBadRequest)
		return
	}
	if err := r.DecodeJsonPayload(s); err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	if err, code := fe.DhcpInfo.AddSubnet(s); err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(s)
}

// Update function
func (fe *Frontend) UpdateSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	s := &Subnet{}
	if r.Body == nil {
		rest.Error(w, "Must have body", http.StatusBadRequest)
		return
	}
	if err := r.DecodeJsonPayload(s); err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	if err, code := fe.DhcpInfo.ReplaceSubnet(subnetName, s); err != nil {
		rest.Error(w, err.Error(), code)
		return
	}
	w.WriteJson(s)
}

// Delete function
func (fe *Frontend) DeleteSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")

	err, code := fe.DhcpInfo.RemoveSubnet(subnetName)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteHeader(code)
}

func (fe *Frontend) BindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	binding := Binding{}
	if r.Body == nil {
		rest.Error(w, "Must have body", http.StatusBadRequest)
		return
	}
	err := r.DecodeJsonPayload(&binding)
	if err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	err, code := fe.DhcpInfo.AddBinding(subnetName, binding)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(binding)
}

func (fe *Frontend) UnbindSubnet(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	mac := r.PathParam("mac")

	err, code := fe.DhcpInfo.DeleteBinding(subnetName, mac)
	if err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteHeader(http.StatusOK)
}

func (fe *Frontend) NextServer(w rest.ResponseWriter, r *rest.Request) {
	subnetName := r.PathParam("id")
	nextServer := NextServer{}
	if r.Body == nil {
		rest.Error(w, "Must have body", http.StatusBadRequest)
		return
	}
	if err := r.DecodeJsonPayload(&nextServer); err != nil {
		rest.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	ip := net.ParseIP(r.PathParam("ip"))

	if err, code := fe.DhcpInfo.SetNextServer(subnetName, ip, nextServer); err != nil {
		rest.Error(w, err.Error(), code)
		return
	}

	w.WriteJson(nextServer)
}

func (fe *Frontend) RunServer(blocking bool, auth_mode string) http.Handler {
	api := rest.NewApi()
	if auth_mode == "BASIC" {
		api.Use(&rest.AuthBasicMiddleware{
			Realm: "test zone",
			Authenticator: func(userId string, password string) bool {
				if userId == fe.cfg.Network.Username &&
					password == fe.cfg.Network.Password {
					return true
				}
				return false
			},
		})
	}
	api.Use(rest.DefaultDevStack...)
	router, err := rest.MakeRouter(
		rest.Get("/subnets", fe.GetAllSubnets),
		rest.Get("/subnets/#id", fe.GetSubnet),
		rest.Post("/subnets", fe.CreateSubnet),
		rest.Put("/subnets/#id", fe.UpdateSubnet),
		rest.Delete("/subnets/#id", fe.DeleteSubnet),
		rest.Post("/subnets/#id/bind", fe.BindSubnet),
		rest.Delete("/subnets/#id/bind/#mac", fe.UnbindSubnet),
		rest.Put("/subnets/#id/next_server/#ip", fe.NextServer),
	)
	if err != nil {
		log.Fatal(err)
	}
	api.SetApp(router)

	connStr := fmt.Sprintf(":%d", fe.cfg.Network.Port)
	log.Println("Web Interface Using", connStr)
	if blocking {
		server := &http.Server{
			Addr:    connStr,
			Handler: api.MakeHandler(),
		}

		if auth_mode == "KEY" {
			caCert, err := ioutil.ReadFile(fe.base_pem)
			if err != nil {
				log.Fatal(err)
			}
			caCertPool := x509.NewCertPool()
			caCertPool.AppendCertsFromPEM(caCert)

			// Setup HTTPS client
			tlsConfig := &tls.Config{
				ClientCAs: caCertPool,
				// NoClientCert
				// RequestClientCert
				// RequireAnyClientCert
				// VerifyClientCertIfGiven
				// RequireAndVerifyClientCert
				ClientAuth: tls.RequireAndVerifyClientCert,
			}
			tlsConfig.BuildNameToCertificate()
			server.TLSConfig = tlsConfig

			log.Fatal(server.ListenAndServeTLS(fe.cert_pem, fe.key_pem))
		} else {
			if fe.cert_pem == "" || fe.key_pem == "" {
				log.Fatal(http.ListenAndServe(connStr, api.MakeHandler()))
			} else {
				log.Fatal(http.ListenAndServeTLS(connStr, fe.cert_pem, fe.key_pem, api.MakeHandler()))
			}
		}
	}

	return api.MakeHandler()
}
