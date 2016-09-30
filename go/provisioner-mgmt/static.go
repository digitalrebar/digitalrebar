package main

import (
	"net"
	"net/http"
)

func serveStatic(listenAt, fsPath string) error {
	conn, err := net.Listen("tcp", listenAt)
	if err != nil {
		return err
	}
	fs := http.FileServer(http.Dir(fsPath))
	http.Handle("/", fs)
	return http.Serve(conn, nil)
}
