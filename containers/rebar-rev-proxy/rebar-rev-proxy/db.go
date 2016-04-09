package main

import (
	"encoding/base64"
	"net/http"
	"strings"
)

type UserDB map[string]string

type DBAuthFilter struct {
	myMux    *http.ServeMux
	store    LoadSaver
	database UserDB
}

func NewDBAuthFilter(mux *http.ServeMux, store LoadSaver) *DBAuthFilter {
	db := &DBAuthFilter{
		myMux: mux,
		store: store,
	}
	store.Load(&db.database)

	return db
}

func (db *DBAuthFilter) Validate(username, password string) bool {
	p, ok := db.database[username]
	if !ok {
		return false
	}
	return p == password
}

func (db *DBAuthFilter) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	auth := strings.SplitN(r.Header["Authorization"][0], " ", 2)

	if len(auth) != 2 || auth[0] != "Basic" {
		http.Error(w, "bad syntax", http.StatusBadRequest)
		return
	}

	payload, _ := base64.StdEncoding.DecodeString(auth[1])
	pair := strings.SplitN(string(payload), ":", 2)

	if len(pair) != 2 || !db.Validate(pair[0], pair[1]) {
		http.Error(w, "authorization failed", http.StatusUnauthorized)
		return
	}

	db.myMux.ServeHTTP(w, r)
}
