package main

import (
	"crypto/md5"
	"encoding/base64"
	"log"
	"math/rand"
	"net/http"
	"time"
)

type Token struct {
	Token        string
	Username     string
	Realm        string
	Capabilities []string
}

var tokencache map[string]Token
var random_string string

var src = rand.NewSource(time.Now().UnixNano())

const letterBytes = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
const (
	letterIdxBits = 6                    // 6 bits to represent a letter index
	letterIdxMask = 1<<letterIdxBits - 1 // All 1-bits, as many as letterIdxBits
	letterIdxMax  = 63 / letterIdxBits   // # of letter indices fitting in 63 bits
)

func RandString(n int) string {
	b := make([]byte, n)
	// A src.Int63() generates 63 random bits, enough for letterIdxMax characters!
	for i, cache, remain := n-1, src.Int63(), letterIdxMax; i >= 0; {
		if remain == 0 {
			cache, remain = src.Int63(), letterIdxMax
		}
		if idx := int(cache & letterIdxMask); idx < len(letterBytes) {
			b[i] = letterBytes[idx]
			i--
		}
		cache >>= letterIdxBits
		remain--
	}

	return string(b)
}

func init() {
	tokencache = make(map[string]Token, 0)
	random_string = RandString(64)
}

func create_token(username string) string {
	hash := md5.Sum([]byte(username + random_string))
	return base64.StdEncoding.EncodeToString(hash[:])
}

func register_user(username, realm string, capabilities []string) Token {
	t := create_token(username)

	tok := Token{
		Token:        t,
		Username:     username,
		Realm:        realm,
		Capabilities: capabilities,
	}

	tokencache[t] = tok
	return tok
}

func lookup_token(token string) (Token, bool) {
	t, ok := tokencache[token]
	return t, ok
}

func validate_token(token, username string) bool {
	return token == create_token(username)
}

// Updates request to have authentication components for down strream
func has_token_info(req *http.Request) *Token {
	ts := req.Header.Get("DR-AUTH-TOKEN")
	tu := req.Header.Get("DR-AUTH-USER")

	if ts == "" || tu == "" {
		for _, cookie := range req.Cookies() {
			//log.Printf("Looking for cookies in request: %v: %v\n", cookie.Name, cookie.Value)
			if cookie.Name == "DrAuthToken" {
				ts = cookie.Value
			} else if cookie.Name == "DrAuthUser" {
				tu = cookie.Value
			}
		}
	}

	t, ok := lookup_token(ts)
	if !ok {
		log.Printf("For %v, Unknown token: %s\n", req, ts)
		return nil
	}

	if !validate_token(t.Token, tu) {
		log.Printf("For %v, Invalid token: %s for %s\n", req, ts, tu)
		return nil
	}

	req.Header.Set("X-Authenticated-Username", t.Username)
	cap := t.Capabilities
	if len(cap) > 0 && cap[0] != "None" {
		req.Header.Set("X-Authenticated-Capability", cap[0])
	}

	return &t
}

func add_token_info(t Token, req *http.Request, w http.ResponseWriter) {
	if req != nil {
		req.Header.Set("X-Authenticated-Username", t.Username)
		cap := t.Capabilities
		if len(cap) > 0 && cap[0] != "None" {
			req.Header.Set("X-Authenticated-Capability", cap[0])
		}
	}

	cookie := http.Cookie{Name: "DrAuthToken", Value: t.Token, Path: "/"}
	http.SetCookie(w, &cookie)
	cookie = http.Cookie{Name: "DrAuthUser", Value: t.Username, Path: "/"}
	http.SetCookie(w, &cookie)
	w.Header().Set("DR-AUTH-TOKEN", t.Token)
	w.Header().Set("DR-AUTH-USER", t.Username)
}
