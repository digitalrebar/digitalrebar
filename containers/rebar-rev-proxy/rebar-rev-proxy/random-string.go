package main

import (
	"math/rand"
	"time"
)

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

// Updates request to have authentication components for down strream
/*
func jjjhasTokenInfo(req *http.Request) *Token {
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

	t, ok := lookupToken(ts)
	if !ok {
		log.Printf("For %v, Unknown token: %s\n", req, ts)
		return nil
	}

	if !validateToken(t.Token, tu) {
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
*/
