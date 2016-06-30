package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"io"
	"net/http"
	"time"

	"github.com/dgrijalva/jwt-go"
	"github.com/dgrijalva/jwt-go/request"
)

const defaultTTL = 3600 * 24 * 7 // 1 week

// A Provider creates, signs, and retrieves JSON Web Tokens (JWTs).
type JwtProvider interface {
	// New returns a new JWT Token using the Store's signing method.
	New() *jwt.Token
	// Sign digitally signs the Token to return the JWT byte slice.
	Sign(token *jwt.Token) ([]byte, error)
	// Get gets the valid JWT from the Authorization header. If the token is
	// missing, expired, or the signature does not validate, returns an error.
	Get(req *http.Request) (*jwt.Token, error)
	// Add token info to response and downstream request.
	AddTokenInfo(token *jwt.Token, w http.ResponseWriter, req *http.Request)
}

// Config configures a Manager.
type JwtConfig struct {
	// digital signing method, defaults to jwt.SigningMethodHS256 (SHA256)
	Method jwt.SigningMethod
	// token expiration time in seconds, defaults to 1 week
	TTL int64
}

// Manager is a JSON Web Token (JWT) Provider which create or retrieves tokens
// with a particular signing key and options.
type JwtManager struct {
	key    []byte
	method jwt.SigningMethod
	ttl    int64
}

// New creates a new Manager which provides JWTs using the given signing key.
// Defaults to signing with SHA256 HMAC (jwt.SigningMethodHS256) and expiring
// tokens after 1 week.
func NewJwtManager(key []byte, configs ...JwtConfig) *JwtManager {
	var c JwtConfig
	if len(configs) == 0 {
		c = JwtConfig{}
	} else {
		c = configs[0]
	}
	m := &JwtManager{
		key:    key,
		method: c.Method,
		ttl:    c.TTL,
	}
	m.setDefaults()
	return m
}

func (m *JwtManager) setDefaults() {
	if m.method == nil {
		m.method = jwt.SigningMethodHS256
	}
	if m.ttl == 0 {
		m.ttl = defaultTTL
	}
}

// getKey accepts an unverified JWT and returns the signing/verification key.
// Also ensures tha the token's algorithm matches the signing method expected
// by the manager.
func (m *JwtManager) getKey(unverified *jwt.Token) (interface{}, error) {
	// require token alg to match the set signing method, do not allow none
	if meth := unverified.Method; meth == nil || meth.Alg() != m.method.Alg() {
		return nil, jwt.ErrHashUnavailable
	}
	return m.key, nil
}

// New returns a new *jwt.Token which has the prescribed signing method, issued
// at time, and expiration time set on it.
//
// Add claims to the Claims map and use the controller to Sign(token) to get
// the standard JWT signed string representation.
func (m *JwtManager) New(user string) *jwt.Token {
	d := time.Duration(m.ttl) * time.Second
	claims := &jwt.StandardClaims{
		IssuedAt:  time.Now().Unix(),
		ExpiresAt: time.Now().Add(d).Unix(),
		Issuer:    "digital rebar",
		Id:        user,
	}
	token := jwt.NewWithClaims(m.method, claims)
	return token
}

func encrypt(key []byte, text string) (string, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return "", err
	}

	ciphertext := make([]byte, aes.BlockSize+len(text))

	// iv =  initialization vector
	iv := ciphertext[:aes.BlockSize]
	if _, err := io.ReadFull(rand.Reader, iv); err != nil {
		return "", err
	}

	cfb := cipher.NewCFBEncrypter(block, iv)
	cfb.XORKeyStream(ciphertext[aes.BlockSize:], []byte(text))

	return base64.URLEncoding.EncodeToString(ciphertext), nil
}

func decrypt(key []byte, b64ciphertext string) (string, error) {

	block, err := aes.NewCipher(key)
	if err != nil {
		return "", err
	}

	ciphertext, err := base64.URLEncoding.DecodeString(b64ciphertext)

	if len(ciphertext) < aes.BlockSize {
		err = errors.New("ciphertext too short")
		return "", err
	}

	iv := ciphertext[:aes.BlockSize]
	ciphertext = ciphertext[aes.BlockSize:]

	cfb := cipher.NewCFBDecrypter(block, iv)
	cfb.XORKeyStream(ciphertext, ciphertext)

	return string(ciphertext), nil
}

// Sign digitally signs a *jwt.Token using the token's method and the manager's
// signing key to return a string
func (m *JwtManager) Sign(token *jwt.Token) (string, error) {
	jwtString, err := token.SignedString(m.key)
	if err != nil {
		return "", err
	}

	return encrypt(m.key, jwtString)
}

// Extractor for finding a token in a cookie.  Looks at each specified
// cookie in order until there's a match
type CookieExtractor []string

func (e CookieExtractor) ExtractToken(req *http.Request) (string, error) {
	// loop over cookie names and return the first one that contains data
	for _, cn := range e {
		for _, cookie := range req.Cookies() {
			if cookie.Name == cn {
				return cookie.Value, nil
			}
		}
	}
	return "", request.ErrNoTokenInRequest
}

// Tries Extractors in order until one returns a token string or an error occurs and decrypts the result
type MultiDecryptExtractor struct {
	Key        []byte
	Extractors []request.Extractor
}

func (e MultiDecryptExtractor) ExtractToken(req *http.Request) (string, error) {
	// loop over header names and return the first one that contains data
	for _, extractor := range e.Extractors {
		if tok, err := extractor.ExtractToken(req); tok != "" {
			return decrypt(e.Key, tok)
		} else if err != request.ErrNoTokenInRequest {
			return "", err
		}
	}
	return "", request.ErrNoTokenInRequest
}

// Get gets the signed JWT from the Authorization header. If the token is
// missing, expired, or the signature does not validate, returns an error.
func (m *JwtManager) Get(req *http.Request) (*jwt.Token, error) {
	extractor := MultiDecryptExtractor{
		m.key,
		[]request.Extractor{
			CookieExtractor{"DrAuthToken"},
			request.HeaderExtractor{"DR-AUTH-TOKEN"},
			request.ArgumentExtractor{"token"},
			request.AuthorizationHeaderExtractor,
		},
	}
	return request.ParseFromRequestWithClaims(req, extractor, &jwt.StandardClaims{}, m.getKey)
}

// Updates the downstream request (if there is one) to have the user id
// Updates the response to have the token info.
func (m *JwtManager) AddTokenInfo(t *jwt.Token, w http.ResponseWriter, req *http.Request) error {
	if req != nil {
		// Claims have been validated or newly created.
		claims, ok := t.Claims.(*jwt.StandardClaims)
		if !ok {
			return jwt.NewValidationError("Missing id in claims", jwt.ValidationErrorId)
		}
		req.Header.Set("X-Authenticated-Username", claims.Id)
	}

	signedString, err := m.Sign(t)
	if err != nil {
		return err
	}
	cookie := http.Cookie{Name: "DrAuthToken", Value: signedString, Path: "/"}
	http.SetCookie(w, &cookie)
	w.Header().Set("DR-AUTH-TOKEN", signedString)
	return nil
}
