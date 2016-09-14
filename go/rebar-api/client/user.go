package client

// Deprecated: use api instead. client will not be updated

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"

	"github.com/digitalrebar/rebar-api/datatypes"
	"golang.org/x/crypto/nacl/box"
)

// User wraps datatypes.User to provide the client API.
type User struct {
	datatypes.User
	Timestamps
	apiHelper
}

// PasswordChangeToken is an auxillary struct used for the secure
// password update API.
type PasswordChangeToken struct {
	Token []byte `json:"token"`
}

func (p *PasswordChangeToken) MarshalJSON() ([]byte, error) {
	k := map[string]string{}
	k["token"] = base64.StdEncoding.EncodeToString(p.Token)
	return json.Marshal(k)
}

func (p *PasswordChangeToken) UnmarshalJSON(buf []byte) error {
	k := map[string]string{}
	if err := json.Unmarshal(buf, &k); err != nil {
		return err
	}
	tok, ok := k["token"]
	if !ok {
		return errors.New("No token!")
	}
	buf, err := base64.StdEncoding.DecodeString(tok)
	if len(buf) != 32 {
		return errors.New("Token not exactly 32 bytes")
	}
	p.Token = buf
	return err
}

// Users returns all the Users in the system
func Users() (res []*User, err error) {
	res = make([]*User, 0)
	return res, List("users", &res)
}

// StartPasswordReset fetches a PasswordChangeToken for this user.
func (u *User) StartPasswordReset() (*PasswordChangeToken, error) {
	buf, err := session.request("GET", urlFor(u, "start_password_reset"), nil)
	if err != nil {
		return nil, err
	}
	res := &PasswordChangeToken{}
	return res, json.Unmarshal(buf, res)
}

// CompletePasswordReset encrypts the new password in such a way that
// it can only be decrypted by the Server using the generated Token,
// and posts everything needed back to the server.
func (u *User) CompletePasswordReset(tok *PasswordChangeToken, newPassword string) error {
	pubKey, privKey, err := box.GenerateKey(rand.Reader)
	theirPubKey := [32]byte{}
	nonce := [24]byte{}
	copy(theirPubKey[:], tok.Token[:])
	cnt, err := rand.Read(nonce[:])
	if err != nil {
		return err
	}
	if cnt != 24 {
		return errors.New("Not enough randomness to encrypt the box")
	}
	payload := `{"password":"` + newPassword + `"}`
	encPayload := box.Seal(nil, []byte(payload), &nonce, &theirPubKey, privKey)
	body := fmt.Sprintf(`{"token":"%v","decoder":"%v","nonce":"%v","payload":"%v","digest":true}`,
		base64.StdEncoding.EncodeToString(theirPubKey[:]),
		base64.StdEncoding.EncodeToString(pubKey[:]),
		base64.StdEncoding.EncodeToString(nonce[:]),
		base64.StdEncoding.EncodeToString(encPayload))
	_, err = session.request("POST", urlFor(u, "complete_password_reset"), []byte(body))
	if err != nil {
		return err
	}
	return nil
}

// Capabilities fetches the capability map for this user.
func (u *User) Capabilities() (*map[string]interface{}, error) {
	buf, err := session.request("GET", urlFor(u, "capabilities"), nil)
	if err != nil {
		return nil, err
	}
	res := &map[string]interface{}{}
	return res, json.Unmarshal(buf, res)
}
