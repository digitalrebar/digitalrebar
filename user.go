package crowbar

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"

	"github.com/VictorLowther/crowbar-api/datatypes"
	"golang.org/x/crypto/nacl/box"
)

type User struct {
	datatypes.User
	Timestamps
	apiHelper
}

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

func Users() (res []*User, err error) {
	res = make([]*User, 0)
	return res, List("users", &res)
}

func (u *User) StartPasswordReset() (*PasswordChangeToken, error) {
	buf, err := session.request("GET", url(u, "start_password_reset"), nil)
	if err != nil {
		return nil, err
	}
	res := &PasswordChangeToken{}
	return res, json.Unmarshal(buf, res)
}

func (u *User) CompletePasswordReset(tok *PasswordChangeToken, newPassword string) error {
	pubKey, privKey, err := box.GenerateKey(rand.Reader)
	theirPubKey := [32]byte{}
	nonce := [24]byte{}
	nb := nonce[:]
	pk := pubKey[:]
	copy(theirPubKey[:], tok.Token[:])
	cnt, err := rand.Read(nb)
	if err != nil {
		return err
	}
	if cnt != 24 {
		return errors.New("Not enough randomness to encrypt the box")
	}
	payload := `{"password":"` + newPassword + `"}`
	encPayload := box.Seal(nil, []byte(payload), &nonce, &theirPubKey, privKey)
	body := fmt.Sprintf(`{"token":"%v","decoder":"%v","payload":"%v","digest":true}`,
		base64.StdEncoding.EncodeToString(tok.Token),
		base64.StdEncoding.EncodeToString(pk),
		base64.StdEncoding.EncodeToString(encPayload))
	_, err = session.request("POST", url(u, "complete_password_reset"), []byte(body))
	if err != nil {
		return err
	}
	return nil
}
