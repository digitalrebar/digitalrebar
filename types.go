package crowbar

// Apache 2 License 2015 by Rob Hirschfeld for RackN

import (
	"fmt"
)

type CrowbarDigest struct {
	CSRFToken string `json:"csrf_token"`
	Message   string `json:"message"`
}

type Error struct {
	Message   string `json:"message"`
	Status    int    `json:"status"`
	Backtrace string `json:"backtrace"`
}

func (e *Error) Error() string {
	return fmt.Sprintf("Status: %v\nMessage: %v\n Backtrace: %v")
}

type NodePower struct {
	ID     int64  `json:"id"`
	Action string `json:"action"`
	Result string `json:"result"`
}

type NodeAddress struct {
	Node      string   `json:"node"`
	Network   string   `json:"network"`
	Category  string   `json:"category"`
	Addresses []string `json:"addresses"`
}
