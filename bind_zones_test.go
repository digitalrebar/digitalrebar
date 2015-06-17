package main

import "testing"

func TestMakeRevName(t *testing.T) {
	answer := makeRevName("A", "1.2.3.4")
	if answer != "4.3.2.1.in-addr.arpa" {
		t.Error("Expected 4.3.2.1.in-addr.arpa, but got " + answer)
	}

	answer = makeRevName("AAAA", "123:4567:89ab:cdef:edc:ba98:7654:3210")
	if answer != "0.1.2.3.4.5.6.7.8.9.a.b.c.d.e.0.f.e.d.c.b.a.9.8.7.6.5.4.3.2.1.0.ip6.arpa" {
		t.Error("Expected 0.1.2.3.4.5.6.7.8.9.a.b.c.d.e.0.f.e.d.c.b.a.9.8.7.6.5.4.3.2.1.0.ip6.arpa, but got " + answer)
	}
}
