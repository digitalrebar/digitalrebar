package main

import (
	"fmt"
	"net"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/willf/bitset"
)

func TestNewSubnet(t *testing.T) {
	subnet := NewSubnet()

	assert.NotNil(t, subnet.Leases, "Leases must not be nil")
	assert.NotNil(t, subnet.Bindings, "Bindings must not be nil")
	assert.NotNil(t, subnet.Options, "Options must not be nil")
	assert.NotNil(t, subnet.ActiveBits, "ActiveBits must not be nil")
}

func TestFreeLeaseNoMatch(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["one"] = &Lease{}

	s.freeLease(dt, "two")

	assert.NotNil(t, s.Leases["one"], "Lease one should not be nil")
	assert.Nil(t, s.Leases["two"], "Lease two should be nil")
}

// Freeing a lease should clear a set ActiveBit if in range
func TestFreeLeaseMatch(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["one"] = &Lease{}
	s.Leases["two"] = &Lease{
		Ip: s.ActiveStart,
	}
	s.ActiveBits.Set(0)

	s.freeLease(dt, "two")

	assert.NotNil(t, s.Leases["one"], "Lease one should not be nil")
	assert.Nil(t, s.Leases["two"], "Lease two should be nil")
	assert.False(t, s.ActiveBits.Test(0), "First bit should be false")
}

func TestFindInfoNothing(t *testing.T) {
	dt, s := simpleSetup()

	l, b := s.findInfo(dt, "fred")

	assert.Nil(t, l, "Lease should be nil")
	assert.Nil(t, b, "Binding should be nil")
}

func TestFindInfoLeaseButNotBinding(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["fred"] = &Lease{}

	l, b := s.findInfo(dt, "fred")

	assert.NotNil(t, l, "Lease should not be nil")
	assert.Nil(t, b, "Binding should be nil")
}

func TestFindInfoBindingButNoLease(t *testing.T) {
	dt, s := simpleSetup()

	s.Bindings["fred"] = &Binding{}

	l, b := s.findInfo(dt, "fred")

	assert.Nil(t, l, "Lease should be nil")
	assert.NotNil(t, b, "Binding should not be nil")
}

func TestFindInfoLeaseAndBinding(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["fred"] = &Lease{}
	s.Bindings["fred"] = &Binding{}

	l, b := s.findInfo(dt, "fred")

	assert.NotNil(t, l, "Lease should not be nil")
	assert.NotNil(t, b, "Binding should not be nil")
}

func TestFirstClearBitNothing(t *testing.T) {
	bs := bitset.New(2)

	i, s := firstClearBit(bs)
	assert.Equal(t, i, uint(0), "First bit should be 0")
	assert.True(t, s, "Success should be true")
}

func TestFirstClearBitAllSet(t *testing.T) {
	bs := bitset.New(2)

	bs.Set(0)
	bs.Set(1)

	i, s := firstClearBit(bs)
	assert.Equal(t, i, uint(0), "First bit should be 0")
	assert.False(t, s, "Success should be false")
}

func TestFirstClearBitLastBit(t *testing.T) {
	bs := bitset.New(3)

	bs.Set(0)
	bs.Set(1)

	i, s := firstClearBit(bs)
	assert.Equal(t, i, uint(2), "First bit should be 2")
	assert.True(t, s, "Success should be true")
}

func TestRunOutOfIps(t *testing.T) {
	dt, s := simpleSetup()

	for i := 5; i <= 25; i++ {
		id := fmt.Sprintf("fred%d", i)
		l, b := s.findOrGetInfo(dt, id, net.ParseIP("0.0.0.0"))

		assert.NotNil(t, l, "Lease should not be nil")
		assert.Nil(t, b, "Binding should not be nil")

		s.updateLeaseTime(dt, l, 360*time.Second)
	}
	l, b := s.findOrGetInfo(dt, "fred26", net.ParseIP("0.0.0.0"))
	assert.Nil(t, l, "Lease should not be nil")
	assert.Nil(t, b, "Binding should not be nil")
}

func TestReleaseIp(t *testing.T) {
	dt, s := simpleSetup()

	for i := 5; i <= 25; i++ {
		id := fmt.Sprintf("fred%d", i)
		l, b := s.findOrGetInfo(dt, id, net.ParseIP("0.0.0.0"))

		assert.NotNil(t, l, "Lease should not be nil")
		assert.Nil(t, b, "Binding should not be nil")

		s.updateLeaseTime(dt, l, 360*time.Second)
	}

	s.freeLease(dt, "fred10")

	l, b := s.findOrGetInfo(dt, "fred26", net.ParseIP("0.0.0.0"))
	assert.NotNil(t, l, "Lease should not be nil")
	assert.Nil(t, b, "Binding should not be nil")
}
