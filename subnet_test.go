package main

import (
	"github.com/stretchr/testify/assert"
	"github.com/willf/bitset"
	"testing"
)

func TestNewSubnet(t *testing.T) {
	subnet := NewSubnet()

	assert.NotNil(t, subnet.Leases, "Leases must not be nil")
	assert.NotNil(t, subnet.Bindings, "Bindings must not be nil")
	assert.NotNil(t, subnet.Options, "Options must not be nil")
	assert.NotNil(t, subnet.ActiveBits, "ActiveBits must not be nil")
}

func TestMarshalJsonSubnet(t *testing.T) {
	_, s := simpleSetup()

	as := convertSubnetToApiSubnet(s)
	s2, _ := convertApiSubnetToSubnet(as, nil)

	b, e := s2.MarshalJSON()
	assert.Nil(t, e, "Error should be nil")

	s3 := &Subnet{}
	e3 := s3.UnmarshalJSON(b)
	assert.Nil(t, e3, "Error should be nil")
	assert.Equal(t, s3, s2, "Subnet should be equal")
}

func TestFreeLeaseNoMatch(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["one"] = &Lease{}

	s.free_lease(dt, "two")

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

	s.free_lease(dt, "two")

	assert.NotNil(t, s.Leases["one"], "Lease one should not be nil")
	assert.Nil(t, s.Leases["two"], "Lease two should be nil")
	assert.False(t, s.ActiveBits.Test(0), "First bit should be false")
}

func TestFindInfoNothing(t *testing.T) {
	dt, s := simpleSetup()

	l, b := s.find_info(dt, "fred")

	assert.Nil(t, l, "Lease should be nil")
	assert.Nil(t, b, "Binding should be nil")
}

func TestFindInfoLeaseButNotBinding(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["fred"] = &Lease{}

	l, b := s.find_info(dt, "fred")

	assert.NotNil(t, l, "Lease should not be nil")
	assert.Nil(t, b, "Binding should be nil")
}

func TestFindInfoBindingButNoLease(t *testing.T) {
	dt, s := simpleSetup()

	s.Bindings["fred"] = &Binding{}

	l, b := s.find_info(dt, "fred")

	assert.Nil(t, l, "Lease should be nil")
	assert.NotNil(t, b, "Binding should not be nil")
}

func TestFindInfoLeaseAndBinding(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["fred"] = &Lease{}
	s.Bindings["fred"] = &Binding{}

	l, b := s.find_info(dt, "fred")

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
