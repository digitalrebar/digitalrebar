package main

import (
	dhcp "github.com/krolaw/dhcp4"
	"github.com/stretchr/testify/assert"
	"net"
	"net/http"
	"testing"
)

func newSubnet(dt *DataTracker, name, subnet string) (s *Subnet) {
	_, theNet, _ := net.ParseCIDR(subnet)

	s = NewSubnet()
	s.Name = name
	s.Subnet = &MyIPNet{theNet}
	s.ActiveStart = dhcp.IPAdd(theNet.IP, 5)
	s.ActiveEnd = dhcp.IPAdd(theNet.IP, 25)
	return
}

func addNewSubnet(dt *DataTracker, name, subnet string) (s *Subnet, err error, code int) {
	s = newSubnet(dt, name, subnet)
	err, code = dt.AddSubnet(s)
	return
}

func simpleSetup() (dt *DataTracker, s *Subnet) {
	dt = NewDataTracker(".")
	s, _, _ = addNewSubnet(dt, "fred", "192.168.128.0/24")
	dt.AddSubnet(s)
	return
}

func TestAddSubnet(t *testing.T) {
	dt, s := simpleSetup()

	assert.Equal(t, len(dt.Subnets), 1, "Subnet should be 1")
	assert.Equal(t, dt.Subnets["fred"], s, "Subnet should be fred")
}

func TestAddSubnetExisting(t *testing.T) {
	dt, _ := simpleSetup()

	_, err, code := addNewSubnet(dt, "fred", "192.168.124.0/24")
	assert.Equal(t, len(dt.Subnets), 1, "subnets should be len 1")
	assert.Equal(t, code, http.StatusConflict, "Return code should be conflict, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Already exists", "Error message should be 'Already exists', but was %s", err.Error())
}

func TestAddSubnetOverlap(t *testing.T) {
	dt, _ := simpleSetup()

	_, err, code := addNewSubnet(dt, "fred2", "192.168.128.0/24")
	assert.Equal(t, len(dt.Subnets), 1, "subnets should be len 1")
	assert.Equal(t, code, http.StatusBadRequest, "Return code should be bad status, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Subnet overlaps with existing subnet", "Error message should be 'Subnet overlaps with existing subnet', but was %s", err.Error())
}

func TestRemoveSubnetMissing(t *testing.T) {
	dt, _ := simpleSetup()

	err, code := dt.RemoveSubnet("missing")

	assert.Equal(t, code, http.StatusNotFound, "Code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Not Found", "Error message should be 'Not Found', but was %s", err.Error())
}

func TestRemoveSubnet(t *testing.T) {
	dt, s := simpleSetup()

	err, code := dt.RemoveSubnet(s.Name)

	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, code, http.StatusOK, "Code should be 200, but is: %d", code)
	assert.Equal(t, len(dt.Subnets), 0, "Subnets length should be 0, but is %d", len(dt.Subnets))
}

func TestReplaceSubnetNotFound(t *testing.T) {
	dt, _ := simpleSetup()

	ns := newSubnet(dt, "fred", "192.168.124.0/24")

	err, code := dt.ReplaceSubnet("fred2", ns)
	assert.Equal(t, code, http.StatusNotFound, "Return code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Not Found", "Error message should be 'Not Found', but was %s", err.Error())
}

func TestReplaceSubnetReplace(t *testing.T) {
	dt, _ := simpleSetup()

	ns := newSubnet(dt, "fred", "192.168.124.0/24")

	err, code := dt.ReplaceSubnet("fred", ns)
	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, ns, dt.Subnets["fred"], "Replaced new subnet should the new one")
	assert.Equal(t, dt.Subnets["fred"].Subnet.String(), "192.168.124.0/24", "Subnet should be 192.168.124.0/24, but is %s", dt.Subnets["fred"].Subnet.String())
}

func TestReplaceSubnetRename(t *testing.T) {
	dt, _ := simpleSetup()

	ns := newSubnet(dt, "fred2", "192.168.128.0/24")

	err, code := dt.ReplaceSubnet("fred", ns)
	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Nil(t, dt.Subnets["fred"], "Old slot should be nil")
	assert.Equal(t, dt.Subnets["fred2"], ns, "Replaced new subnet should the new one")
	assert.Equal(t, dt.Subnets["fred2"].Subnet.String(), "192.168.128.0/24", "Subnet should be 192.168.128.0/24, but is %s", dt.Subnets["fred2"].Subnet.String())
}

func TestReplaceSubnetPreserveInfo(t *testing.T) {
	dt, s := simpleSetup()

	s.Leases["greg"] = &Lease{
		Mac: "macit",
	}

	s.Bindings["greg"] = &Binding{
		Mac: "macit",
	}

	ns := newSubnet(dt, "fred2", "192.168.128.0/24")

	err, code := dt.ReplaceSubnet("fred", ns)
	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Nil(t, dt.Subnets["fred"], "Old slot should be nil")
	assert.Equal(t, dt.Subnets["fred2"], ns, "Replaced new subnet should the new one")
	assert.Equal(t, dt.Subnets["fred2"].Subnet.String(), "192.168.128.0/24", "Subnet should be 192.168.128.0/24, but is %s", dt.Subnets["fred2"].Subnet.String())
	assert.NotNil(t, dt.Subnets["fred2"].Leases["greg"], "Leases['greg'] should not be nil")
	assert.NotNil(t, dt.Subnets["fred2"].Bindings["greg"], "Bindings['greg'] should not be nil")
	assert.Equal(t, dt.Subnets["fred2"].Leases["greg"].Mac, "macit", "Leases['greg'].Mac should be 'macit', but is %s", dt.Subnets["fred2"].Leases["greg"].Mac)
	assert.Equal(t, dt.Subnets["fred2"].Bindings["greg"].Mac, "macit", "Bindings['greg'].Mac should be 'macit', but is %s", dt.Subnets["fred2"].Bindings["greg"].Mac)
}

func TestReplaceSubnetMustNotOverlap(t *testing.T) {
	dt, _ := simpleSetup()
	os, _, _ := addNewSubnet(dt, "fred2", "192.168.124.0/24")

	ns := newSubnet(dt, "fred2", "192.168.128.0/24")

	err, code := dt.ReplaceSubnet("fred2", ns)
	assert.Equal(t, code, http.StatusBadRequest, "Return code should be bad request, but is %d", code)
	assert.NotNil(t, err, "Error should be not nil")
	assert.Equal(t, err.Error(), "Subnet overlaps with existing subnet", "Error should be 'Subnet overlaps with existing subnet', but is %s", err.Error())
	assert.NotNil(t, dt.Subnets["fred2"], "fred2 slot should not be nil")
	assert.Equal(t, dt.Subnets["fred2"], os, "Should not replace the old subnet")
}

func TestFindSubnetEmpty(t *testing.T) {
	dt := NewDataTracker(".")

	s := dt.FindSubnet(net.ParseIP("0.0.0.0"))
	assert.Nil(t, s, "Expected nil subnets")
}

func TestFindSubnetWithSubnetNotMatched(t *testing.T) {
	dt, _ := simpleSetup()

	s := dt.FindSubnet(net.ParseIP("0.0.0.0"))
	assert.Nil(t, s, "Expected nil subnets")
}

func TestFindSubnetWithSubnetMatches(t *testing.T) {
	dt, s := simpleSetup()

	ns := dt.FindSubnet(net.ParseIP("192.168.128.0"))
	assert.Equal(t, s, ns, "Expected to find subnet")
	ns = dt.FindSubnet(net.ParseIP("192.168.128.255"))
	assert.Equal(t, s, ns, "Expected to find subnet")
	ns = dt.FindSubnet(net.ParseIP("192.168.128.124"))
	assert.Equal(t, s, ns, "Expected to find subnet")
}

func TestMarshallingMyIPNet(t *testing.T) {
	_, netdata, _ := net.ParseCIDR("192.168.124.0/24")
	myipnet := &MyIPNet{netdata}

	b, err := myipnet.MarshalText()
	assert.Equal(t, string(b[:len(b)]), "192.168.124.0/24", "Should be 192.168.124.0/24, but is %s", string(b[:len(b)]))
	assert.Nil(t, err, "Err should be nil")

	myunipnet := &MyIPNet{}
	err = myunipnet.UnmarshalText(b)
	assert.Equal(t, myunipnet.String(), "192.168.124.0/24", "Should be 192.168.124.0/24, but is %s", myunipnet.String())
}

// GREG: Test load/save data

func TestSubnetOverlapInside(t *testing.T) {
	dt, _ := simpleSetup()
	_, err, code := addNewSubnet(dt, "fred2", "192.168.128.128/25")

	assert.Equal(t, code, http.StatusBadRequest, "Return code should be bad status, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Subnet overlaps with existing subnet", "Error message should be 'Subnet overlaps with existing subnet', but was %s", err.Error())
}

func TestSubnetOverlapOutside(t *testing.T) {
	dt, _ := simpleSetup()
	_, err, code := addNewSubnet(dt, "fred2", "192.168.0.0/16")

	assert.Equal(t, code, http.StatusBadRequest, "Return code should be bad status, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Subnet overlaps with existing subnet", "Error message should be 'Subnet overlaps with existing subnet', but was %s", err.Error())
}

func TestAddBindingMissing(t *testing.T) {
	dt, s := simpleSetup()
	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")

	err, code := dt.AddBinding("fred2", *b)
	assert.Equal(t, code, http.StatusNotFound, "Return code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Not Found", "Error message should be 'Not Found', but was %s", err.Error())
	assert.Equal(t, len(s.Bindings), 0, "There should not be any bindings")
}

func TestAddBinding(t *testing.T) {
	dt, s := simpleSetup()

	assert.Equal(t, s.ActiveBits.Any(), false, "No bits should be set")

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")

	err, code := dt.AddBinding("fred", *b)
	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, len(s.Bindings), 1, "There should be one binding")
	assert.NotNil(t, s.Bindings["macit"], "Binding should be not nil")
	assert.Equal(t, s.Bindings["macit"], b, "Binding should be the same")
	assert.Equal(t, s.Bindings["macit"].Ip.String(), "192.168.128.10", "Binding ip should be 192.168.128.10, but is %s", s.Bindings["macit"].Ip.String())
	assert.Equal(t, s.ActiveBits.Count(), uint(1), "bit count should be 1, but is %d", s.ActiveBits.Count())
	assert.Equal(t, s.ActiveBits.Test(5), true, "bit 5 should be set")
}

func TestAddBindingReplace(t *testing.T) {
	dt, s := simpleSetup()

	assert.Equal(t, s.ActiveBits.Any(), false, "No bits should be set")

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")
	dt.AddBinding("fred", *b)

	assert.Equal(t, s.ActiveBits.Test(5), true, "bit 5 should be set")

	b2 := NewBinding()
	b2.Mac = "macit"
	b2.Ip = net.ParseIP("192.168.128.16")
	err, code := dt.AddBinding("fred", *b2)

	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, len(s.Bindings), 1, "There should be one binding")
	assert.NotNil(t, s.Bindings["macit"], "Binding should be not nil")
	assert.Equal(t, s.Bindings["macit"], b2, "Binding should be the same")
	assert.Equal(t, s.Bindings["macit"].Ip.String(), "192.168.128.16", "Binding ip should be 192.168.128.16, but is %s", s.Bindings["macit"].Ip.String())
	assert.Equal(t, s.ActiveBits.Count(), uint(1), "bit count should be 1, but is %d", s.ActiveBits.Count())
	assert.Equal(t, s.ActiveBits.Test(5), false, "bit 5 should be false")
	assert.Equal(t, s.ActiveBits.Test(11), true, "bit 11 should be true")
}

func TestDeleteBindingMissingSubnet(t *testing.T) {
	dt, s := simpleSetup()

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")
	dt.AddBinding("fred", *b)

	err, code := dt.DeleteBinding("fred2", "macit")

	assert.Equal(t, code, http.StatusNotFound, "Return code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Subnet Not Found", "Error message should be 'Subnet Not Found', but was %s", err.Error())
	assert.Equal(t, len(s.Bindings), 1, "There should be one binding")
	assert.Equal(t, s.ActiveBits.Count(), uint(1), "bit count should be 1, but is %d", s.ActiveBits.Count())
	assert.Equal(t, s.ActiveBits.Test(5), true, "bit 5 should be true")
}

func TestDeleteBindingMissingBinding(t *testing.T) {
	dt, s := simpleSetup()

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")
	dt.AddBinding("fred", *b)

	err, code := dt.DeleteBinding("fred", "missing")

	assert.Equal(t, code, http.StatusNotFound, "Return code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Binding Not Found", "Error message should be 'Binding Not Found', but was %s", err.Error())
	assert.Equal(t, len(s.Bindings), 1, "There should one binding")
	assert.Equal(t, s.ActiveBits.Count(), uint(1), "bit count should be 1, but is %d", s.ActiveBits.Count())
	assert.Equal(t, s.ActiveBits.Test(5), true, "bit 5 should be true")
}

func TestDeleteBinding(t *testing.T) {
	dt, s := simpleSetup()

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")
	dt.AddBinding("fred", *b)

	err, code := dt.DeleteBinding("fred", "macit")
	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, len(s.Bindings), 0, "There should not be any bindings")

	assert.Equal(t, s.ActiveBits.Count(), uint(0), "bit count should be 0, but is %d", s.ActiveBits.Count())
	assert.Equal(t, s.ActiveBits.Test(5), false, "bit 5 should be false")
}

func TestSetNextServerMissing(t *testing.T) {
	dt, _ := simpleSetup()

	ip := net.ParseIP("192.168.128.10")
	nextServer := NextServer{
		Server: "1.1.1.1",
	}

	err, code := dt.SetNextServer("fred2", ip, nextServer)

	assert.Equal(t, code, http.StatusNotFound, "Return code should be not found, but is %d", code)
	assert.NotNil(t, err, "Error should not be nil")
	assert.Equal(t, err.Error(), "Not Found", "Error message should be 'Not Found', but was %s", err.Error())
}

func TestSetNextServer(t *testing.T) {
	dt, s := simpleSetup()

	b := NewBinding()
	b.Mac = "macit"
	b.Ip = net.ParseIP("192.168.128.10")
	dt.AddBinding("fred", *b)

	ip := net.ParseIP("192.168.128.10")
	nextServer := NextServer{
		Server: "1.1.1.1",
	}

	err, code := dt.SetNextServer("fred", ip, nextServer)

	b = s.Bindings["macit"]

	assert.Equal(t, code, http.StatusOK, "Return code should be ok, but is %d", code)
	assert.Nil(t, err, "Error should be nil")
	assert.Equal(t, *b.NextServer, "1.1.1.1", "Next server should be 1.1.1.1, but is %s", b.NextServer)
}
