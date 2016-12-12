package main

import (
	"fmt"
	"strings"
	"time"

	dr "github.com/digitalrebar/digitalrebar/go/rebar-api/api"
	drtypes "github.com/digitalrebar/digitalrebar/go/rebar-api/datatypes"
	"github.com/hashicorp/terraform/helper/schema"
	"github.com/hashicorp/terraform/plugin"
	"github.com/hashicorp/terraform/terraform"
)

func provider() terraform.ResourceProvider {
	return &schema.Provider{
		Schema: map[string]*schema.Schema{
			"key": &schema.Schema{
				Type:        schema.TypeString,
				Required:    true,
				DefaultFunc: schema.EnvDefaultFunc("REBAR_KEY", nil),
				Description: "The username:password pair for API operations",
			},
			"endpoint": &schema.Schema{
				Type:        schema.TypeString,
				Required:    true,
				DefaultFunc: schema.EnvDefaultFunc("REBAR_ENDPOINT", nil),
				Description: "The API endpoint to talk to",
			},
		},
		ResourcesMap: map[string]*schema.Resource{
			"digitalrebar_node": &schema.Resource{
				Create: nodeGrab,
				Delete: nodeRelease,
				Read:   nodeRead,
				Schema: map[string]*schema.Schema{
					"name": &schema.Schema{
						Type:     schema.TypeString,
						Computed: true,
					},
					"operating_system": &schema.Schema{
						Type:     schema.TypeString,
						Required: true,
						ForceNew: true,
					},
					"idle_pool": &schema.Schema{
						Type:     schema.TypeString,
						Required: true,
						ForceNew: true,
					},
					"working_pool": &schema.Schema{
						Type:     schema.TypeString,
						Required: true,
						ForceNew: true,
					},
					"control_address": &schema.Schema{
						Type:     schema.TypeString,
						Computed: true,
					},
					"access_key": &schema.Schema{
						Type:     schema.TypeString,
						Required: true,
						ForceNew: true,
					},
					"created": &schema.Schema{
						Type:     schema.TypeString,
						Computed: true,
					},
					"updated": &schema.Schema{
						Type:     schema.TypeString,
						Computed: true,
					},
				},
			},
		},
		ConfigureFunc: func(r *schema.ResourceData) (interface{}, error) {
			up := strings.SplitN(r.Get("key").(string), ":", 2)
			endpoint := r.Get("endpoint").(string)
			return nil, dr.Session(endpoint, up[0], up[1])
		},
	}
}

func nodeRead(d *schema.ResourceData, meta interface{}) error {
	node := &dr.Node{}
	if err := dr.Fetch(node, d.Id()); err != nil {
		return fmt.Errorf("Unable to fetch node %s: %v", d.Id(), err)
	}
	d.Set("name", node.Name)
	d.Set("control_address", node.CtrlAddr)
	d.Set("created", node.CreatedAt.String())
	d.Set("updated", node.UpdatedAt.String())
	d.SetConnInfo(map[string]string{
		"type": "ssh",
		"host": node.CtrlAddr,
	})
	return nil
}

// cleanNode cleans stale noderoles off the passed node.
// A stale noderole is one that was bound into the working pool.
func cleanNode(node *dr.Node) error {
	// Remove all the noderoles bound to this node in the workingPool.
	toRemove := []*dr.NodeRole{}
	tmpl := &dr.NodeRole{}
	matcher := map[string]interface{}{
		"node_id":       node.ID,
		"deployment_id": node.DeploymentID,
	}
	if err := dr.Match(tmpl.ApiPath(), matcher, &toRemove); err != nil {
		return fmt.Errorf("Failed to fetch noderoles to remove from %s: %v", node.Name, err)
	}
	for i := len(toRemove); i > 0; i-- {
		dr.Destroy(toRemove[i-1])
	}
	return nil
}

func getPools(d *schema.ResourceData) (idlePool, workingPool *dr.Deployment, err error) {
	idlePoolID, ok := d.Get("idle_pool").(string)
	if !ok {
		return nil, nil, fmt.Errorf("idle_pool %v not a String", d.Get("idle_pool"))
	}
	workingPoolID, ok := d.Get("working_pool").(string)
	if !ok {
		return nil, nil, fmt.Errorf("working_pool %v not a String", d.Get("working_pool"))
	}
	idlePool = &dr.Deployment{}
	workingPool = &dr.Deployment{}
	if err = dr.Fetch(idlePool, idlePoolID); err != nil {
		return nil, nil, fmt.Errorf("Idle pool %s not found: %v", idlePoolID, err)
	}
	if err = dr.Fetch(workingPool, workingPoolID); err != nil {
		return nil, nil, fmt.Errorf("Working pool %s not found: %v", workingPoolID, err)
	}
	if workingPool.State == drtypes.DeploymentProposed {
		return nil, nil, fmt.Errorf("Working pool %s proposed, no progress will be made. Commit it first.", workingPool.Name)
	}
	return idlePool, workingPool, err
}

func nodeGrab(d *schema.ResourceData, meta interface{}) error {
	idlePool, workingPool, err := getPools(d)
	if err != nil {
		return err
	}
	nodes, err := dr.Nodes(idlePool)
	if err != nil {
		return fmt.Errorf("Could not get list of idle nodes: %v", err)
	}

	// See if the OS is available to target.
	osTarget := d.Get("operating_system").(string)
	sysDepl := &dr.Deployment{}
	sysDepl.Name = "system"
	availableOsesAttr, err := dr.FetchAttrib(sysDepl, "provisioner-available-oses", "")
	if err != nil {
		return fmt.Errorf("Unable to fetch list of available operating systems: %v", err)
	}
	availableOses := availableOsesAttr.Value.(map[string]interface{})
	if _, ok := availableOses[osTarget]; !ok {
		return fmt.Errorf("OS %s not available to provision.  Try one of %#v.", osTarget, availableOses)
	}

	// We want to iterate over the nodes in random order, so stuff them in a map.
	var target *dr.Node
	nodemap := map[string]*dr.Node{}
	for _, n := range nodes {
		if !n.System {
			nodemap[n.Name] = n
		}
	}

	for _, n := range nodemap {
		if n.Move(workingPool) == nil {
			target = n
			break
		}
	}
	if target == nil {
		return fmt.Errorf("No idle nodes to provision")
	}
	d.SetId(target.UUID)

	if err := cleanNode(target); err != nil {
		return err
	}

	// Arrange to have an OS installed on the node.
	targetRole := &dr.Role{}
	if err := dr.Fetch(targetRole, "rebar-installed-node"); err != nil {
		return fmt.Errorf("Failed to fetch rebar-installed-node milestone: %v", err)
	}

	nodeRole := &dr.NodeRole{}
	dr.Init(nodeRole)
	nodeRole.NodeID = target.ID
	nodeRole.RoleID = targetRole.ID
	nodeRole.DeploymentID = workingPool.ID
	if err := dr.BaseCreate(nodeRole); err != nil {
		return fmt.Errorf("Failed to bind %s to %s: %v", targetRole.Name, target.Name, err)
	}
	targetOS, err := dr.FetchAttrib(target, "provisioner-target_os", "")
	if err != nil {
		return fmt.Errorf("Failed to fetch provisioner-target_os from %s: %v", target.Name, err)
	}
	targetOS.Value = osTarget
	if err := dr.SetAttrib(target, targetOS, ""); err != nil {
		return fmt.Errorf("Failed to set target OS to %v: %v", targetOS.Value, err)
	}
	// Add the passed-in SSH public key to allow passwordless root access
	accessKeys, err := dr.FetchAttrib(target, "rebar-access_keys", "")
	if err != nil {
		return fmt.Errorf("Failed to fetch SSH access keys for %s: %v", target.Name, err)
	}
	keys, ok := accessKeys.Value.(map[string]interface{})
	if !ok {
		return fmt.Errorf("Stored keys are not in the proper format on %s: %v", target.Name, accessKeys.Value)
	}
	keys["terraform-0"] = d.Get("access_key")
	accessKeys.Value = keys
	if err := dr.SetAttrib(target, accessKeys, ""); err != nil {
		return fmt.Errorf("Failed to add access key for Terraform to %s: %v", target.Name, err)
	}
	// Commit the node and wait for it to finish deploying
	if err := dr.Commit(target); err != nil {
		return fmt.Errorf("Failed to commit %s: %v", target.Name, err)
	}
	for {
		time.Sleep(10 * time.Second)
		if err := dr.Read(target); err != nil {
			return fmt.Errorf("Failed to get current node status for %s: %v", target.Name, err)
		}
		if target.Bootenv != "local" {
			continue
		}
		nodeRoles, err := dr.NodeRoles(target)
		if err != nil {
			return fmt.Errorf("Failed to fetch noderoles on %s to check status: %v", target.Name, err)
		}
		active := 0
		for _, nr := range nodeRoles {
			switch nr.State {
			case -1:
				return fmt.Errorf("Node %s failed to converge, check Digital Rebar: %v", target.Name, err)
			case 0:
				active++
			}
		}
		if active == len(nodeRoles) {
			break
		}
	}
	return nodeRead(d, meta)
}

func nodeRelease(d *schema.ResourceData, meta interface{}) error {
	idlePool, _, err := getPools(d)
	if err != nil {
		return err
	}
	nodeUUID := d.Id()
	target := &dr.Node{}
	if err := dr.Fetch(target, nodeUUID); err != nil {
		return fmt.Errorf("Failed to fetch node %s: %v", nodeUUID, err)
	}

	// Clean off the node
	if err := cleanNode(target); err != nil {
		return fmt.Errorf("Error cleaning node %s: %v", target.Name, err)
	}

	// Remove the terraform SSH key
	accessKeys, err := dr.FetchAttrib(target, "rebar-access_keys", "")
	if err != nil {
		return fmt.Errorf("Failed to fetch SSH access keys for %s: %v", target.Name, err)
	}
	keys, ok := accessKeys.Value.(map[string]interface{})
	if !ok {
		return fmt.Errorf("Stored keys are not in the proper format on %s: %v", target.Name, accessKeys.Value)
	}
	delete(keys, "terraform-0")
	accessKeys.Value = keys
	if err := dr.SetAttrib(target, accessKeys, ""); err != nil {
		return fmt.Errorf("Failed to remove access key for Terraform to %s: %v", target.Name, err)
	}

	// Have the node boot back into Sledgehammer
	target.Bootenv = "sledgehammer"
	if err := dr.Update(target); err != nil {
		return fmt.Errorf("Failed to set %s to boot into Sledgehammer: %v", target.Name, err)
	}
	caughtUp := false
	for i := 0; i < 30; i++ {
		time.Sleep(10 * time.Second)
		bootstate := target.ActiveBootstate()
		if bootstate == target.Bootenv {
			caughtUp = true
			break
		}
	}
	if !caughtUp {
		return fmt.Errorf("Node %s bootenv stuck at %s", target.Name, target.Bootenv)
	}
	if target.Power("off") == nil {
		// Wait for node to turn off
		for target.Alive {
			time.Sleep(10 * time.Second)
			if err := dr.Update(target); err != nil {
				return fmt.Errorf("Failed to get node %s status: %v", target.Name, err)
			}
		}
	} else if err := target.Power("reboot"); err == nil {
		// Wait for node to turn off
		for target.Alive {
			time.Sleep(10 * time.Second)
			if err := dr.Update(target); err != nil {
				return fmt.Errorf("Failed to get node %s status: %v", target.Name, err)
			}
		}
		// Wait for node to turn off
		for !target.Alive {
			time.Sleep(10 * time.Second)
			if err := dr.Update(target); err != nil {
				return fmt.Errorf("Failed to get node %s status: %v", target.Name, err)
			}
		}
	} else {
		return fmt.Errorf("Cannot turn off or reboot node %s: %v", target.Name, err)
	}
	if err := target.Move(idlePool); err != nil {
		return fmt.Errorf("Failed to move node %s back to idle pool %s: %v", target.Name, idlePool.Name, err)
	}
	return nil
}

func main() {
	plugin.Serve(&plugin.ServeOpts{
		ProviderFunc: provider,
	})
}
