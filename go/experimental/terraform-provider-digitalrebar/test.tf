resource "digitalrebar_node"  "foo" {
    operating_system = "centos-7.2.1511"
    idle_pool = "system"
    working_pool = "tf"
    access_key = "fakey mcfakedata fake@bad_idea"
}