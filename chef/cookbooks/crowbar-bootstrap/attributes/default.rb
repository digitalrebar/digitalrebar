default["bootstrap"]["tracedir"] = "/var/cache/crowbar-bootstrap"
default["bootstrap"]["crowbar_yml"]= "/opt/opencrowbar/core/crowbar.yml"
default["bootstrap"]["sledgehammer"]["url"] = "http://opencrowbar.s3-website-us-east-1.amazonaws.com/sledgehammer"
#default["bootstrap"]["sledgehammer"]["signature"] = "0f61af2f6be9288d5529e15aa223e036730a8387"
# Includes lldpd for link layer switch detection.
default["bootstrap"]["sledgehammer"]["signature"] = "6bce86d4f5c50f8be7493f2d9ec23075845ea4b9"
default["bootstrap"]["tftproot"] = "/tftpboot"
default["bootstrap"]["gopath"] = "/root/go"
default["bootstrap"]["gover"] = "1.4"
default["bootstrap"]["goiardi"]["repo"] = "github.com/ctdk/goiardi"
default["bootstrap"]["goiardi"]["port"] = 4646
default["bootstrap"]["goiardi"]["protocol"] = "http"
default["bootstrap"]["sws"] = "github.com/VictorLowther/sws"
default["bootstrap"]["openwsman"]["repo"]="https://github.com/openwsman/openwsman"
default["bootstrap"]["openwsman"]["version"]="v2.4.12"
default["bootstrap"]["grub2"]["repo"]="https://github.com/VictorLowther/grub"
default["bootstrap"]["grub2"]["bootfont"]="http://mirrors2.kernel.org/opensuse/factory/repo/oss/boot/x86_64/grub2-efi/unicode.pf2"
default["bootstrap"]["grub2"]["version"]="allow-ports-in-protocols"
default["bootstrap"]["grub2"]["platforms"]={
  "efi" => {
    cmd: "--format=x86_64-efi --prefix='(tftp)/boot/grub' tftp http efinet net",
    file: "grub-x86_64.efi"
  },
  "pc" => {
    cmd: "--format=i386-pc-pxe --prefix='(pxe)/boot/grub' pxe tftp http net pxechain",
    file: "grub.pxe"
  }
}
