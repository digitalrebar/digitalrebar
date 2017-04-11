#!/bin/bash

die() {
    printf '%s\n' "$@"
    exit 1
}

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
# Commands we have to run under sudo -n
SUDO_CMDS="/sbin/brctl ip umount mount"
# Commands we need to run as $USER
NEEDED_CMDS="screen qemu-img sudo"

# Find out what KVM is called locally.
for KVM in kvm qemu-kvm qemu-system-x86_64 ''; do
    type $KVM &>/dev/null && break
done

[[ $KVM ]] || die "Cannot find kvm!  Are you sure it is installed?"

if ! [[ -c /dev/kvm && -w /dev/kvm ]]; then
    echo "Please make sure that /dev/kvm exists, and that"
    echo "$USER has write permissions on it."
    exit 1
fi

for cmd in $NEEDED_CMDS $SUDO_CMDS; do
    which $cmd &>/dev/null && continue
    echo "Missing required command $cmd (or it is not in \$PATH)."
    echo "Please make sure the following commands are installed:"
    echo "$SUDO_CMDS"
    echo "$NEEDED_CMDS"
    exit 1
done

for cmd in $SUDO_CMDS; do
    sudo -l -n "$(which $cmd)" &>/dev/null && continue
    echo "$USER is not allowed to run $(which $cmd) using sudo."
    echo "Please make sure that $USER has passwordless sudo rights to run:"
    printf "%s " $(for cmd in $SUDO_CMDS; do which "$cmd"; done)
    echo
    exit 1
done

# Amount of memory to dedicate to admin and slave nodes.
OCB_MEM=4G

# CPUs to give admin and slave nodes
OCB_CPUS=4
VMID=$$

# IP address of the node that ends up as the admin node.
OCB_ADMIN_IP=192.168.124.10/24
OCB_BRIDGE_IP=192.168.124.1/24
REBAR_KEY="rebar:rebar1"

ADMIN_HOSTNAMES=("cr0wbar.pwns.joo"
    "vltima.ratio.regvm"
    "omnia.fines.bon"
    "admin.smoke.test"
    "eheyeh.asher.eheyeh"
    "bork.bork.bork")

ADMIN_HOSTNAME=${ADMIN_HOSTNAMES[$(($RANDOM % ${#ADMIN_HOSTNAMES[@]}))]}
#debug "Picked $ADMIN_HOSTNAME"
export OCB_DOMAIN=${ADMIN_HOSTNAME#*.}

: ${OCB_BRIDGE:="rebar-br"}
NICS_PER_VM=3
VM_DIR="$HOME/.cache/digitalrebar/vms"

mkdir -p "$VM_DIR"

die() {
    echo "$*" >&2
    exit 1
}

bridge_exists() [[ -d /sys/class/net/$1/brif ]]

update_vm_status() {
    # $1 = Status update
    local current_date=$(date '+%F %T %z')
    echo "$current_date: $VMID - $1"
    {
        flock 66
        echo "$current_date: $1" >> "$VM_DIR/$VMID.status"
    } 66>"$VM_DIR/.$VMID.status.lck"
}

make_rebar_bridge() {
    sudo -n brctl show |grep -q "$OCB_BRIDGE" || \
        sudo -n brctl addbr "$OCB_BRIDGE" || \
        die "Could not create $OCB_BRIDGE bridge!"
    # Emulate a switch with STP but no portfast.
    sudo -n brctl stp "$OCB_BRIDGE" on || \
        die "Could not enable spanning tree protocol on $OCB_BRIDGE!"
    sudo -n brctl setfd "$OCB_BRIDGE" 2 || \
        die "Could not set forwarding time on $OCB_BRIDGE!"
    sudo -n brctl sethello "$OCB_BRIDGE" 1 || \
        die "Could not set hello time for $OCB_BRIDGE!"
    sudo -n ip link set "$OCB_BRIDGE" up || \
        die "Could not set link on $OCB_BRIDGE up!"
    sudo -n ip addr add "$OCB_BRIDGE_IP" dev "$OCB_BRIDGE"
}

kill_rebar_bridge() {
    sudo -n ip addr flush dev "$OCB_BRIDGE"
    sudo -n ip link set "$OCB_BRIDGE" down
    sudo -n brctl delbr "$OCB_BRIDGE"
}

# Simple unique mac address generation.
MACNR=0
getmac() {
    MACADDR=$(printf "52:54:%02x:%02x:%02x:%02x" $(($VMID/256)) $(($VMID%256)) $((MACNR/256)) $((MACNR%256)))
    MACNR=$((MACNR + 1))
    :
}

# Make a tap interface, and attach it to the right bridge.
maketap() {
    # $1 = preferred device name
    # $2 = bridge to attach it to.

    # preemptively arrange to clean up.
    sudo -n ip tuntap add dev "$1" mode tap || \
        die "Could not create tap device $1"
    sudo -n ip link set "$1" up || \
        die "Could not bring link on tap device $1 up!"
    sudo -n brctl addif "$2" "$1" || \
        die "Could not add tap $1 to bridge $2!"
}

# Remove a tap interface we created.
killtap() {
    set +e
    local res_re='(does not exist|Cannot find device)'
    # $1 = device to kill
    # $2 = bridge to detach it from
    while ! [[ $(sudo -n ip link show "$1" 2>&1) =~ $res_re ]]; do
        sudo -n brctl delif "$2" "$1"
        sudo -n ip link set "$1" down
        sudo -n ip tuntap del dev "$1" mode tap
    done
}

# Make the nics for a VM.  This creates the tap interfaces
# and sets vm_nics to the array of macaddr,nic_name pairs.
vm_nics=()
makenics(){
    local idx
    for ((idx=0; idx < NICS_PER_VM; idx++));  do
        local nic_name="vm-$VMID-${idx}"
        maketap "$nic_name" "$OCB_BRIDGE"
        getmac
        if [ $idx == 0 ]; then
            local vm_name="d${MACADDR//:/-}"
            echo == TO PRECREATE NODE, run these steps on the admin node:
            echo export MY_FQDN="\"\$(hostname)\""
            echo export DOMAINNAME="\${MY_FQDN#*.}"
            echo rebar nodes create "\"{\\\"name\\\":\\\"$vm_name.\$DOMAINNAME\\\",\\\"mac\\\":\\\"$MACADDR\\\"}\""
            echo rebar roles bind rebar-managed-node to \"$vm_name.\$DOMAINNAME\"
            echo rebar nodes commit \"$vm_name.\$DOMAINNAME\"
        fi
        vm_nics+=("$MACADDR,$nic_name")
    done
}

killnics(){
    local nic
    for nic in "${vm_nics[@]}"; do
        killtap "${nic##*,}" "$OCB_BRIDGE"
    done
}

cleanup() {
    set +e
    kill_vm killed
    killnics
    rm -rf "$VM_DIR/$VMID"*
}

# Kill a running VM.
kill_vm() (
    # $1 = state to assign, defaults to killed.
    set +e
    local killsig=TERM killtries=0 killstate="${1:-killed}"
    # If there is no PID file, assume that the VM is already dead.
    [[ -f $VM_DIR/$VMID.pid ]] || {
        > "$VM_DIR/$VMID.$killstate"
        return 0
    }
    local pid=$(cat "$VM_DIR/$VMID.pid")
    # If there is no entry for the VM process in /proc/ assume the VM is dead.
    # If there is, cd into that directory so that we can be sure
    # we are killing the right process and so that we can tell when the process
    # is dead -- when the process has finished exiting, the kernel will
    # unlink /proc/$pid and everything in it.  Even if the kernel manages
    # to cycle through pidspace to create another process with the same pid
    # between kill attempts, we will not see entry for that process in
    # our $PWD, because it will be a different dentry and set of inodes that
    # happen to have the same name.
    cd "/proc/$pid" &>/dev/null || {
        rm -f "$VM_DIR/$VMID.pid" &>/dev/null
        > "$VM_DIR/$VMID.$killstate"
        return 0
    }
    # If the cmdline entry does not exist in /proc/$pid, the process is
    # already dead but the kernel has not finished cleaning up.
    [[ -f cmdline ]] || {
        rm -f "$VM_DIR/$VMID.pid" &>/dev/null
        > "$VM_DIR/$VMID.$killstate"
        return 0
    }
    # If the /proc/$pid/cmdline does not contain the name of our VM,
    # the kernel has finished cleaning up our process and enough processes
    # have been spawned that our PID has been reused.
    grep -q "$VMID" cmdline || {
        rm -f "$VM_DIR/$VMID.pid" &>/dev/null
        > "$VM_DIR/$VMID.$killstate"
        return 0
    }
    # Loop trying to kill this VM.  Escalate our violence and sleep longer
    # the more tries we take.
    while (( killtries++ < 10)); do
        update_vm_status "Killing $VMID(try $killtries, signal $killsig)"
        kill "-$killsig" "$pid"
        ((killtries < 5)) || killsig=KILL
        sleep $killtries
        # if /proc/$pid/cmdline (or any other file that normally exists there)
        # is gone, the process is dead, and our work is done.
        if [[ ! -f cmdline ]]; then
            update_vm_status "Killed with SIG${killsig}"
            rm -f "$VM_DIR/$VMID.pid" &>/dev/null
            > "$VM_DIR/$VMID.$killstate"
            return 0
        fi
    done
    echo "Could not kill $VMID, something went horribly wrong."
    return 1
)

# Wait for a KVM instance to die naturally, for a timeout to expire,
# or for a daemonization condition to be reached.
wait_for_kvm() {
    local pidfile="$VM_DIR/$VMID.pid"
    [[ -f $pidfile ]] || {
        update_vm_status "No pid file for KVM."
        return 1 # no pidfile? Bad Things happened.
    }
    local kvmpid=$(cat "$pidfile")
    while [[ $1 ]]; do
        case $1 in
            -timeout) local deadline=$(($(date +%s) + $2)) timeout=$2; shift;;
            -daemonif) local daemonif=$2; shift;;
            -dieif) local dieif=$2; shift;;
            *) break;;
        esac
        shift
    done
    local lastres= thisres=
    # Use the same /proc/$kvmpid trick we used in kill_vm to
    # make sure we are watching the right process.
    if [[ ! -d /proc/$kvmpid ]]; then
        update_vm_status "No process ID $kvmpid, what happened to our VM?"
        ps aux |grep kvm
        die "We are done here."
    fi
    (   cd "/proc/$kvmpid"
        # if /proc/$kvmpid/$cmdline does not contain the name of our
        # VM, something went horrbly wrong.
        [[ -f cmdline && $(cat cmdline) =~ $VMID ]] || {
            smoketest_status_update "$VMID" "/proc/$kvmpid is not for our VM."
            return 1
        }
        while [[ -f cmdline ]]; do
            # If there is a condition on which we should kill the VM
            # immediatly, test and see if it is true.
            if [[ $dieif ]] && $dieif; then
                update_vm_status "Ran into instant-kill condition."
                return 1
            fi
            # If there is a condition on which we should stop waiting for
            # a VM, test to see if it is true.
            if [[ $daemonif ]]; then
                # We assign the output of $daemonif to a variable so that
                # we don't spam up the test run transcript.
                if thisres=$($daemonif 2>&1); then
                    # If it is, stop watching this VM.
                    update_vm_status "$thisres"
                    update_vm_status \
                        "Daemonizing node with $(($deadline - $(date +%s))) seconds left."
                    return 0
                elif [[ $thisres && $lastres != $thisres ]]; then
                    update_vm_status "$thisres"
                    lastres="$thisres"
                fi
            fi
            # If we were supposed to test for a deadline and we overran it,
            # return with the appropriate status code.
            if [[ $deadline && ! $develop_mode ]] && (($(date +%s) > $deadline)); then
                update_vm_status "Node ran for more than $timeout seconds."
                return 1
            fi
            sleep 10
        done
        # If we wanted to be daemonized but were not, game over man.
        if [[ $daemonif ]]; then
            update_vm_status "Node failed to daemonize."
            return 1
        else
            # We appear to have exited normally.
            update_vm_status "Node exited."
            return 0
        fi
    )
}

# Hash that allows us to track the number of reboots a VM has had.
declare -A kvm_generations

# Run a KVM session.
run_kvm() {
    # run_kvm will try to process the arguemnts it knows about first.
    # It will assume that the first argument it does not know how to process
    # is the name of the VM you want to create, and that any remining arguments
    # are to be passed to KVM verbatim.
    # Note that you must provide either a -bootc or a -bootn argument
    # before the name of the VM if you want the VM to boot off the first
    # hard drive or to PXE boot.  If you do not, or do not otherwise arrange
    # for a way to boot the VM, it will never boot.  The reason for this
    # is the design decision that anu hard drives not attached to IDE channels
    # are not considered boot candidates without a special argument passed
    # to the -drive parameter, and we have to use SCSI attached hard drives to
    # work around device naming differences in CentOS 5 (for Sledgehammer) and
    # more current kernels.
    # $1 = name of KVM to run. Disk image and logfile names are
    # derived from this.
    # KVMs are always oneshot virtual machines -- this works around
    # a few booting and rebooting bugs.
    # $@ = after shifing, other args to be passed to kvm.
    # In addition, we expect that the caller has set vm_nics appropriatly.
    local waitargs=() reboot=false snapshot=''
    local pxeboot='' driveboot='' diskfile="$VM_DIR/$VMID.disk"
    local disk_format="raw" discard=''
    while true; do
        case $1 in
            # -daemon tells the framework to stop active monitoring
            # once the PID file is written.
            -daemon) waitargs+=('-daemonif' 'true');;
            # -bootc tells the VM to boot off the first hard drive
            -bootc) driveboot=true;;
            # -bootn tells the VM to PXE boot.
            -bootn) pxeboot=true;;
            # -timeout will wait up to $2 seconds before killing the VM if
            # we are actively monitoring the VM.
            -timeout) waitargs+=("$1" "$2"); shift;;
            # -daemonif will have the framework stop actively monitoring the
            # VM once $2 exits with a zero status.
            -daemonif) waitargs+=("$1" "$2"); shift;;
            -dieif) waitargs+=("$1" "$2"); shift;;
            # -reboot allows the VM to reboot instead of halting on reboot.
            -reboot) reboot=true;;
            # Run the main disk in snapshot mode.
            -snapshot) snapshot=true;;
            # Use the specified disk format.
            -diskformat) disk_format="$2"; shift;;
            # Use the passed disk as the main one instead of $VMID.disk
            -disk)
                diskfile="$2"
                disk_format="qcow2"
                shift;;
            *) break;;
        esac
        shift
    done
    # Track the number of times that this VM has been started by the
    # framework.
    if [[ ! ${kvm_generations["$VMID"]} ]]; then
        kvm_generations["$VMID"]=1
    else
        kvm_generations["$VMID"]=$((${kvm_generations["$VMID"]} + 1))
    fi
    # Hack to pick the fastest disk caching mode.
    # We use unsafe caching if we can on the vms because we will just
    # rebuild the filesystems from scratch if anything goes wrong.
    if ! [[ $drive_cache ]]; then
        if "$KVM" --help |grep -q 'cache.*unsafe'; then
            drive_cache=unsafe
        else
            drive_cache=writeback
        fi
        if "$KVM" -device \? 2>&1 |grep -q ahci; then
            kvm_use_ahci=true
        fi
        if "$KVM" --help |grep -q 'discard.*unmap'; then
            discard=unmap
        fi
    fi
    local vm_gen="$VMID.${kvm_generations[$VMID]}"
    # create a new log directory for us.  vm_logdir needs to be global
    # because other things will need to read the logs to see what is happening.
    vm_logdir="$VM_DIR/logs/$vm_gen"
    mkdir -p "$vm_logdir"
    local pidfile="$VM_DIR/$VMID.pid"
    [[ -f $pidfile ]] && rm "$pidfile" || :
    # Our initial array of args for kvm.
    # We pass in our own ROM for PXE booting to work around some old
    # PXE boot bugs in older versions of the E1000 etherboot roms.
    local kvmargs=(-enable-kvm
        -m $OCB_MEM
        -smp $OCB_CPUS
        -pidfile "$pidfile"
        -serial "file:$vm_logdir/ttyS0.log"
        -serial "file:$vm_logdir/ttyS1.log"
        -name "kvm-$vm_gen")
    if [[ $kvm_use_ahci = true ]]; then
        local drivestr="file=$diskfile,if=none,format=$disk_format,cache=$drive_cache,id=drive-ahci-0"
        if [[ $discard ]]; then
            drivestr+=",discard=unmap"
        fi
        kvmargs+=(-device "ahci,id=ahci0,bus=pci.0,multifunction=on")
        kvmargs+=(-drive "$drivestr")
        kvmargs+=(-device "ide-drive,bus=ahci0.0,drive=drive-ahci-0,id=drive-0")
        local drive_idx=1
        for image in "$VM_DIR/$VMID-"*".disk"; do
            [[ -f $image ]] || continue
            kvmargs+=(-device "ahci,id=ahci${drive_idx},bus=pci.0,multifunction=on")
            kvmargs+=(-drive "file=$image,if=none,cache=$drive_cache,id=drive-ahci-${drive_idx}")
            kvmargs+=(-device "ide-drive,bus=ahci${drive_idx}.0,drive=drive-ahci-${drive_idx},id=drive-${drive_idx}")
            drive_idx=$((drive_idx + 1))
        done
        unset drive_idx
    else
        local drivestr="file=$diskfile,if=scsi,format=$disk_format,cache=$drive_cache"
        if [[ $driveboot ]]; then
            drivestr+=",boot=on"
        fi
        kvmargs+=(-drive "$drivestr")
        # Add additional disks if we have any.
        for image in "$VM_DIR/$VMID-"*".disk"; do
            [[ -f $image ]] || continue
            kvmargs+=(-drive "file=$image,if=scsi,format=qcow2,cache=$drive_cache")
        done
    fi
    # Add appropriate nics based on the contents of vm_nics.
    local vlan=0
    for line in "${vm_nics[@]}"; do
        kvmargs+=(-net "nic,macaddr=${line%%,*},model=e1000,vlan=$vlan")
        kvmargs+=(-net "tap,ifname=${line##*,},script=no,downscript=no,vlan=$vlan")
        vlan=$(($vlan + 1))
    done
    unset vlan
    if [[ $pxeboot ]]; then
        kvmargs+=(-boot "order=n")
    elif [[ $driveboot && $reboot = false ]]; then
        kvmargs+=(-boot "order=nc")
    else
        kvmargs+=(-boot "order=c")
    fi

    if [[ $reboot = false ]]; then
        kvmargs+=(-no-reboot)
    fi
    # If we are running under X, then use a graphical display.
    if [[ $TMUX && $OCB_SESSION && $SESSION = $OCB_SESSION ]]; then
        kvmargs+=( -vnc "unix:${vm_logdir%.*}.vncsock" -monitor stdio)
        echo "Attach to this VM with 'ssvncviewer \"${vm_logdir%.*}.vncsock\"'"
        tmux new-window -n "$SESSION/kvm-$VMID" -c "$vm_logdir" "$KVM ${kvmargs[*]} $*"
        tmux set-window-option -t "$SESSION/kvm-$VMID" remain-on-exit off
    elif [[ $DISPLAY ]]; then
        kvmargs+=( -sdl -daemonize )
        "$KVM" "${kvmargs[@]}" "$@"
    else
        # otherwise, launch ourselves under screen.
        kvmargs+=( -vnc "unix:${vm_logdir%.*}.vncsock" -monitor stdio)
        screen -S "${VMID}" -d -m -- "$KVM" "${kvmargs[@]}" "$@" || die "Aiee!"
        screen -S "${VMID}" -X log on || die "Turbo-die"
    fi
    # wait up to 10 seconds for a PID file
    local s
    for ((s=0; s<10; s++)); do
        [[ -f $pidfile ]] && break
        sleep 1
    done
    # Now that we have launched the KVM instance, wait for it using the
    # waitargs we were passed.
    wait_for_kvm "${waitargs[@]}" || return 1
}
