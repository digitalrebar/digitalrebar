#!/bin/bash

if [[ -f $BUILT_CFG_FILE ]]; then
    cd /opt/digitalrebar/core

    # Process networks
    admin_nets=()
    network_count=`jq ".networks | length" ${BUILT_CFG_FILE}`
    for ((i=0; i < network_count; i++)) ; do
        network=`jq ".networks[$i]" ${BUILT_CFG_FILE}`
        group=`jq -r ".networks[$i].group" ${BUILT_CFG_FILE}`
        category=`jq -r ".networks[$i].category" ${BUILT_CFG_FILE}`
        if ! [[ $category && $group ]]; then
            echo "Network must have a category and a group defined!"
            exit 1
        fi
        name="$category-$group"
        if rebar networks show $name >/dev/null 2>&1 ; then
            rebar networks update $name "$network" 
        else
            rebar networks import "$network"
        fi
        if [ "$category" == "admin" ] ; then
            admin_nets=(${admin_nets[@]} $name)
        fi
    done

    # Deployments
    deployment_count=`jq ".deployments | length" ${BUILT_CFG_FILE}`
    for ((i=0; i < deployment_count; i++)) ; do
        name=`jq -r ".deployments[$i].deployment.name" ${BUILT_CFG_FILE}`
        deployment=`jq ".deployments[$i].deployment" ${BUILT_CFG_FILE}`
        
        # Create or update the deployment
        if rebar deployments show $name >/dev/null 2>&1 ; then
            rebar deployments update $name "$deployment"
        else
            rebar deployments create "$deployment"
        fi
        
        # Add roles
        dr_count=`jq ".deployments[$i].roles | length" ${BUILT_CFG_FILE}`
        for ((dri=0; dri < dr_count; dri++)) ; do
            dr_role=`jq -r ".deployments[$i].roles[$dri]" ${BUILT_CFG_FILE}`
            
            rebar deployments bind $name to $dr_role 2>/dev/null || true
        done
        
        # Update attributes
        count=`jq ".deployments[$i].attributes|keys|length" ${BUILT_CFG_FILE}`
        for ((k=0; k < count; k++)) ; do
            kname=`jq -r ".deployments[$i].attributes|keys|.[$k]" ${BUILT_CFG_FILE}`
            kvalue=`jq ".deployments[$i].attributes[\"$kname\"]" ${BUILT_CFG_FILE}`
            
            rebar deployments set $name attrib $kname to "{ \"value\": $kvalue }"
        done
        
        rebar deployments commit $name
    done
     
    # Add/Update DNS Filters into the system
    filter_count=`jq ".filters | length" ${BUILT_CFG_FILE}`
    for ((i=0; i < filter_count; i++)) ; do
        dnf=`jq ".filters[$i]" ${BUILT_CFG_FILE}`
        name=`jq -r ".filters[$i].name" ${BUILT_CFG_FILE}`
        if rebar dnsnamefilters show $name >/dev/null 2>&1 ; then
            rebar dnsnamefilters update $name "$dnf"
        else
            rebar dnsnamefilters create "$dnf"
        fi
    done
    
    # Add/Update users into the system
    user_count=`jq ".users | length" ${BUILT_CFG_FILE}`
    for ((i=0; i < user_count; i++)) ; do
        user=`jq ".users[$i]" ${BUILT_CFG_FILE}`
        name=`jq -r ".users[$i].username" ${BUILT_CFG_FILE}`
        if rebar users show $name >/dev/null 2>&1 ; then
            rebar users update $name "$user"
        else
            rebar users import "$user"
        fi
    done
fi
