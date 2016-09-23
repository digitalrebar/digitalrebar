#!/bin/bash
# If ohai is tool old, remove it
if which ohai; then
    matcher='^Ohai: 6'
    if [[ "$(ohai --version)" =~ $matcher ]] ; then
        case $OS_FAMILY in
            rhel)
                yum -y erase ohai;;
            debian)
                apt-get purge -y ohai;;
            *)
                die "Need to remove ohai";;
        esac
    fi
fi

GEM=gem
RUBY=ruby

# Add a good version
if ! which ohai; then
    case $OS_FAMILY in
        rhel)
            [[ $OS_VER = 6* ]] && die "No good way to install ohai"
            yum -y install ruby-devel gcc;;            
        debian)
            case $OS_NAME in
                ubuntu-12.*)
                    apt-get -y install ruby1.9.1-dev build-essential
                    RUBY=ruby1.9.1
                    GEM=gem1.9.1;;
                *)
                    apt-get -y install ruby-dev build-essential;;
            esac;;
        *) die "No idea how to install ruby and rubygems";;
    esac
    matcher="^ruby 1.9"
    if [[ $($RUBY --version) =~ $matcher ]] ; then
        $GEM install ohai -v 7.4.1
    else
        $GEM install ffi-yajl -v 2.2.3
        $GEM install ohai -v 8.17.1
    fi
    hash -r
fi

ohai --directory $TMPDIR/rebar-inventory/plugin > $TMPDIR/attrs/ohai.json

