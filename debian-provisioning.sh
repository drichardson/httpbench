#!/bin/bash
# Provision a Rackspace OnMetal Debian Wheezy instance
# This script should be idempotent.
# To run, do something like this:
# ssh root@104.130.18.76 'bash -s' < debian-provisioning.sh
# or, on Amazon EC2 ubunti AMIs use:
# ssh ubuntu@104.130.18.76 'sudo bash -s' < debian-provisioning.sh

set -e

# Use same ssh key for non-root user
copy_roots_authorized_keys_to_user() {
    echo "Installing root's authorized_keys for $1"
    local UD=/home/$1
    local AUTHKEYS=$UD/.ssh/authorized_keys
    mkdir -p $UD/.ssh
    cp /root/.ssh/authorized_keys $AUTHKEYS
    chown $1:$1 $UD/.ssh $AUTHKEYS
    chmod 600 $AUTHKEYS
    chmod 700 $UD/.ssh
}

create_user () {
    # Create non-root user. If user already exists this will fail.
    set +e
    useradd -m --shell /bin/bash $1
    set -e
}

provision() {
    echo Provisioning host


    # Get some standard stuff I need
    apt-get --yes update

    local DIST_ID=$(lsb_release -i | awk -F ' ' '{print $3}') 
    local PACKAGES="vim-nox emacs gcc g++ git subversion sbcl sudo curl make apache2-utils \
strace sysstat gdb tcpdump libclang-dev cscope cl-quicklisp"

    
    case "$DIST_ID" in
        Debian)
            PACKAGES="$PACKAGES linux-tools"
            ;;
        Ubuntu)
            PACKAGES="$PACKAGES linux-tools-generic"
            ;;
        *)
            echo "Unhandled distributor ID $DIST_ID"
            exit 1
    esac

    apt-get --yes install $PACKAGES

    # would also be nice to install these but wheezy doesn't have them
    # apt-get --yes install blkstat nicstat

    # if you want to install ktap, need few more things
    apt-get --yes install linux-headers-$(uname -r)

    local NONROOT_USER=doug
    create_user $NONROOT_USER
    copy_roots_authorized_keys_to_user $NONROOT_USER
}

RESULT=FAILURE
report_result() {
    echo $RESULT
}
trap "report_result;" EXIT
provision
RESULT=OK
