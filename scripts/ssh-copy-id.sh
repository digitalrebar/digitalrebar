#!/bin/sh

#
# Specialied copy-ssh-id.sh that attempts to update the root user for access.
#
# Args:
#   --init-ident <file>   # Initial identity file for login, otherwise password login is required
#   --identity <file>     # Identity file to put in place - if missing try to get from ssh-add -L
#   --clean               # Remove anti-login lines.
#   <host ip>             # system to login as
#   --user <user>         # User to update
#

ID_FILE=""
CLEAN_IT=""
INIT_ID_FILE=""
ACCOUNT="root"
SUDO=""

while (( $# > 0 )); do
  arg="$1"
  case $arg in
    --clean) shift ; CLEAN_IT="YES";;
    --identity)
      shift
      ID_FILE=$1
      shift;;
    --user)
      shift
      ACCOUNT=$1
      shift;;
    --init-ident)
      shift
      INIT_ID_FILE="-i $1"
      shift;;
    --help|-h)
      echo "Usage: $0 [--identity <identity_file>] [--clean] [--init-ident <file>] user@machine" >&2
      exit 1;;
    *) break;;
  esac
done

NODE_INFO=$1
shift

if [ "$NODE_INFO" == "" ] ; then
  echo "Usage: $0 [--identity <identity_file>] [--clean] [--init-ident <file>] user@machine" >&2
  exit 1
fi

NODE_LOGIN=${NODE_INFO%%@*}
if [ "$NODE_LOGIN" == "" ] ; then
  echo "Must specify login identity"
  echo "Usage: $0 [--identity <identity_file>] [--clean] [--init-ident <file>] user@machine" >&2
  exit 1
fi

if [ "$ID_FILE" == "" ]; then
  if [ x$SSH_AUTH_SOCK != x ] ; then
    GET_ID="$GET_ID ssh-add -L | grep -vxF 'The agent has no identities.'"
  fi
fi

if [ "$ID_FILE" == "" ]; then
  ID_FILE="${HOME}/.ssh/id_rsa.pub"
fi

if [ -z "`eval $GET_ID`" ] && [ -r "${ID_FILE}" ] ; then
  GET_ID="cat ${ID_FILE}"
fi

if [ -z "`eval $GET_ID`" ]; then
  echo "$0: ERROR: No identities found" >&2
  exit 1
fi

DIR_TO_CHANGE="/home/${ACCOUNT}/.ssh"
FILE_TO_CHANGE="/home/${ACCOUNT}/.ssh/authorized_keys"
if [ "$ACCOUNT" == "root" ] ; then
  DIR_TO_CHANGE="/root/.ssh"
  FILE_TO_CHANGE="/root/.ssh/authorized_keys"
fi

if [ "$NODE_LOGIN" != "$ACCOUNT" ] ; then
  SUDO="sudo"
fi

if [ "$CLEAN_IT" == "YES" ] ; then
  CLEAN_IT="$SUDO sed -i -e 's/^.* \(ssh-\)/\1/' ${FILE_TO_CHANGE} ; "
fi

ID=$(eval $GET_ID)

echo "You may be asked for $NODE_INFO password"
ssh -tt $INIT_ID_FILE $NODE_INFO "umask 077; $SUDO test -d ${DIR_TO_CHANGE} || $SUDO mkdir ${DIR_TO_CHANGE} ; $SUDO echo \"$ID\" >> ${FILE_TO_CHANGE}; $CLEAN_IT $SUDO sort -u ${FILE_TO_CHANGE} > /tmp/a ; $SUDO cp /tmp/a ${FILE_TO_CHANGE} ; $SUDO rm /tmp/a ; $SUDO test -x /sbin/restorecon && $SUDO /sbin/restorecon ${DIR_TO_CHANGE} ${FILE_TO_CHANGE}" || exit 1
echo "Keys in place"

