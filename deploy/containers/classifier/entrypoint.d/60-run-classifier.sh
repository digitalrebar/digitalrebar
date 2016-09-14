#!/bin/bash

if [[ ! $MYPORT ]] ; then
	MYPORT=13753
fi

/usr/local/bin/classifier -rules /etc/classifier/rules.yml -listen $IP:$MYPORT

