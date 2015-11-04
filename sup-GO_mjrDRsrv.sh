#!/bin/bash

# This script can keep create a pipe, and make sure a mjrDRsrv is always running to listen to that pipe.  It can also stop the
# mjrDRsrv, and not start up a new one. In an init.d-esque way, only a single argument is provided: [start|stop]

MJRDRSRV_FIFO=/home/richmit/.lispy-mjrDRsrv-fifo

if [ "$1" = "start" ] ; then
	if [ -e $MJRDRSRV_FIFO ] ; then
		rm $MJRDRSRV_FIFO
	fi
	mkfifo $MJRDRSRV_FIFO
    MJRDRSRV_BIN=mjrDRsrv
    for BINPOS in '/home/richmit/bin/mjrDRsrv' '/home/richmit/world/my_prog/mjrDrawServer/libplot/mjrDRsrv' ; do
        if [ -x "$BINPOS" ] ; then
            MJRDRSRV_BIN=$BINPOS
            break
        fi
    done
	while true; do
		echo "Sleeping for 1 second before continuing..."
		sleep 1
		if [ -e $MJRDRSRV_FIFO ] ; then
			echo "Starting up mjrDRsrv: " `date`
			$MJRDRSRV_BIN < $MJRDRSRV_FIFO
		else
			echo "FIFO mising.  Aborting now: " `date`
			exit
		fi
	done
    exit
fi

if [ "$1" = "stop" ] ; then
	if [ -e $MJRDRSRV_FIFO ] ; then
		echo "exit" > $MJRDRSRV_FIFO
		rm $MJRDRSRV_FIFO
    else
        echo "No FIFO found.  Don't know what to do...."
	fi
    exit
fi

echo "ERROR!"
echo "Provide one argument to this script:  [start|stop]"
