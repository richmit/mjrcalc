#!/bin/bash

# This script can keep create a pipe, and make sure a gnuplot is always running to listen to that pipe.  It can also stop the
# gnuplot, and not start up a new one. In an init.d-esque way, only a single argument is provided: [start|stop]

GNUPLOT_FIFO=/home/richmit/.lispy-gnuplot-fifo

if [ "$1" = "start" ] ; then
	if [ -e $GNUPLOT_FIFO ] ; then
		rm $GNUPLOT_FIFO
	fi
	mkfifo $GNUPLOT_FIFO
    GNUPLOT_BIN=gnuplot
    for BINPOS in '/apps/free/gnuplot/4.4.1/bin/gnuplot' '/apps/free/gnuplot/4.2.4/bin/gnuplot' '/opt/local/bin/gnuplot' '/usr/local/bin/gnuplot' ; do
        if [ -x "$BINPOS" ] ; then
            GNUPLOT_BIN=$BINPOS
            break
        fi
    done
	while true; do
		echo "Sleeping for 1 second before continuing..."
		sleep 1
		if [ -e $GNUPLOT_FIFO ] ; then
			echo "Starting up gnuplot: " `date`
			$GNUPLOT_BIN < $GNUPLOT_FIFO
		else
			echo "FIFO mising.  Aborting now: " `date`
			exit
		fi
	done
    exit
fi

if [ "$1" = "stop" ] ; then
	if [ -e $GNUPLOT_FIFO ] ; then
		echo "exit" > $GNUPLOT_FIFO
		rm $GNUPLOT_FIFO
    else
        echo "No FIFO found.  Don't know what to do...."
	fi
    exit
fi

echo "ERROR!"
echo "Provide one argument to this script:  [start|stop]"
