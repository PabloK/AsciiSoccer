#!/bin/bash

if [ "$1" = "1" ]; then
    /student/bin/mpg123 -z $2 >& /dev/null &

fi
#if [ "$1" = "0" ]; then
#    ps -a | grep mpg123 | cut -c2-7 | xargs kill >& /dev/null
#fi

if [ "$1" = "0" ]; then
    pkill -KILL mpg123
fi
