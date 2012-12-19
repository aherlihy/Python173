#!/bin/bash
# @aherlihy


sigquit_handler() {
    echo "exiting..."
    exit
}
trap sigquit_handler SIGQUIT

DIRECTORY=./test_output/$1_output
FILES=~/python/Python173/design3/python-reference/$1/*

if [ ! -d "./test_output" ]; then
    mkdir "./test_output"
fi

if [ ! -d "$DIRECTORY" ]; then
    mkdir $DIRECTORY
fi
if [ ! -d "$DIRECTORY/passing" ];then
   mkdir $DIRECTORY/passing
fi

rm -f $DIRECTORY/passing/*
rm -f $DIRECTORY/*-OUT
for f in $FILES
do
   FILENAME=$(basename $f)
   echo "Testing $FILENAME"
   cat $f | racket python-main.rkt --python-path /course/cs173/python/Python-3.2.3/python --interp 2> $FILENAME"-OUT"
   mv $FILENAME"-OUT" $DIRECTORY/$FILENAME"-OUT"

   if [ -s $DIRECTORY/$FILENAME"-OUT" ]; then
       echo "	failed " $FILENAME
	gedit $f &
   else
       mv $DIRECTORY/$FILENAME"-OUT" $DIRECTORY/passing/$FILENAME"-OUT"
       
   fi

echo "Done"
done


