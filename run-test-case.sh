#!/bin/bash

X=./smarthome
PORT=8281

# Make some named pipes.
# ---------------------------------------------------------

rm -rf ./tmp
mkdir ./tmp

IN_TEMP=./tmp/temp-input
IN_MOTION=./tmp/motion-input
IN_TESTLOG=./tmp/light-input
OUT_TEMP=./tmp/temp-output
OUT_MOTION=./tmp/motion-output

mkfifo $IN_TEMP $IN_MOTION $IN_TESTLOG
mkfifo $OUT_TEMP $OUT_MOTION

# Start the devices.
# ---------------------------------------------------------

echo "Starting gateway..."
$X gateway 0.0.0.0 $PORT > /dev/null &
PID_GATEWAY=$!
sleep 1
echo "Starting devices and controller..."
tail -f $IN_TEMP | $X temp 127.0.0.1 $PORT silent > $OUT_TEMP &
ID_TEMP=`head -n 1 $OUT_TEMP`
tail -f $IN_MOTION | $X motion 127.0.0.1 $PORT silent > $OUT_MOTION &
ID_MOTION=`head -n 1 $OUT_MOTION`
tail -f $IN_TESTLOG | $X control testlog 127.0.0.1 $PORT silent > test-output.txt &
echo $ID_TEMP > $IN_TESTLOG
echo $ID_MOTION > $IN_TESTLOG

# Send the CSV data to the pipes.
# ---------------------------------------------------------

echo "Executing CSV instructions (takes ~15s)..."

LAST_MOTION=0

awk "NR>2" test-input.csv | while read -ra LINE; do
  IFS=',' read -ra FIELD <<< "${LINE[@]}"
  TIME=${FIELD[0]}
  TEMP=${FIELD[1]}
  MOTION=${FIELD[2]}
  ACTION=${FIELD[3]}
  {
    sleep $TIME
    echo "$TEMP" > $IN_TEMP
    if [ "$MOTION" -ne "$LAST_MOTION" ]; then
      if [ "$MOTION" -eq 1 ]; then echo "on" > $IN_MOTION; fi
      if [ "$MOTION" -eq 0 ]; then echo "off" > $IN_MOTION; fi
    fi
    echo "$ACTION" > $IN_TESTLOG
  } &
  LAST_MOTION=$MOTION
done

# Kill everything.
# ---------------------------------------------------------

sleep 16

echo "Done; killing processes."

echo "exit" > $IN_TESTLOG
echo "exit" > $IN_MOTION
echo "exit" > $IN_TEMP
sleep 1
fuser -k $IN_TESTLOG
fuser -k $IN_MOTION
fuser -k $IN_TEMP
kill $PID_GATEWAY
rm -rf ./tmp

