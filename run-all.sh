#!/bin/bash
echo "Creating named pipes..."
rm -rf ./tmp
mkdir ./tmp

X=./smarthome
PORT=9191

IN_TEMP=./tmp/temp-input
IN_MOTION=./tmp/motion-input
IN_OUTLET=./tmp/outlet-input
IN_BULB=./tmp/bulb-input
IN_HEATER=./tmp/heater-input
IN_LIGHT=./tmp/light-input
OUT_TEMP=./tmp/temp-output
OUT_MOTION=./tmp/motion-output
OUT_OUTLET=./tmp/outlet-output
OUT_BULB=./tmp/bulb-output

mkfifo $IN_TEMP $IN_MOTION $IN_OUTLET $IN_BULB $IN_HEATER $IN_LIGHT
mkfifo $OUT_TEMP $OUT_MOTION $OUT_OUTLET $OUT_BULB

echo "Starting gateway..."
$X gateway 0.0.0.0 $PORT > /dev/null &
PID_GATEWAY=$!
sleep 1
echo "Starting devices..."
tail -f $IN_TEMP | $X temp 127.0.0.1 $PORT silent > $OUT_TEMP &
ID_TEMP=`head -n 1 $OUT_TEMP`
tail -f $IN_MOTION | $X motion 127.0.0.1 $PORT silent > $OUT_MOTION &
ID_MOTION=`head -n 1 $OUT_MOTION`
tail -f $IN_OUTLET | $X outlet 127.0.0.1 $PORT silent > $OUT_OUTLET &
ID_OUTLET=`head -n 1 $OUT_OUTLET`
tail -f $IN_BULB | $X bulb 127.0.0.1 $PORT silent > $OUT_BULB &
ID_BULB=`head -n 1 $OUT_BULB`
echo "Starting controllers..."
tail -f $IN_HEATER | $X control heater 127.0.0.1 $PORT silent > /dev/null &
PID_HEATER=$!
echo $ID_TEMP > $IN_HEATER
echo $ID_OUTLET > $IN_HEATER
tail -f $IN_LIGHT | $X control light 127.0.0.1 $PORT silent > /dev/null &
PID_LIGHT=$!
echo $ID_MOTION > $IN_LIGHT
echo $ID_BULB > $IN_LIGHT


echo "-------------------------"
echo "  DEVICE IDs"
echo "-------------------------"
echo "  Temperature sensor: $ID_TEMP"
echo "  Motion sensor:      $ID_MOTION"
echo "  Smart outlet:       $ID_OUTLET"
echo "  Smart light bulb:   $ID_BULB"
echo "-------------------------"

./smarthome control user 127.0.0.1 9191

echo "-------------------------"
echo "Killing all programs..."
echo "-------------------------"

fuser -k $IN_LIGHT
fuser -k $IN_HEATER
kill $PID_LIGHT
kill $PID_HEATER

echo "exit" > $IN_BULB
echo "exit" > $IN_OUTLET
echo "exit" > $IN_MOTION
echo "exit" > $IN_TEMP
sleep 1
fuser -k $IN_BULB
fuser -k $IN_OUTLET
fuser -k $IN_MOTION
fuser -k $IN_TEMP
kill $PID_GATEWAY
rm -rf ./tmp

