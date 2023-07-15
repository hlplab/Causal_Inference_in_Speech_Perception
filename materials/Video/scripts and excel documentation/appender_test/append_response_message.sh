#!/bin/bash
for f in *.webm;
do
	(echo file $f.webm & echo file trial-end-message.webm)>list.txt
	C:\\ffmpeg\\bin\\ffmpeg -nostdin -f concat -safe 0 -i list.txt -c copy $f.webm
echo "appending trial-end-message.webm to $f.webm"
done
