for f in *.mp4;
do
	if [ ! -f "${f%%.*}".webm ]; then
        ffmpeg -i $f -codec:v libx264 -profile:v high -preset slow -b:v 1000k -maxrate 1000k -bufsize 1000k -threads 8 -codec:a copy mp4/"${f%%.*}".mp4

		echo "converting ${f%%.*}".mp4." to h264 format."
	else
		echo "already exists... skipping"
	fi
	
done

