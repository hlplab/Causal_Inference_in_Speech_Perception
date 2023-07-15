for f in *.mp4;
do
	if [ ! -f "${f%%.*}".webm ]; then
        ffmpeg -i $f -codec:v libvpx -crf 10 -b:v 1000k -threads 8 -codec:a libvorbis webm/"${f%%.*}".webm

		echo "converting ${f%%.*}".mp4." to webm format."
	else
		echo "already exists... skipping"
	fi
	
done

