filename=$(printf %q "$1")
filename_without_extension=`echo $filename | sed -e 's/\.[^.]*$//'`

echo $filename_without_extension
echo $filename

ffmpeg -i $filename -acodec libmp3lame -aq 4 $filename_without_extension.mp3
