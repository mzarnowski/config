filename=$(printf %q "$1")
filename_without_extension=`echo $filename | sed -e 's/\.[^.]*$//'`

echo $filename_without_extension
echo $filename

ffmpeg -i $filename -vcodec copy -acodec copy $filename_without_extension.mp4
