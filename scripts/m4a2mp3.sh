filename=$(printf %q "$1")
filename_without_extension=`echo $filename | sed -e 's/\.[^.]*$//'`

echo $filename_without_extension
echo $filename

ffmpeg -i $filename -acodec mp3 -ac 2 -ab 192k $filename_without_extension.mp3
