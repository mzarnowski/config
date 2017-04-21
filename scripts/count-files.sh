Files="$(find . -maxdepth 1 -type f -printf x | wc -c)"
Directories="$(find . -maxdepth 1 -type d -printf x | wc -c)"

echo "file count: $Files"
echo "directory count: $Directories"
