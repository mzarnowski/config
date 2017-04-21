for file in *.txt; do iconv -f windows-1250 -t utf8 -o .new  && mv -f .new ; done
