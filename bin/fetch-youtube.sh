#!/bin/sh
find . -type f -name '.yt' -exec sh -c 'youtube-dl -o "$(dirname {})/%(title)s.%(ext)s" $(cat {})' \;
