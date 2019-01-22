LEFT="DP2-1"
MIDDLE="DP2-2"
RIGHT="eDP1"

xrandr --auto --output "${LEFT}" --mode 1920x1080 --left-of "${MIDDLE}" && \
    xrandr --auto --output "${MIDDLE}" --mode  1920x1080 --right-of "${LEFT}"
#    xrandr --auto --output "${RIGHT}" --mode  1920x1080 --right-of "${MIDDLE}"
