

is_active(){
	display=$1
	xrandr --listactivemonitors | grep "${display}"
}

left="HDMI-0"
right="DVI-I-1"

is_active $left \
	&& xrandr --output "${right}" --off \
	|| xrandr --output "${right}" --auto --right-of "${left}"
