#!/usr/bin/sh

is_active(){
	display=$1
	xrandr --listactivemonitors | grep "${display}"
}

left="HDMI-0"
right="DVI-I-1"

is_active $left \
	&& xrandr --output "${left}" --off \
	|| xrandr --output "${left}" --auto --left-of "${right}"
