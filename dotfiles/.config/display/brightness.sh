#!/bin/bash

STEP=0.1

brightness_file(){
	local display=$1
	
	echo ~/.config/display/"${display}"-brightness
}

set_brightness(){
	local display=$1
	local brightness="$2"
	local file=$(brightness_file "${display}")

	xrandr --output "${display}" --brightness "${brightness}" \
	&& echo "${brightness}" > "${file}"
}

current_brightness(){
	local display=$1
	local file=$(brightness_file "${display}")

	([ -f "${file}" ] || set_brightness "${display}" 1.0) && cat "${file}"
}

new_brightness(){
	local display=$1
	local mode=$2

	local brightness=$(current_brightness "${display}") || return 3

	if [ "${mode}" = '+' ]; then
		brightness=$(echo "$brightness + ${STEP}" | bc)
		if [ "$(echo "$brightness > 1.0" | bc)" -eq 1 ]; then
			brightness='1.0'
		fi
	elif [ "${mode}" = '-' ]; then
		brightness=$(echo "$brightness - ${STEP}" | bc)
		if [ "$(echo "$brightness < 0.1" | bc)" -eq 1 ]; then
			brightness='0.1'
		fi
	fi

	echo "${brightness}"
}

mode=$1
displays=$(xrandr -q | grep ' connected' | head -n 1 | cut -d ' ' -f1)
for display in ${displays}; 
do
	brightness="$(new_brightness "${display}" "${mode}")"
	set_brightness "${display}" "${brightness}"
done