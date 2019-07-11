#!/usr/bin/env bash

for file in ~/.config/bash/startup/*.sh ; do
    [[ -r "${file}" ]] && [[ -f "${file}" ]] && source $file;
done;

unset file;
