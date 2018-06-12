#!/bin/bash
if [ ! -f ~/.ssh/id_dsa.pub ]
then
    echo 'id_dsa.pub does not exist, creating'
    ssh-keygen -tdsa
fi
ssh-copy-id $1
