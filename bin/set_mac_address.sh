#! /bin/bash

# e.g. set_mac_address f0:4d:a2:7a:60:7b

sudo ifconfig eth0 down
sudo ifconfig eth0 hw ether "$1"
sudo ifconfig eth0 up
