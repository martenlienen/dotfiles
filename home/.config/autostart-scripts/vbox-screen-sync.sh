#!/bin/sh

# Synchronize screen size etc. if the system runs in a virtualbox
command -v VBoxClient-all > /dev/null && VBoxClient-all
