#!/bin/sh

launchctl list | fgrep surfboard
launchctl stop local.surfboard
launchctl remove local.surfboard
launchctl list | fgrep surfboard

