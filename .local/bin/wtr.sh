#!/bin/bash
# pierreD

# get weather from wttr.in
# usage: wtr.sh <city>

set -e

curl wttr.in/"$1"?m\&lang=fr
