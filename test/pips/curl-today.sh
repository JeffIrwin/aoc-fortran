#!/usr/bin/env bash

# Download today's input in JSON format

set -exu

date_=$(date +%Y-%m-%d)

url=https://www.nytimes.com/svc/pips/v1/"$date_".json
curl -L "$url" -o inputs/"$date_".json

