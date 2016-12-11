#!/usr/bin/env bash

git ls-files | entr -r -- /bin/bash -c 'stack build --fast && stack exec tube-bot-fulfillment'
