#!/usr/bin/env bash 
set -xe

purs-backend-es build
purs-backend-es bundle-app --main Main --minify --no-build --to dist.js --platform=node

