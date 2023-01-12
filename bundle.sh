#!/usr/bin/sh
set -xe

spago build --config ./prod.dhall 
purs-backend-es build
esbuild --bundle ./index.mjs --platform=node > dist.js
