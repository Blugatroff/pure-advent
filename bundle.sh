#!/usr/bin/sh
set -xe

spago build --config ./prod.dhall 
purs-backend-es build
echo "import { main } from './output/Main/index.js'; main();" | esbuild --bundle --platform=node > dist.js
