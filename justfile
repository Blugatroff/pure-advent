pursargs := '"--censor-codes="MissingTypeDeclaration,ShadowedName" --stash"'

default:
    just --list

build:
    spago build -u {{pursargs}}

run *ARGS:
    spago run -u {{pursargs}} --exec-args "{{ARGS}}"

all:
    spago run -a all

bundle:
    spago build --config ./prod.dhall 
    purs-backend-es build
    echo "#!/usr/bin/env node" > dist.js
    echo "import { main } from './output/Main/index.js'; main();" | esbuild --bundle --platform=node --minify >> dist.js
