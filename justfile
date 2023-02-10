pursargs := '"--censor-codes="MissingTypeDeclaration,ShadowedName" --stash"'

default:
    just --list

build:
    spago build -u {{pursargs}}

run *ARGS:
    spago run -u {{pursargs}} --exec-args "{{ARGS}}"
