let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230425/packages.dhall
        sha256:74bd0986f8c25c67be10cd702466425caaefe495981a49b21092cb746ca63768
in upstream
  with js-bigints.repo = "https://github.com/purescript-contrib/purescript-js-bigints.git" 
  with js-bigints.version = "e026a785e6241801d23024f0a9d74685d4179fa5"
