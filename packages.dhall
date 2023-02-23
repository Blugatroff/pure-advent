let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230222/packages.dhall
        sha256:79789966818455024bafbfe1b0629aaae10c4017b3b9085931debd11d1905b16

in  upstream
  with js-bigints.repo
       = "https://github.com/rowtype-yoga/purescript-js-bigints.git"
  with js-bigints.version = "a7fa91c08f873bc590512302ac08b273f8fdcae4"
