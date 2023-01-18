let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221229/packages.dhall
        sha256:a6af1091425f806ec0da34934bb6c0ab0ac1598620bbcbb60a7d463354e7d87c

in  upstream
  with js-bigints.repo = "https://github.com/rowtype-yoga/purescript-js-bigints.git"
  with js-bigints.version = "a7fa91c08f873bc590512302ac08b273f8fdcae4"
  with arrays.repo = "https://github.com/Blugatroff/purescript-arrays.git"
  with arrays.version = "b518e62e1266ac06c0033be50bc50533b18babcb"

