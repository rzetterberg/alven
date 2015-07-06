module Layout.AdminBench (benches) where

import           Criterion.Types
import           Prelude

import           Layout.Admin (removeClass)

--------------------------------------------------------------------------------

benches :: Benchmark
benches = bgroup "Layout admin" $ map build
    [ ( "removeClass one class and multiple attributes"
      , (removeClass "test")
      , [ ("class", "test")
        , ("test", "some other attribute")
        , ("test2", "some other attribute2")
        , ("hello", "hello there")
        ]
      )
    , ( "removeClass one long class and multiple attributes"
      , (removeClass "testtesttesttesttesttesttesttesttesttesttest")
      , [ ("class", "testtesttesttesttesttesttesttesttesttesttest")
        , ("test", "some other attribute")
        , ("test2", "some other attribute2")
        , ("hello", "hello there")
        ]
      )
    , ( "removeClass multiple classes and attributes"
      , (removeClass "test")
      , [ ("class", "test hello how are you multiple classesh hehe haha")
        , ("test", "some other attribute")
        , ("test2", "some other attribute2")
        , ("hello", "hello there")
        ]
      )
    ]
  where
    build (label, f, args) = bench label $ nf f args
