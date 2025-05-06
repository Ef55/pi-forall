import Criterion.Main
import PiForall.AutoEnv.Modules qualified as AutoPiForall
import PiForall.Unbound.Modules qualified as UnPiForall

-- TODO: suppress IO

prefixes = ["pi/std"]

b name path =
  bgroup
    name
    [ bench "Unbound" $ nfIO (UnPiForall.goFilename prefixes path),
      bench "AutoEnv" $ nfIO (AutoPiForall.goFilename prefixes path)
    ]

main =
  defaultMain
    [ bgroup
        "/"
        [ bgroup
            "Compute"
            [ b "Lennart" "pi/examples/cLennart.pi",
              b "Compiler" "pi/examples/cCompiler.pi"
            ],
          bgroup
            "Typechecking"
            [ b "Compiler" "pi/examples/Compiler.pi"
            -- TODO: fix bug in unbound version
              --b "AVL" "pi/examples/AVL.pi"
            ]
        ]
    ]