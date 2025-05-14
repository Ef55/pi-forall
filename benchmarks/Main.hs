import Criterion.Main
import PiForall.AutoEnv.Modules qualified as AutoPiForall
import PiForall.Unbound.Modules qualified as UnPiForall

-- TODO: suppress IO

prefixes = ["pi/std"]

onImplementation :: String -> ([String] -> String -> IO ()) -> Benchmark
onImplementation group run =
  bgroup
    group
    [ bgroup
        "Compute"
        [ b "Lennart" "pi/examples/cLennart.pi",
          b "Compiler" "pi/examples/cCompiler.pi"
        ],
      bgroup
        "Typechecking"
        [ b "Compiler" "pi/examples/Compiler.pi",
        -- TODO: fix bug in unbound version
          b "AVL" "pi/examples/AVL.pi"
        ]
    ]
  where
    b name path = bench name $ nfIO (run prefixes path)

main =
  defaultMain
    [ onImplementation "Unbound" UnPiForall.goFilename,
      onImplementation "Autoenv" AutoPiForall.goFilename
    ]