import Criterion.Main

import PiForall.Unbound.Modules qualified as UnPiForall
import PiForall.AutoEnv.Modules qualified as AutoPiForall

-- TODO: suppress IO

main =
  defaultMain
    [ bgroup
        "Lennart"
        [  bench "Unbound" $ nfIO (UnPiForall.goFilename "pi/examples/Lennart.pi"),
           bench "AutoEnv" $ nfIO (AutoPiForall.goFilename "pi/examples/Lennart.pi")
        ]
    ]