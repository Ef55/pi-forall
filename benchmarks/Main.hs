import Criterion.Main

import Modules qualified as PiForall

main =
  defaultMain
    [ bgroup
        "Lennart"
        [ bench "Lennart" $ nfIO (PiForall.goFilename "pi/Lennart.pi")
        ]
    ]