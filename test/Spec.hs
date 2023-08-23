module Main where

import Run(_main)

iotaFiles :: [String]
iotaFiles =
    [
        "christmas-lamps-on.iota",
        "conflict-example-leave-the-garage-and-enter-the-house.iota",
        "conflict-example-turn-off-the-lights-and-then-immediately-turn-on-the-lights.iota",
        "conflict-example-turn-on-or-turn-off-the-light.iota",

        "conflict-example-turn-on-or-turn-off-the-light.iota",
        "turn-off-hallway-light-five-minutes-after-the-front-door-locks.iota",
        "turn-off-hallway-light-when-all-family-members-are-outside-the-house-for-more-than-ten-minutes.iota",
        "turn-on-hallway-light-when-the-front-door-unlocks.iota",
        "turn-on-siren-when-no-one-is-present.iota"
    ]

iotaDir :: String
iotaDir = ".\\examples\\"

main :: IO ()
main = _main (map (iotaDir ++) iotaFiles)
