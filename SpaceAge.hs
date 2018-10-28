module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = round2Digits (seconds / (secPerYear planet))

secPerYear :: Planet -> Float
secPerYear x = case x of
    Earth -> 365.25 * 24 * 60 * 60
    Mercury -> 0.2408467 * e
    Venus -> 0.61519726 * e
    Mars -> 1.8808158 * e
    Jupiter -> 11.862615 * e
    Saturn -> 29.447498 * e
    Uranus -> 84.016846 * e
    Neptune -> 164.79132 * e
    where e = secPerYear Earth

round2Digits :: Float -> Float
round2Digits x = fromInteger (round (x * 100)) / 100
