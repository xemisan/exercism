module SpaceAge

type Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

let earthYearsToSeconds orbitalPeriod = orbitalPeriod * 365.25 * 24.0 * 60.0 * 60.0

let orbitalPeriod planet = 
    match planet with
    | Earth -> earthYearsToSeconds 1.0
    | Mercury -> earthYearsToSeconds 0.2408467
    | Venus -> earthYearsToSeconds 0.61519726
    | Mars -> earthYearsToSeconds 1.8808158
    | Jupiter -> earthYearsToSeconds 11.862615
    | Saturn -> earthYearsToSeconds 29.447498
    | Uranus -> earthYearsToSeconds 84.016846
    | Neptune -> earthYearsToSeconds 164.79132

let age (planet: Planet) (seconds: int64): float = float seconds / orbitalPeriod planet