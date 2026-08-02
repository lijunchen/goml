package main

type Color int32

const (
    Color_Red Color = 0
    Green Color = 1
)

type Signal int32

const (
    Yellow Signal = 1
)

func main0() Signal {
    var current__2 Color = Color_Red
    switch current__2 {
    case Color_Red:
        return Yellow
    case Green:
        return Yellow
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
