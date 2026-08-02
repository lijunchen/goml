package main

type Color int32

const (
    Color_Red Color = 0
    Green Color = 1
)

type Signal int32

const (
    Signal_Red Signal = 0
    Yellow Signal = 1
)

func color_is_red(color__0 Color) bool {
    switch color__0 {
    case Color_Red:
        return true
    case Green:
        return false
    default:
        panic("non-exhaustive match")
    }
}

func toggle_signal(signal__1 Signal) Signal {
    switch signal__1 {
    case Signal_Red:
        return Yellow
    case Yellow:
        return Signal_Red
    default:
        panic("non-exhaustive match")
    }
}

func main0() Signal {
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t166 Signal = toggle_signal(Signal_Red)
    return t166
}

func main() {
    main0()
}
