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
    var retv9 bool
    var jp11 bool
    switch color__0 {
    case Color_Red:
        jp11 = true
    case Green:
        jp11 = false
    default:
        panic("non-exhaustive match")
    }
    retv9 = jp11
    return retv9
}

func toggle_signal(signal__1 Signal) Signal {
    var retv13 Signal
    var jp15 Signal
    switch signal__1 {
    case Signal_Red:
        jp15 = Yellow
    case Yellow:
        jp15 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv13 = jp15
    return retv13
}

func main0() Signal {
    var retv17 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t18 Signal = toggle_signal(Signal_Red)
    retv17 = t18
    return retv17
}

func main() {
    main0()
}
