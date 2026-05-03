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
    var retv2 bool
    var jp4 bool
    switch color__0 {
    case Color_Red:
        jp4 = true
    case Green:
        jp4 = false
    default:
        panic("non-exhaustive match")
    }
    retv2 = jp4
    return retv2
}

func toggle_signal(signal__1 Signal) Signal {
    var retv6 Signal
    var jp8 Signal
    switch signal__1 {
    case Signal_Red:
        jp8 = Yellow
    case Yellow:
        jp8 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv6 = jp8
    return retv6
}

func main0() Signal {
    var retv10 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t11 Signal = toggle_signal(Signal_Red)
    retv10 = t11
    return retv10
}

func main() {
    main0()
}
