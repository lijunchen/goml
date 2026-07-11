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
    var retv6 bool
    var jp8 bool
    switch color__0 {
    case Color_Red:
        jp8 = true
    case Green:
        jp8 = false
    default:
        panic("non-exhaustive match")
    }
    retv6 = jp8
    return retv6
}

func toggle_signal(signal__1 Signal) Signal {
    var retv10 Signal
    var jp12 Signal
    switch signal__1 {
    case Signal_Red:
        jp12 = Yellow
    case Yellow:
        jp12 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv10 = jp12
    return retv10
}

func main0() Signal {
    var retv14 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t15 Signal = toggle_signal(Signal_Red)
    retv14 = t15
    return retv14
}

func main() {
    main0()
}
