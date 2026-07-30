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
    var retv110 bool
    var jp112 bool
    switch color__0 {
    case Color_Red:
        jp112 = true
    case Green:
        jp112 = false
    default:
        panic("non-exhaustive match")
    }
    retv110 = jp112
    return retv110
}

func toggle_signal(signal__1 Signal) Signal {
    var retv114 Signal
    var jp116 Signal
    switch signal__1 {
    case Signal_Red:
        jp116 = Yellow
    case Yellow:
        jp116 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv114 = jp116
    return retv114
}

func main0() Signal {
    var retv118 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t119 Signal = toggle_signal(Signal_Red)
    retv118 = t119
    return retv118
}

func main() {
    main0()
}
