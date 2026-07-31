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
    var retv154 bool
    var jp156 bool
    switch color__0 {
    case Color_Red:
        jp156 = true
    case Green:
        jp156 = false
    default:
        panic("non-exhaustive match")
    }
    retv154 = jp156
    return retv154
}

func toggle_signal(signal__1 Signal) Signal {
    var retv158 Signal
    var jp160 Signal
    switch signal__1 {
    case Signal_Red:
        jp160 = Yellow
    case Yellow:
        jp160 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv158 = jp160
    return retv158
}

func main0() Signal {
    var retv162 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t163 Signal = toggle_signal(Signal_Red)
    retv162 = t163
    return retv162
}

func main() {
    main0()
}
