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
    var retv70 bool
    var jp72 bool
    switch color__0 {
    case Color_Red:
        jp72 = true
    case Green:
        jp72 = false
    default:
        panic("non-exhaustive match")
    }
    retv70 = jp72
    return retv70
}

func toggle_signal(signal__1 Signal) Signal {
    var retv74 Signal
    var jp76 Signal
    switch signal__1 {
    case Signal_Red:
        jp76 = Yellow
    case Yellow:
        jp76 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv74 = jp76
    return retv74
}

func main0() Signal {
    var retv78 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t79 Signal = toggle_signal(Signal_Red)
    retv78 = t79
    return retv78
}

func main() {
    main0()
}
