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
    var retv66 bool
    var jp68 bool
    switch color__0 {
    case Color_Red:
        jp68 = true
    case Green:
        jp68 = false
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func toggle_signal(signal__1 Signal) Signal {
    var retv70 Signal
    var jp72 Signal
    switch signal__1 {
    case Signal_Red:
        jp72 = Yellow
    case Yellow:
        jp72 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv70 = jp72
    return retv70
}

func main0() Signal {
    var retv74 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t75 Signal = toggle_signal(Signal_Red)
    retv74 = t75
    return retv74
}

func main() {
    main0()
}
