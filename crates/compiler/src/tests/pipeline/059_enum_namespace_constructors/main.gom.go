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
    var retv63 bool
    var jp65 bool
    switch color__0 {
    case Color_Red:
        jp65 = true
    case Green:
        jp65 = false
    default:
        panic("non-exhaustive match")
    }
    retv63 = jp65
    return retv63
}

func toggle_signal(signal__1 Signal) Signal {
    var retv67 Signal
    var jp69 Signal
    switch signal__1 {
    case Signal_Red:
        jp69 = Yellow
    case Yellow:
        jp69 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv67 = jp69
    return retv67
}

func main0() Signal {
    var retv71 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t72 Signal = toggle_signal(Signal_Red)
    retv71 = t72
    return retv71
}

func main() {
    main0()
}
