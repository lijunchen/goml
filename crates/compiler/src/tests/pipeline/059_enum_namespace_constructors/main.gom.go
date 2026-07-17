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
    var retv60 bool
    var jp62 bool
    switch color__0 {
    case Color_Red:
        jp62 = true
    case Green:
        jp62 = false
    default:
        panic("non-exhaustive match")
    }
    retv60 = jp62
    return retv60
}

func toggle_signal(signal__1 Signal) Signal {
    var retv64 Signal
    var jp66 Signal
    switch signal__1 {
    case Signal_Red:
        jp66 = Yellow
    case Yellow:
        jp66 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv64 = jp66
    return retv64
}

func main0() Signal {
    var retv68 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t69 Signal = toggle_signal(Signal_Red)
    retv68 = t69
    return retv68
}

func main() {
    main0()
}
