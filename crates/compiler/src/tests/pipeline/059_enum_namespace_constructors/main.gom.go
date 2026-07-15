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
    var retv24 bool
    var jp26 bool
    switch color__0 {
    case Color_Red:
        jp26 = true
    case Green:
        jp26 = false
    default:
        panic("non-exhaustive match")
    }
    retv24 = jp26
    return retv24
}

func toggle_signal(signal__1 Signal) Signal {
    var retv28 Signal
    var jp30 Signal
    switch signal__1 {
    case Signal_Red:
        jp30 = Yellow
    case Yellow:
        jp30 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv28 = jp30
    return retv28
}

func main0() Signal {
    var retv32 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t33 Signal = toggle_signal(Signal_Red)
    retv32 = t33
    return retv32
}

func main() {
    main0()
}
