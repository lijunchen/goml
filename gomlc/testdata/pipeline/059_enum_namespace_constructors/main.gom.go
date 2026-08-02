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
    var retv157 bool
    var jp159 bool
    switch color__0 {
    case Color_Red:
        jp159 = true
    case Green:
        jp159 = false
    default:
        panic("non-exhaustive match")
    }
    retv157 = jp159
    return retv157
}

func toggle_signal(signal__1 Signal) Signal {
    var retv161 Signal
    var jp163 Signal
    switch signal__1 {
    case Signal_Red:
        jp163 = Yellow
    case Yellow:
        jp163 = Signal_Red
    default:
        panic("non-exhaustive match")
    }
    retv161 = jp163
    return retv161
}

func main0() Signal {
    var retv165 Signal
    var current__2 Color = Color_Red
    color_is_red(current__2)
    var t166 Signal = toggle_signal(Signal_Red)
    retv165 = t166
    return retv165
}

func main() {
    main0()
}
