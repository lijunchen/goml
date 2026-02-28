package main

type Color interface {
    isColor()
}

type Color_Red struct {}

func (_ Color_Red) isColor() {}

type Green struct {}

func (_ Green) isColor() {}

type Signal interface {
    isSignal()
}

type Signal_Red struct {}

func (_ Signal_Red) isSignal() {}

type Yellow struct {}

func (_ Yellow) isSignal() {}

func color_is_red(color__0 Color) bool {
    var jp2 bool
    switch color__0.(type) {
    case Color_Red:
        jp2 = true
    case Green:
        jp2 = false
    default:
        panic("non-exhaustive match")
    }
    return jp2
}

func toggle_signal(signal__1 Signal) Signal {
    var jp4 Signal
    switch signal__1.(type) {
    case Signal_Red:
        jp4 = Yellow{}
    case Yellow:
        jp4 = Signal_Red{}
    default:
        panic("non-exhaustive match")
    }
    return jp4
}

func main0() Signal {
    var current__2 Color = Color_Red{}
    color_is_red(current__2)
    var t5 Signal = toggle_signal(Signal_Red{})
    return t5
}

func main() {
    main0()
}
