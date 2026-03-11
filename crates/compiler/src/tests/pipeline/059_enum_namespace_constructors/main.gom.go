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

type GoError = error

func color_is_red(color__0 Color) bool {
    var retv2 bool
    var jp4 bool
    switch color__0.(type) {
    case Color_Red:
        jp4 = true
    case Green:
        jp4 = false
    default:
        panic("non-exhaustive match")
    }
    retv2 = jp4
    return retv2
}

func toggle_signal(signal__1 Signal) Signal {
    var retv6 Signal
    var jp8 Signal
    switch signal__1.(type) {
    case Signal_Red:
        jp8 = Yellow{}
    case Yellow:
        jp8 = Signal_Red{}
    default:
        panic("non-exhaustive match")
    }
    retv6 = jp8
    return retv6
}

func main0() Signal {
    var retv10 Signal
    var current__2 Color = Color_Red{}
    color_is_red(current__2)
    var t11 Signal = toggle_signal(Signal_Red{})
    retv10 = t11
    return retv10
}

func main() {
    main0()
}
