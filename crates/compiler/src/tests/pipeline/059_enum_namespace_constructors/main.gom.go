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
        goto b2
    case Green:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp2
    b2:
    jp2 = true
    goto b1
    b3:
    jp2 = false
    goto b1
}

func toggle_signal(signal__1 Signal) Signal {
    var jp4 Signal
    switch signal__1.(type) {
    case Signal_Red:
        goto b2
    case Yellow:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp4
    b2:
    jp4 = Yellow{}
    goto b1
    b3:
    jp4 = Signal_Red{}
    goto b1
}

func main0() Signal {
    var current__2 Color
    var t5 Signal
    current__2 = Color_Red{}
    color_is_red(current__2)
    t5 = toggle_signal(Signal_Red{})
    return t5
}

func main() {
    main0()
}
