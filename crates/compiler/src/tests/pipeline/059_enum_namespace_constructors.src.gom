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
    var ret2 bool
    switch color__0.(type) {
    case Color_Red:
        ret2 = true
    case Green:
        ret2 = false
    }
    return ret2
}

func toggle_signal(signal__1 Signal) Signal {
    var ret3 Signal
    switch signal__1.(type) {
    case Signal_Red:
        ret3 = Yellow{}
    case Yellow:
        ret3 = Signal_Red{}
    }
    return ret3
}

func main0() Signal {
    var ret4 Signal
    var current__2 Color = Color_Red{}
    color_is_red(current__2)
    var t1 Signal = Signal_Red{}
    ret4 = toggle_signal(t1)
    return ret4
}

func main() {
    main0()
}
