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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch color__0.(type) {
            case Color_Red:
                pc = 2
            case Green:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp2
        case 2:
            jp2 = true
            pc = 1
        case 3:
            jp2 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func toggle_signal(signal__1 Signal) Signal {
    var jp4 Signal
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch signal__1.(type) {
            case Signal_Red:
                pc = 2
            case Yellow:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp4
        case 2:
            jp4 = Yellow{}
            pc = 1
        case 3:
            jp4 = Signal_Red{}
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() Signal {
    var current__2 Color
    var mtmp0 bool
    var t5 Signal
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            current__2 = Color_Red{}
            color_is_red(current__2)
            t5 = toggle_signal(Signal_Red{})
            return t5
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
