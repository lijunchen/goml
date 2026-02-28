package main

type S struct {}

func _goml_inherent_S_S_method1(self__0 S) struct{} {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var s__2 S
    var t0 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            s__2 = S{}
            t0 = _goml_inherent_S_S_method1(s__2)
            return t0
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
