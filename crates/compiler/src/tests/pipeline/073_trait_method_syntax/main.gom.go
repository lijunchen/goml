package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {
    value int32
}

func _goml_trait_impl_ToString_S_to_string(self__0 S) string {
    var t1 int32
    var t2 string
    var t3 string
    var t4 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t1 = self__0.value
            t2 = int32_to_string(t1)
            t3 = "S(" + t2
            t4 = t3 + ")"
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var s__1 S
    var t5 string
    var _wild0 struct{}
    _ = _wild0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            s__1 = S{
                value: 7,
            }
            t5 = _goml_trait_impl_ToString_S_to_string(s__1)
            println__T_string(t5)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t6 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t6 = string_println(value__1)
            return t6
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
