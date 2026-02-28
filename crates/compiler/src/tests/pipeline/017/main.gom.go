package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_int32_int32 struct {
    _0 int32
    _1 int32
}

func _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(self__0 Tuple2_int32_int32) string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "(?, ?)"
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var x__1 int32
    var t3 string
    var mtmp0 struct{}
    var x__2 bool
    var t4 string
    var mtmp1 struct{}
    var x__3 Tuple2_int32_int32
    var t5 string
    var mtmp2 struct{}
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x__1 = 123
            t3 = int32_to_string(x__1)
            string_println(t3)
            x__2 = true
            t4 = bool_to_string(x__2)
            string_println(t4)
            x__3 = Tuple2_int32_int32{
                _0: 3,
                _1: 4,
            }
            t5 = _goml_trait_impl_ToString__x28_int32_x2c_int32_x29__to_string(x__3)
            string_println(t5)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
