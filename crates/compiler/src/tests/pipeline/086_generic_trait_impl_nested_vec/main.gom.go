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

type Wrap__int32 struct {
    value int32
}

type Wrap__string struct {
    value string
}

func main0() struct{} {
    var a__1 Wrap__int32
    var b__2 Wrap__string
    var t2 int32
    var t3 string
    var _wild0 struct{}
    var t4 int32
    var t5 string
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__1 = Wrap__int32{
                value: 1,
            }
            b__2 = Wrap__string{
                value: "x",
            }
            t2 = _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_int32(a__1)
            t3 = int32_to_string(t2)
            println__T_string(t3)
            t4 = _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_string(b__2)
            t5 = int32_to_string(t4)
            println__T_string(t5)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_int32(self__0 Wrap__int32) int32 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 1
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

func _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_string(self__0 Wrap__string) int32 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return 1
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
