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

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ptr_eq__Ref_int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func main0() struct{} {
    var a__0 *ref_int32_x
    var b__1 *ref_int32_x
    var c__2 *ref_int32_x
    var t2 bool
    var _wild0 struct{}
    var t3 bool
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = ref__Ref_int32(1)
            b__1 = a__0
            c__2 = ref__Ref_int32(1)
            t2 = ptr_eq__Ref_int32(a__0, b__1)
            println__T_bool(t2)
            t3 = ptr_eq__Ref_int32(a__0, c__2)
            println__T_bool(t3)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t4 string
    var t5 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = bool_to_string(value__1)
            t5 = string_println(t4)
            return t5
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
