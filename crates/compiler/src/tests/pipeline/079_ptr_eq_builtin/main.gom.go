package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

type GoError = error

func main0() struct{} {
    var a__0 *ref_int32_x = ref__Ref_int32(1)
    var b__1 *ref_int32_x = a__0
    var c__2 *ref_int32_x = ref__Ref_int32(1)
    var t3 bool = ptr_eq__Ref_int32(a__0, b__1)
    println__T_bool(t3)
    var t4 bool = ptr_eq__Ref_int32(a__0, c__2)
    println__T_bool(t4)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t6 string = bool_to_string(value__1)
    string_println(t6)
    return struct{}{}
}

func main() {
    main0()
}
