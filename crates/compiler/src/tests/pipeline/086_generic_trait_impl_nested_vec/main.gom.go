package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Wrap__int32 struct {
    value int32
}

type Wrap__string struct {
    value string
}

func main0() struct{} {
    var a__1 Wrap__int32 = Wrap__int32{
        value: 1,
    }
    var b__2 Wrap__string = Wrap__string{
        value: "x",
    }
    var t3 int32 = _goml_trait_x5f_impl_x23_Size_x23_Wrap_x5f__x5f_int32_x23_size(a__1)
    var t4 string = int32_to_string(t3)
    println__T_string(t4)
    var t5 int32 = _goml_trait_x5f_impl_x23_Size_x23_Wrap_x5f__x5f_string_x23_size(b__2)
    var t6 string = int32_to_string(t5)
    println__T_string(t6)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Size_x23_Wrap_x5f__x5f_int32_x23_size(self__0 Wrap__int32) int32 {
    var retv10 int32
    retv10 = 1
    return retv10
}

func _goml_trait_x5f_impl_x23_Size_x23_Wrap_x5f__x5f_string_x23_size(self__0 Wrap__string) int32 {
    var retv12 int32
    retv12 = 1
    return retv12
}

func main() {
    main0()
}
