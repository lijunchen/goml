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
    var ret6 struct{}
    var a__1 Wrap__int32 = Wrap__int32{
        value: 1,
    }
    var b__2 Wrap__string = Wrap__string{
        value: "x",
    }
    var t3 int32 = _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_int32(a__1)
    var t2 string = int32_to_string(t3)
    println__T_string(t2)
    var t5 int32 = _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_string(b__2)
    var t4 string = int32_to_string(t5)
    println__T_string(t4)
    ret6 = struct{}{}
    return ret6
}

func _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_int32(self__0 Wrap__int32) int32 {
    var ret7 int32
    ret7 = 1
    return ret7
}

func println__T_string(value__1 string) struct{} {
    var ret8 struct{}
    ret8 = string_println(value__1)
    return ret8
}

func _goml_trait_impl_Size_Wrap_x5b_T_x5d__size__T_string(self__0 Wrap__string) int32 {
    var ret9 int32
    ret9 = 1
    return ret9
}

func main() {
    main0()
}
