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

func _goml_trait_impl_Size_Wrap_x5b_int32_x5d__size(self__0 Wrap__int32) int32 {
    var ret6 int32
    ret6 = 10
    return ret6
}

func _goml_trait_impl_Size_Wrap_x5b_string_x5d__size(self__1 Wrap__string) int32 {
    var ret7 int32
    ret7 = 20
    return ret7
}

func main0() struct{} {
    var ret8 struct{}
    var a__2 Wrap__int32 = Wrap__int32{
        value: 1,
    }
    var b__3 Wrap__string = Wrap__string{
        value: "x",
    }
    var t3 int32 = _goml_trait_impl_Size_Wrap_x5b_int32_x5d__size(a__2)
    var t2 string = int32_to_string(t3)
    println__T_string(t2)
    var t5 int32 = _goml_trait_impl_Size_Wrap_x5b_string_x5d__size(b__3)
    var t4 string = int32_to_string(t5)
    println__T_string(t4)
    ret8 = struct{}{}
    return ret8
}

func println__T_string(value__1 string) struct{} {
    var ret9 struct{}
    ret9 = string_println(value__1)
    return ret9
}

func main() {
    main0()
}
