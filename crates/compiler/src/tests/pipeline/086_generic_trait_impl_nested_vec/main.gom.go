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
    var a__1 Wrap__int32 = Wrap__int32{
        value: 1,
    }
    var b__2 Wrap__string = Wrap__string{
        value: "x",
    }
    var t2 int32 = _goml_trait_impl_Size_Wrap__int32_size__T_int32(a__1)
    var t3 string = int32_to_string(t2)
    println__T_string(t3)
    var t4 int32 = _goml_trait_impl_Size_Wrap__string_size__T_string(b__2)
    var t5 string = int32_to_string(t4)
    println__T_string(t5)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t6 struct{} = string_println(value__1)
    return t6
}

func _goml_trait_impl_Size_Wrap__int32_size__T_int32(self__0 Wrap__int32) int32 {
    return 1
}

func _goml_trait_impl_Size_Wrap__string_size__T_string(self__0 Wrap__string) int32 {
    return 1
}

func main() {
    main0()
}
