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

func _goml_trait_impl_Arith_int32_add(self__0 int32, other__1 int32) int32 {
    var ret17 int32
    ret17 = self__0 + other__1
    return ret17
}

func _goml_trait_impl_Arith_int32_less(self__2 int32, other__3 int32) bool {
    var ret18 bool
    ret18 = self__2 < other__3
    return ret18
}

func _goml_trait_impl_Output_int32_output(self__4 int32) struct{} {
    var ret19 struct{}
    var t15 string = int32_to_string(self__4)
    ret19 = string_println(t15)
    return ret19
}

func _goml_trait_impl_Output_bool_output(self__5 bool) struct{} {
    var ret20 struct{}
    var t16 string = bool_to_string(self__5)
    ret20 = string_println(t16)
    return ret20
}

func main0() struct{} {
    var ret21 struct{}
    var a__7 int32 = id__T_int32(1)
    var b__8 int32 = id__T_int32(2)
    var c__9 int32 = _goml_trait_impl_Arith_int32_add(a__7, b__8)
    _goml_trait_impl_Output_int32_output(c__9)
    var a__10 int32 = id__T_int32(3)
    var b__11 int32 = id__T_int32(4)
    var c__12 bool = _goml_trait_impl_Arith_int32_less(a__10, b__11)
    _goml_trait_impl_Output_bool_output(c__12)
    id__T_string("abc")
    id__T_bool(true)
    ret21 = struct{}{}
    return ret21
}

func id__T_int32(x__6 int32) int32 {
    var ret22 int32
    ret22 = x__6
    return ret22
}

func id__T_string(x__6 string) string {
    var ret23 string
    ret23 = x__6
    return ret23
}

func id__T_bool(x__6 bool) bool {
    var ret24 bool
    ret24 = x__6
    return ret24
}

func main() {
    main0()
}
