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
    var ret6 int32
    ret6 = self__0 + other__1
    return ret6
}

func _goml_trait_impl_Arith_int32_less(self__2 int32, other__3 int32) bool {
    var ret7 bool
    ret7 = self__2 < other__3
    return ret7
}

func _goml_trait_impl_Output_int32_output(self__4 int32) struct{} {
    var ret8 struct{}
    var t4 string = int32_to_string(self__4)
    ret8 = string_println(t4)
    return ret8
}

func _goml_trait_impl_Output_bool_output(self__5 bool) struct{} {
    var ret9 struct{}
    var t5 string = bool_to_string(self__5)
    ret9 = string_println(t5)
    return ret9
}

func main0() struct{} {
    var ret10 struct{}
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
    ret10 = struct{}{}
    return ret10
}

func id__T_int32(x__6 int32) int32 {
    var ret11 int32
    ret11 = x__6
    return ret11
}

func id__T_string(x__6 string) string {
    var ret12 string
    ret12 = x__6
    return ret12
}

func id__T_bool(x__6 bool) bool {
    var ret13 bool
    ret13 = x__6
    return ret13
}

func main() {
    main0()
}
