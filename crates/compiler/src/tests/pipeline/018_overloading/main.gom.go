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

func goml__trait__impl_x23_Arith_x23_int32_x23_add(self__0 int32, other__1 int32) int32 {
    var ret6 int32
    ret6 = self__0 + other__1
    return ret6
}

func goml__trait__impl_x23_Arith_x23_int32_x23_less(self__2 int32, other__3 int32) bool {
    var ret7 bool
    ret7 = self__2 < other__3
    return ret7
}

func goml__trait__impl_x23_ToString_x23_int32_x23_to__string(self__4 int32) string {
    var ret8 string
    ret8 = int32_to_string(self__4)
    return ret8
}

func goml__trait__impl_x23_ToString_x23_bool_x23_to__string(self__5 bool) string {
    var ret9 string
    ret9 = bool_to_string(self__5)
    return ret9
}

func goml__trait__impl_x23_Output_x23_int32_x23_output(self__6 int32) struct{} {
    var ret10 struct{}
    var t4 string = goml__trait__impl_x23_ToString_x23_int32_x23_to__string(self__6)
    ret10 = string_println(t4)
    return ret10
}

func goml__trait__impl_x23_Output_x23_bool_x23_output(self__7 bool) struct{} {
    var ret11 struct{}
    var t5 string = goml__trait__impl_x23_ToString_x23_bool_x23_to__string(self__7)
    ret11 = string_println(t5)
    return ret11
}

func main0() struct{} {
    var ret12 struct{}
    var a__9 int32 = id__T_int32(1)
    var b__10 int32 = id__T_int32(2)
    var c__11 int32 = goml__trait__impl_x23_Arith_x23_int32_x23_add(a__9, b__10)
    goml__trait__impl_x23_Output_x23_int32_x23_output(c__11)
    var a__12 int32 = id__T_int32(3)
    var b__13 int32 = id__T_int32(4)
    var c__14 bool = goml__trait__impl_x23_Arith_x23_int32_x23_less(a__12, b__13)
    goml__trait__impl_x23_Output_x23_bool_x23_output(c__14)
    id__T_string("abc")
    id__T_bool(true)
    ret12 = struct{}{}
    return ret12
}

func id__T_int32(x__8 int32) int32 {
    var ret13 int32
    ret13 = x__8
    return ret13
}

func id__T_string(x__8 string) string {
    var ret14 string
    ret14 = x__8
    return ret14
}

func id__T_bool(x__8 bool) bool {
    var ret15 bool
    ret15 = x__8
    return ret15
}

func main() {
    main0()
}
