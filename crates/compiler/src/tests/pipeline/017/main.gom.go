package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_5int32_5int32 struct {
    _0 int32
    _1 int32
}

func _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(self__0 Tuple2_5int32_5int32) string {
    var retv26 string
    retv26 = "(?, ?)"
    return retv26
}

func main0() struct{} {
    var x__1 int32 = 123
    var t28 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__1)
    println__T_string(t28)
    var x__2 bool = true
    var t29 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t29)
    var x__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 3,
        _1: 4,
    }
    var t30 string = _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(x__3)
    println__T_string(t30)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t32)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv35 string
    var t36 string = _goml_runtime_core_int32_to_string(self__13)
    retv35 = t36
    return retv35
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv38 string
    var t39 string = _goml_runtime_core_bool_to_string(self__8)
    retv38 = t39
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv41 string
    retv41 = self__9
    return retv41
}

func main() {
    main0()
}
