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
    var retv62 string
    retv62 = "(?, ?)"
    return retv62
}

func main0() struct{} {
    var x__1 int32 = 123
    var t64 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__1)
    println__T_string(t64)
    var x__2 bool = true
    var t65 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t65)
    var x__3 Tuple2_5int32_5int32 = Tuple2_5int32_5int32{
        _0: 3,
        _1: 4,
    }
    var t66 string = _goml_m_trait__impl_i_ToString_i__o_int32_c_int32_q__i_to__string(x__3)
    println__T_string(t66)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t68)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__38)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv74 string
    var t75 string = _goml_runtime_core_bool_to_string(self__33)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv77 string
    retv77 = self__34
    return retv77
}

func main() {
    main0()
}
