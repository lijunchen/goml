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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

func _goml_m_trait__impl_i_ToString_i__o_int_c_int_q__i_to__string(self__0 Tuple2_3int_3int) string {
    var retv159 string
    retv159 = "(?, ?)"
    return retv159
}

func main0() struct{} {
    var x__1 int = 123
    var t161 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x__1)
    println__T_string(t161)
    var x__2 bool = true
    var t162 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x__2)
    println__T_string(t162)
    var x__3 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 3,
        _1: 4,
    }
    var t163 string = _goml_m_trait__impl_i_ToString_i__o_int_c_int_q__i_to__string(x__3)
    println__T_string(t163)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv168 string
    var t169 string = _goml_runtime_core_int_to_string(self__40)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv171 string
    var t172 string = _goml_runtime_core_bool_to_string(self__37)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv174 string
    retv174 = self__38
    return retv174
}

func main() {
    main0()
}
