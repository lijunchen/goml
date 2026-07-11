package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

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

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    print__T_unit(struct{}{})
    print__T_bool(true)
    print__T_bool(false)
    print__T_int32(123)
    return struct{}{}
}

func print__T_unit(value__0 struct{}) struct{} {
    var t28 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__0)
    _goml_runtime_core_string_print(t28)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t31)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t34)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv37 string
    var t38 string = _goml_runtime_core_unit_to_string(self__7)
    retv37 = t38
    return retv37
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv40 string
    var t41 string = _goml_runtime_core_bool_to_string(self__8)
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__13)
    retv43 = t44
    return retv43
}

func main() {
    main0()
}
