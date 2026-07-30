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

func _goml_runtime_core_int_to_string(x int) string {
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
    print__T_int(123)
    return struct{}{}
}

func print__T_unit(value__0 struct{}) struct{} {
    var t114 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__0)
    _goml_runtime_core_string_print(t114)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t117)
    return struct{}{}
}

func print__T_int(value__0 int) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv123 string
    var t124 string = _goml_runtime_core_unit_to_string(self__36)
    retv123 = t124
    return retv123
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv126 string
    var t127 string = _goml_runtime_core_bool_to_string(self__37)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int_to_string(self__40)
    retv129 = t130
    return retv129
}

func main() {
    main0()
}
