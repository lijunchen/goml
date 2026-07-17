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
    var t64 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__0)
    _goml_runtime_core_string_print(t64)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t67 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t67)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__0)
    _goml_runtime_core_string_print(t70)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__32 struct{}) string {
    var retv73 string
    var t74 string = _goml_runtime_core_unit_to_string(self__32)
    retv73 = t74
    return retv73
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv76 string
    var t77 string = _goml_runtime_core_bool_to_string(self__33)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__38)
    retv79 = t80
    return retv79
}

func main() {
    main0()
}
