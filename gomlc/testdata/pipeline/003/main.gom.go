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
    var t74 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__0)
    _goml_runtime_core_string_print(t74)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t77)
    return struct{}{}
}

func print__T_int(value__0 int) struct{} {
    var t80 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t80)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv83 string
    var t84 string = _goml_runtime_core_unit_to_string(self__36)
    retv83 = t84
    return retv83
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv86 string
    var t87 string = _goml_runtime_core_bool_to_string(self__37)
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv89 string
    var t90 string = _goml_runtime_core_int_to_string(self__40)
    retv89 = t90
    return retv89
}

func main() {
    main0()
}
