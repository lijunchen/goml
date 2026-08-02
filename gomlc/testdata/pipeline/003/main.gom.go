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
    var t161 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__0)
    _goml_runtime_core_string_print(t161)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t164 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__0)
    _goml_runtime_core_string_print(t164)
    return struct{}{}
}

func print__T_int(value__0 int) struct{} {
    var t167 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__0)
    _goml_runtime_core_string_print(t167)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var t171 string = _goml_runtime_core_unit_to_string(self__36)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t174 string = _goml_runtime_core_bool_to_string(self__37)
    return t174
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t177 string = _goml_runtime_core_int_to_string(self__40)
    return t177
}

func main() {
    main0()
}
