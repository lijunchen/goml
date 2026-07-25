package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type S struct {
    value int32
}

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var retv66 string
    var t67 int32 = self__0.value
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    var t69 string = "S(" + t68
    var t70 string = t69 + ")"
    retv66 = t70
    return retv66
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t72 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t72)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__6)
    retv74 = t75
    return retv74
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv80 string
    retv80 = self__38
    return retv80
}

func main() {
    main0()
}
