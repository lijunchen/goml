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
    var retv63 string
    var t64 int32 = self__0.value
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t64)
    var t66 string = "S(" + t65
    var t67 string = t66 + ")"
    retv63 = t67
    return retv63
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t69 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t69)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__5)
    retv71 = t72
    return retv71
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv77 string
    retv77 = self__37
    return retv77
}

func main() {
    main0()
}
