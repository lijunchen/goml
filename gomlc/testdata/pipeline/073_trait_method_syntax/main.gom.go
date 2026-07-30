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
    var retv70 string
    var t71 int32 = self__0.value
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t71)
    var t73 string = "S(" + t72
    var t74 string = t73 + ")"
    retv70 = t74
    return retv70
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t76 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t76)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__6)
    retv78 = t79
    return retv78
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv84 string
    retv84 = self__38
    return retv84
}

func main() {
    main0()
}
