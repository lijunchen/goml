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
    var retv24 string
    var t25 int32 = self__0.value
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    var t27 string = "S(" + t26
    var t28 string = t27 + ")"
    retv24 = t28
    return retv24
}

func main0() struct{} {
    var s__1 S = S{
        value: 7,
    }
    var t30 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    println__T_string(t30)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv32 string
    var t33 string = _goml_runtime_core_int32_to_string(self__2)
    retv32 = t33
    return retv32
}

func println__T_string(value__1 string) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv38 string
    retv38 = self__9
    return retv38
}

func main() {
    main0()
}
