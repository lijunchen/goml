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

type Boxed struct {
    value int32
}

func _goml_m_inherent_i_Boxed_i_Boxed_i_format(self__0 Boxed) string {
    var retv64 string
    retv64 = "inherent"
    return retv64
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv66 string
    var t67 int32 = self__1.value
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    retv66 = t68
    return retv66
}

func main0() struct{} {
    var t70 Boxed = Boxed{
        value: 9,
    }
    var t71 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t70)
    println__T_string(t71)
    var t72 Boxed = Boxed{
        value: 9,
    }
    var t73 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t72)
    println__T_string(t73)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__5)
    retv75 = t76
    return retv75
}

func println__T_string(value__1 string) struct{} {
    var t78 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t78)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv81 string
    retv81 = self__37
    return retv81
}

func main() {
    main0()
}
