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
    var retv61 string
    retv61 = "inherent"
    return retv61
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv63 string
    var t64 int32 = self__1.value
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t64)
    retv63 = t65
    return retv63
}

func main0() struct{} {
    var t67 Boxed = Boxed{
        value: 9,
    }
    var t68 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t67)
    println__T_string(t68)
    var t69 Boxed = Boxed{
        value: 9,
    }
    var t70 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t69)
    println__T_string(t70)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv72 string
    var t73 string = _goml_runtime_core_int32_to_string(self__2)
    retv72 = t73
    return retv72
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv78 string
    retv78 = self__34
    return retv78
}

func main() {
    main0()
}
