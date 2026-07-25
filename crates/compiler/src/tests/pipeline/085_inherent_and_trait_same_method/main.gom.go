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
    var retv67 string
    retv67 = "inherent"
    return retv67
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv69 string
    var t70 int32 = self__1.value
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t70)
    retv69 = t71
    return retv69
}

func main0() struct{} {
    var t73 Boxed = Boxed{
        value: 9,
    }
    var t74 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t73)
    println__T_string(t74)
    var t75 Boxed = Boxed{
        value: 9,
    }
    var t76 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t75)
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
