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
    var retv71 string
    retv71 = "inherent"
    return retv71
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv73 string
    var t74 int32 = self__1.value
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t74)
    retv73 = t75
    return retv73
}

func main0() struct{} {
    var t77 Boxed = Boxed{
        value: 9,
    }
    var t78 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t77)
    println__T_string(t78)
    var t79 Boxed = Boxed{
        value: 9,
    }
    var t80 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t79)
    println__T_string(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__6)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv88 string
    retv88 = self__38
    return retv88
}

func main() {
    main0()
}
