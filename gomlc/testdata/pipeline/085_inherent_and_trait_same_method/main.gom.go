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
    var retv155 string
    retv155 = "inherent"
    return retv155
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv157 string
    var t158 int32 = self__1.value
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    retv157 = t159
    return retv157
}

func main0() struct{} {
    var t161 Boxed = Boxed{
        value: 9,
    }
    var t162 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t161)
    println__T_string(t162)
    var t163 Boxed = Boxed{
        value: 9,
    }
    var t164 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t163)
    println__T_string(t164)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int32_to_string(self__6)
    retv166 = t167
    return retv166
}

func println__T_string(value__1 string) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv172 string
    retv172 = self__38
    return retv172
}

func main() {
    main0()
}
