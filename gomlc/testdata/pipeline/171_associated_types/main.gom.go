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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var retv159 int32
    var t160 int32 = self__0.value
    retv159 = t160
    return retv159
}

func main0() struct{} {
    var t162 Number = Number{
        value: 42,
    }
    var t163 int32 = read__P_Number(t162)
    var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t163)
    _goml_runtime_core_string_println(t164)
    var t165 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t165)
    var t166 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t166)
    var t167 Box__string = Box__string{
        value: "generic",
    }
    var t168 string = _goml_m_read____P__Box_l_string_r_(t167)
    _goml_runtime_core_string_println(t168)
    var t169 Number = Number{
        value: 11,
    }
    var t170 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t169)
    var t171 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t170)
    _goml_runtime_core_string_println(t171)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv174 int32
    var t175 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv174 = t175
    return retv174
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv177 string
    var t178 string = _goml_runtime_core_int32_to_string(self__6)
    retv177 = t178
    return retv177
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv180 int32
    var t181 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv180 = t181
    return retv180
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv183 string
    var t184 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv183 = t184
    return retv183
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv186 string
    var t187 string = self__1.value
    retv186 = t187
    return retv186
}

func main() {
    main0()
}
