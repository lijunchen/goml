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
    var retv156 int32
    var t157 int32 = self__0.value
    retv156 = t157
    return retv156
}

func main0() struct{} {
    var t159 Number = Number{
        value: 42,
    }
    var t160 int32 = read__P_Number(t159)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t160)
    _goml_runtime_core_string_println(t161)
    var t162 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t162)
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t163)
    var t164 Box__string = Box__string{
        value: "generic",
    }
    var t165 string = _goml_m_read____P__Box_l_string_r_(t164)
    _goml_runtime_core_string_println(t165)
    var t166 Number = Number{
        value: 11,
    }
    var t167 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t166)
    var t168 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t167)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv171 int32
    var t172 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv174 string
    var t175 string = _goml_runtime_core_int32_to_string(self__6)
    retv174 = t175
    return retv174
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv177 int32
    var t178 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv177 = t178
    return retv177
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv180 string
    var t181 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv180 = t181
    return retv180
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv183 string
    var t184 string = self__1.value
    retv183 = t184
    return retv183
}

func main() {
    main0()
}
