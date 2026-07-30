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
    var retv112 int32
    var t113 int32 = self__0.value
    retv112 = t113
    return retv112
}

func main0() struct{} {
    var t115 Number = Number{
        value: 42,
    }
    var t116 int32 = read__P_Number(t115)
    var t117 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t116)
    _goml_runtime_core_string_println(t117)
    var t118 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t118)
    var t119 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t119)
    var t120 Box__string = Box__string{
        value: "generic",
    }
    var t121 string = _goml_m_read____P__Box_l_string_r_(t120)
    _goml_runtime_core_string_println(t121)
    var t122 Number = Number{
        value: 11,
    }
    var t123 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t122)
    var t124 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t123)
    _goml_runtime_core_string_println(t124)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv127 int32
    var t128 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv127 = t128
    return retv127
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv130 string
    var t131 string = _goml_runtime_core_int32_to_string(self__6)
    retv130 = t131
    return retv130
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv133 int32
    var t134 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv133 = t134
    return retv133
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv136 string
    var t137 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv136 = t137
    return retv136
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv139 string
    var t140 string = self__1.value
    retv139 = t140
    return retv139
}

func main() {
    main0()
}
