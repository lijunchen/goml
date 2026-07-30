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
    var retv72 int32
    var t73 int32 = self__0.value
    retv72 = t73
    return retv72
}

func main0() struct{} {
    var t75 Number = Number{
        value: 42,
    }
    var t76 int32 = read__P_Number(t75)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    _goml_runtime_core_string_println(t77)
    var t78 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t78)
    var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t79)
    var t80 Box__string = Box__string{
        value: "generic",
    }
    var t81 string = _goml_m_read____P__Box_l_string_r_(t80)
    _goml_runtime_core_string_println(t81)
    var t82 Number = Number{
        value: 11,
    }
    var t83 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t82)
    var t84 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t83)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv87 int32
    var t88 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__6)
    retv90 = t91
    return retv90
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv93 int32
    var t94 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv93 = t94
    return retv93
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv96 string
    var t97 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv99 string
    var t100 string = self__1.value
    retv99 = t100
    return retv99
}

func main() {
    main0()
}
