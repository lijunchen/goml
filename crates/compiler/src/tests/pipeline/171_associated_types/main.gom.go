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
    var retv68 int32
    var t69 int32 = self__0.value
    retv68 = t69
    return retv68
}

func main0() struct{} {
    var t71 Number = Number{
        value: 42,
    }
    var t72 int32 = read__P_Number(t71)
    var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t72)
    _goml_runtime_core_string_println(t73)
    var t74 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t74)
    var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t75)
    var t76 Box__string = Box__string{
        value: "generic",
    }
    var t77 string = _goml_m_read____P__Box_l_string_r_(t76)
    _goml_runtime_core_string_println(t77)
    var t78 Number = Number{
        value: 11,
    }
    var t79 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t78)
    var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t79)
    _goml_runtime_core_string_println(t80)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv83 int32
    var t84 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv86 string
    var t87 string = _goml_runtime_core_int32_to_string(self__6)
    retv86 = t87
    return retv86
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv89 int32
    var t90 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv89 = t90
    return retv89
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv92 string
    var t93 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv92 = t93
    return retv92
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv95 string
    var t96 string = self__1.value
    retv95 = t96
    return retv95
}

func main() {
    main0()
}
