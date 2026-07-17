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
    var retv65 int32
    var t66 int32 = self__0.value
    retv65 = t66
    return retv65
}

func main0() struct{} {
    var t68 Number = Number{
        value: 42,
    }
    var t69 int32 = read__P_Number(t68)
    var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t69)
    _goml_runtime_core_string_println(t70)
    var t71 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t71)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t72)
    var t73 Box__string = Box__string{
        value: "generic",
    }
    var t74 string = _goml_m_read____P__Box_l_string_r_(t73)
    _goml_runtime_core_string_println(t74)
    var t75 Number = Number{
        value: 11,
    }
    var t76 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t75)
    var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t76)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv80 int32
    var t81 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv80 = t81
    return retv80
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv83 string
    var t84 string = _goml_runtime_core_int32_to_string(self__5)
    retv83 = t84
    return retv83
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv86 int32
    var t87 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv86 = t87
    return retv86
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv89 string
    var t90 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv89 = t90
    return retv89
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv92 string
    var t93 string = self__1.value
    retv92 = t93
    return retv92
}

func main() {
    main0()
}
