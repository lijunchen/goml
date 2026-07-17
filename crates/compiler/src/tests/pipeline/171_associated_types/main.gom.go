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
    var retv62 int32
    var t63 int32 = self__0.value
    retv62 = t63
    return retv62
}

func main0() struct{} {
    var t65 Number = Number{
        value: 42,
    }
    var t66 int32 = read__P_Number(t65)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    _goml_runtime_core_string_println(t67)
    var t68 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t68)
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t69)
    var t70 Box__string = Box__string{
        value: "generic",
    }
    var t71 string = _goml_m_read____P__Box_l_string_r_(t70)
    _goml_runtime_core_string_println(t71)
    var t72 Number = Number{
        value: 11,
    }
    var t73 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t72)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv77 int32
    var t78 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv77 = t78
    return retv77
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int32_to_string(self__2)
    retv80 = t81
    return retv80
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv83 int32
    var t84 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv83 = t84
    return retv83
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv86 string
    var t87 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv89 string
    var t90 string = self__1.value
    retv89 = t90
    return retv89
}

func main() {
    main0()
}
