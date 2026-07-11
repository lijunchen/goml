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
    var retv26 int32
    var t27 int32 = self__0.value
    retv26 = t27
    return retv26
}

func main0() struct{} {
    var t29 Number = Number{
        value: 42,
    }
    var t30 int32 = read__P_Number(t29)
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t30)
    _goml_runtime_core_string_println(t31)
    var t32 Number = Number{
        value: 7,
    }
    var value__4 int32 = read_as__P_Number__T_int32(t32)
    var t33 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
    _goml_runtime_core_string_println(t33)
    var t34 Box__string = Box__string{
        value: "generic",
    }
    var t35 string = _goml_m_read____P__Box_l_string_r_(t34)
    _goml_runtime_core_string_println(t35)
    var t36 Number = Number{
        value: 11,
    }
    var t37 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t36)
    var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t37)
    _goml_runtime_core_string_println(t38)
    return struct{}{}
}

func read__P_Number(provider__2 Number) int32 {
    var retv41 int32
    var t42 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__2)
    retv41 = t42
    return retv41
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv44 string
    var t45 string = _goml_runtime_core_int32_to_string(self__2)
    retv44 = t45
    return retv44
}

func read_as__P_Number__T_int32(provider__3 Number) int32 {
    var retv47 int32
    var t48 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(provider__3)
    retv47 = t48
    return retv47
}

func _goml_m_read____P__Box_l_string_r_(provider__2 Box__string) string {
    var retv50 string
    var t51 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(provider__2)
    retv50 = t51
    return retv50
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var retv53 string
    var t54 string = self__1.value
    retv53 = t54
    return retv53
}

func main() {
    main0()
}
