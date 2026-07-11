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

func match_int(n__0 int32) int32 {
    var retv15 int32
    var jp17 int32
    switch n__0 {
    case 0:
        jp17 = 10
    case 1:
        jp17 = 20
    default:
        jp17 = 30
    }
    retv15 = jp17
    return retv15
}

func wildcard_first(n__1 int32) int32 {
    var retv19 int32
    retv19 = 40
    return retv19
}

func wildcard_middle(n__2 int32) int32 {
    var retv21 int32
    var jp23 int32
    switch n__2 {
    case 2:
        jp23 = 90
    case 3:
        jp23 = 100
    default:
        jp23 = 100
    }
    retv21 = jp23
    return retv21
}

func repeated(n__3 int32) int32 {
    var retv25 int32
    var jp27 int32
    switch n__3 {
    case 1:
        jp27 = 60
    default:
        jp27 = 80
    }
    retv25 = jp27
    return retv25
}

func main0() struct{} {
    var t29 int32 = match_int(0)
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t29)
    println__T_string(t30)
    var t31 int32 = match_int(5)
    var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t31)
    println__T_string(t32)
    var t33 int32 = wildcard_first(0)
    var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t33)
    println__T_string(t34)
    var t35 int32 = wildcard_first(2)
    var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t35)
    println__T_string(t36)
    var t37 int32 = wildcard_middle(2)
    var t38 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t37)
    println__T_string(t38)
    var t39 int32 = wildcard_middle(3)
    var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t39)
    println__T_string(t40)
    var t41 int32 = repeated(1)
    var t42 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t41)
    println__T_string(t42)
    var t43 int32 = repeated(3)
    var t44 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t43)
    println__T_string(t44)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t47 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t47)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv50 string
    var t51 string = _goml_runtime_core_int32_to_string(self__2)
    retv50 = t51
    return retv50
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv53 string
    retv53 = self__9
    return retv53
}

func main() {
    main0()
}
