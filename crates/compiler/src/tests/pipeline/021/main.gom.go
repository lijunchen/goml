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
    var retv12 int32
    var jp14 int32
    switch n__0 {
    case 0:
        jp14 = 10
    case 1:
        jp14 = 20
    default:
        jp14 = 30
    }
    retv12 = jp14
    return retv12
}

func wildcard_first(n__1 int32) int32 {
    var retv16 int32
    retv16 = 40
    return retv16
}

func wildcard_middle(n__2 int32) int32 {
    var retv18 int32
    var jp20 int32
    switch n__2 {
    case 2:
        jp20 = 90
    case 3:
        jp20 = 100
    default:
        jp20 = 100
    }
    retv18 = jp20
    return retv18
}

func repeated(n__3 int32) int32 {
    var retv22 int32
    var jp24 int32
    switch n__3 {
    case 1:
        jp24 = 60
    default:
        jp24 = 80
    }
    retv22 = jp24
    return retv22
}

func main0() struct{} {
    var t26 int32 = match_int(0)
    var t27 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t26)
    println__T_string(t27)
    var t28 int32 = match_int(5)
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t28)
    println__T_string(t29)
    var t30 int32 = wildcard_first(0)
    var t31 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t30)
    println__T_string(t31)
    var t32 int32 = wildcard_first(2)
    var t33 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t32)
    println__T_string(t33)
    var t34 int32 = wildcard_middle(2)
    var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t34)
    println__T_string(t35)
    var t36 int32 = wildcard_middle(3)
    var t37 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t36)
    println__T_string(t37)
    var t38 int32 = repeated(1)
    var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t38)
    println__T_string(t39)
    var t40 int32 = repeated(3)
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t40)
    println__T_string(t41)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t44 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t44)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv47 string
    var t48 string = _goml_runtime_core_int32_to_string(self__2)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
