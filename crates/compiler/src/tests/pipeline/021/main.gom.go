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
    var retv30 int32
    var jp32 int32
    switch n__0 {
    case 0:
        jp32 = 10
    case 1:
        jp32 = 20
    default:
        jp32 = 30
    }
    retv30 = jp32
    return retv30
}

func wildcard_first(n__1 int32) int32 {
    var retv34 int32
    retv34 = 40
    return retv34
}

func wildcard_middle(n__2 int32) int32 {
    var retv36 int32
    var jp38 int32
    switch n__2 {
    case 2:
        jp38 = 90
    case 3:
        jp38 = 100
    default:
        jp38 = 100
    }
    retv36 = jp38
    return retv36
}

func repeated(n__3 int32) int32 {
    var retv40 int32
    var jp42 int32
    switch n__3 {
    case 1:
        jp42 = 60
    default:
        jp42 = 80
    }
    retv40 = jp42
    return retv40
}

func main0() struct{} {
    var t44 int32 = match_int(0)
    var t45 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t44)
    println__T_string(t45)
    var t46 int32 = match_int(5)
    var t47 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t46)
    println__T_string(t47)
    var t48 int32 = wildcard_first(0)
    var t49 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t48)
    println__T_string(t49)
    var t50 int32 = wildcard_first(2)
    var t51 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t50)
    println__T_string(t51)
    var t52 int32 = wildcard_middle(2)
    var t53 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t52)
    println__T_string(t53)
    var t54 int32 = wildcard_middle(3)
    var t55 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t54)
    println__T_string(t55)
    var t56 int32 = repeated(1)
    var t57 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t56)
    println__T_string(t57)
    var t58 int32 = repeated(3)
    var t59 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t58)
    println__T_string(t59)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t62 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t62)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv65 string
    var t66 string = _goml_runtime_core_int32_to_string(self__2)
    retv65 = t66
    return retv65
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv68 string
    retv68 = self__9
    return retv68
}

func main() {
    main0()
}
