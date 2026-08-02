package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 struct{}
}

func (_ B) isT() {}

func main0() struct{} {
    var x155 bool = true
    switch x155 {
    case true:
        var t163 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
        println__T_string(t163)
        return struct{}{}
    case false:
        var t165 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
        println__T_string(t165)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t172 string = _goml_runtime_core_int_to_string(self__5)
    return t172
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
