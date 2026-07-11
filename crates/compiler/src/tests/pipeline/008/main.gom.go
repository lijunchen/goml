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
    var t__0 T = B{
        _0: true,
        _1: struct{}{},
    }
    switch t__0.(type) {
    case A:
        var t8 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t8)
    case B:
        var x4 bool = t__0.(B)._0
        switch x4 {
        case true:
            var t12 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t12)
        case false:
            var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t14)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t17 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t17)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv20 string
    var t21 string = _goml_runtime_core_int32_to_string(self__2)
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv23 string
    retv23 = self__9
    return retv23
}

func main() {
    main0()
}
