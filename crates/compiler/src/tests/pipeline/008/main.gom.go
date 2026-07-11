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
        var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t11)
    case B:
        var x7 bool = t__0.(B)._0
        switch x7 {
        case true:
            var t15 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t15)
        case false:
            var t17 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t17)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t20)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv23 string
    var t24 string = _goml_runtime_core_int32_to_string(self__2)
    retv23 = t24
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv26 string
    retv26 = self__9
    return retv26
}

func main() {
    main0()
}
