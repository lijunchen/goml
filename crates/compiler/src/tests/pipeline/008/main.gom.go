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
        var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t62)
    case B:
        var x58 bool = t__0.(B)._0
        switch x58 {
        case true:
            var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t66)
        case false:
            var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t68)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__2)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv77 string
    retv77 = self__34
    return retv77
}

func main() {
    main0()
}
