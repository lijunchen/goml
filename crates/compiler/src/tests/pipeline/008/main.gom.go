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
        var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t65)
    case B:
        var x61 bool = t__0.(B)._0
        switch x61 {
        case true:
            var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t69)
        case false:
            var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t71)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv77 string
    var t78 string = _goml_runtime_core_int32_to_string(self__5)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv80 string
    retv80 = self__37
    return retv80
}

func main() {
    main0()
}
