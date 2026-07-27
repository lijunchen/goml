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
    var t__0 T = B{
        _0: true,
        _1: struct{}{},
    }
    switch t__0.(type) {
    case A:
        var t68 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t68)
    case B:
        var x64 bool = t__0.(B)._0
        switch x64 {
        case true:
            var t72 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t72)
        case false:
            var t74 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t74)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int_to_string(self__5)
    retv80 = t81
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv83 string
    retv83 = self__38
    return retv83
}

func main() {
    main0()
}
