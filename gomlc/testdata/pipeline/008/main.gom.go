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
        var t72 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t72)
    case B:
        var x68 bool = t__0.(B)._0
        switch x68 {
        case true:
            var t76 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t76)
        case false:
            var t78 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t78)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv84 string
    var t85 string = _goml_runtime_core_int_to_string(self__5)
    retv84 = t85
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv87 string
    retv87 = self__38
    return retv87
}

func main() {
    main0()
}
