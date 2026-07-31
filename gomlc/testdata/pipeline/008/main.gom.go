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
        var t156 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t156)
    case B:
        var x152 bool = t__0.(B)._0
        switch x152 {
        case true:
            var t160 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t160)
        case false:
            var t162 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t162)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv168 string
    var t169 string = _goml_runtime_core_int_to_string(self__5)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv171 string
    retv171 = self__38
    return retv171
}

func main() {
    main0()
}
