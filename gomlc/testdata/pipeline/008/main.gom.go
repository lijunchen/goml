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
        var t159 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t159)
    case B:
        var x155 bool = t__0.(B)._0
        switch x155 {
        case true:
            var t163 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t163)
        case false:
            var t165 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t165)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv171 string
    var t172 string = _goml_runtime_core_int_to_string(self__5)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv174 string
    retv174 = self__38
    return retv174
}

func main() {
    main0()
}
