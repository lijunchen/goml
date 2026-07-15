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
        var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t26)
    case B:
        var x22 bool = t__0.(B)._0
        switch x22 {
        case true:
            var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
            println__T_string(t30)
        case false:
            var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
            println__T_string(t32)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int32_to_string(self__2)
    retv38 = t39
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv41 string
    retv41 = self__9
    return retv41
}

func main() {
    main0()
}
