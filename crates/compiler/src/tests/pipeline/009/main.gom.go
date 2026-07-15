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
    _1 bool
}

func (_ B) isT() {}

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t30)
    case B:
        var x22 bool = t__0.(B)._0
        var x23 bool = t__0.(B)._1
        switch x23 {
        case true:
            switch x22 {
            case true:
                var t34 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t34)
            case false:
                var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
                println__T_string(t36)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x22 {
            case true:
                var t39 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t39)
            case false:
                var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
                println__T_string(t41)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t44 T = B{
        _0: true,
        _1: true,
    }
    test(t44)
    var t45 T = B{
        _0: false,
        _1: true,
    }
    test(t45)
    var t46 T = B{
        _0: false,
        _1: false,
    }
    test(t46)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t48 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t48)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv51 string
    var t52 string = _goml_runtime_core_int32_to_string(self__2)
    retv51 = t52
    return retv51
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv54 string
    retv54 = self__9
    return retv54
}

func main() {
    main0()
}
