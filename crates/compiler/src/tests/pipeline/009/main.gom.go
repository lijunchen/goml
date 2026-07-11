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
        var t15 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t15)
    case B:
        var x7 bool = t__0.(B)._0
        var x8 bool = t__0.(B)._1
        switch x8 {
        case true:
            switch x7 {
            case true:
                var t19 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t19)
            case false:
                var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
                println__T_string(t21)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x7 {
            case true:
                var t24 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t24)
            case false:
                var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
                println__T_string(t26)
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
    var t29 T = B{
        _0: true,
        _1: true,
    }
    test(t29)
    var t30 T = B{
        _0: false,
        _1: true,
    }
    test(t30)
    var t31 T = B{
        _0: false,
        _1: false,
    }
    test(t31)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t33 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t33)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__2)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func main() {
    main0()
}
