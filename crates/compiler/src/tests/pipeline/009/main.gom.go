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
        var t12 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t12)
    case B:
        var x4 bool = t__0.(B)._0
        var x5 bool = t__0.(B)._1
        switch x5 {
        case true:
            switch x4 {
            case true:
                var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t16)
            case false:
                var t18 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
                println__T_string(t18)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x4 {
            case true:
                var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t21)
            case false:
                var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
                println__T_string(t23)
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
    var t26 T = B{
        _0: true,
        _1: true,
    }
    test(t26)
    var t27 T = B{
        _0: false,
        _1: true,
    }
    test(t27)
    var t28 T = B{
        _0: false,
        _1: false,
    }
    test(t28)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv33 string
    var t34 string = _goml_runtime_core_int32_to_string(self__2)
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv36 string
    retv36 = self__9
    return retv36
}

func main() {
    main0()
}
