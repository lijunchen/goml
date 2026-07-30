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
    _1 bool
}

func (_ B) isT() {}

func test(t__0 T) struct{} {
    switch t__0.(type) {
    case A:
        var t76 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t76)
    case B:
        var x68 bool = t__0.(B)._0
        var x69 bool = t__0.(B)._1
        switch x69 {
        case true:
            switch x68 {
            case true:
                var t80 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t80)
            case false:
                var t82 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
                println__T_string(t82)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x68 {
            case true:
                var t85 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t85)
            case false:
                var t87 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
                println__T_string(t87)
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
    var t90 T = B{
        _0: true,
        _1: true,
    }
    test(t90)
    var t91 T = B{
        _0: false,
        _1: true,
    }
    test(t91)
    var t92 T = B{
        _0: false,
        _1: false,
    }
    test(t92)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv97 string
    var t98 string = _goml_runtime_core_int_to_string(self__5)
    retv97 = t98
    return retv97
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv100 string
    retv100 = self__38
    return retv100
}

func main() {
    main0()
}
