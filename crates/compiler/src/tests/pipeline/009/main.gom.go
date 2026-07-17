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
        var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t69)
    case B:
        var x61 bool = t__0.(B)._0
        var x62 bool = t__0.(B)._1
        switch x62 {
        case true:
            switch x61 {
            case true:
                var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t73)
            case false:
                var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
                println__T_string(t75)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x61 {
            case true:
                var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t78)
            case false:
                var t80 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
                println__T_string(t80)
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
    var t83 T = B{
        _0: true,
        _1: true,
    }
    test(t83)
    var t84 T = B{
        _0: false,
        _1: true,
    }
    test(t84)
    var t85 T = B{
        _0: false,
        _1: false,
    }
    test(t85)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_int32_to_string(self__5)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv93 string
    retv93 = self__37
    return retv93
}

func main() {
    main0()
}
