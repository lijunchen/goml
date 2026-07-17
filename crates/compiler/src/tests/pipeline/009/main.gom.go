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
        var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(1)
        println__T_string(t66)
    case B:
        var x58 bool = t__0.(B)._0
        var x59 bool = t__0.(B)._1
        switch x59 {
        case true:
            switch x58 {
            case true:
                var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t70)
            case false:
                var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(3)
                println__T_string(t72)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x58 {
            case true:
                var t75 string = _goml_m_inherent_i_int32_i_int32_i_to__string(4)
                println__T_string(t75)
            case false:
                var t77 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
                println__T_string(t77)
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
    var t80 T = B{
        _0: true,
        _1: true,
    }
    test(t80)
    var t81 T = B{
        _0: false,
        _1: true,
    }
    test(t81)
    var t82 T = B{
        _0: false,
        _1: false,
    }
    test(t82)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv87 string
    var t88 string = _goml_runtime_core_int32_to_string(self__2)
    retv87 = t88
    return retv87
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv90 string
    retv90 = self__34
    return retv90
}

func main() {
    main0()
}
