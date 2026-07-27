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
        var t72 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t72)
    case B:
        var x64 bool = t__0.(B)._0
        var x65 bool = t__0.(B)._1
        switch x65 {
        case true:
            switch x64 {
            case true:
                var t76 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t76)
            case false:
                var t78 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
                println__T_string(t78)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x64 {
            case true:
                var t81 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t81)
            case false:
                var t83 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
                println__T_string(t83)
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
    var t86 T = B{
        _0: true,
        _1: true,
    }
    test(t86)
    var t87 T = B{
        _0: false,
        _1: true,
    }
    test(t87)
    var t88 T = B{
        _0: false,
        _1: false,
    }
    test(t88)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int_to_string(self__5)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv96 string
    retv96 = self__38
    return retv96
}

func main() {
    main0()
}
