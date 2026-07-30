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
        var t116 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t116)
    case B:
        var x108 bool = t__0.(B)._0
        var x109 bool = t__0.(B)._1
        switch x109 {
        case true:
            switch x108 {
            case true:
                var t120 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t120)
            case false:
                var t122 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
                println__T_string(t122)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x108 {
            case true:
                var t125 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t125)
            case false:
                var t127 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
                println__T_string(t127)
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
    var t130 T = B{
        _0: true,
        _1: true,
    }
    test(t130)
    var t131 T = B{
        _0: false,
        _1: true,
    }
    test(t131)
    var t132 T = B{
        _0: false,
        _1: false,
    }
    test(t132)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv137 string
    var t138 string = _goml_runtime_core_int_to_string(self__5)
    retv137 = t138
    return retv137
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func main() {
    main0()
}
