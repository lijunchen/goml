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
        var t160 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t160)
    case B:
        var x152 bool = t__0.(B)._0
        var x153 bool = t__0.(B)._1
        switch x153 {
        case true:
            switch x152 {
            case true:
                var t164 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t164)
            case false:
                var t166 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
                println__T_string(t166)
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x152 {
            case true:
                var t169 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t169)
            case false:
                var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
                println__T_string(t171)
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
    var t174 T = B{
        _0: true,
        _1: true,
    }
    test(t174)
    var t175 T = B{
        _0: false,
        _1: true,
    }
    test(t175)
    var t176 T = B{
        _0: false,
        _1: false,
    }
    test(t176)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t178 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv181 string
    var t182 string = _goml_runtime_core_int_to_string(self__5)
    retv181 = t182
    return retv181
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func main() {
    main0()
}
