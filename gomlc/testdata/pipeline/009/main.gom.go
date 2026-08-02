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
        var t163 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
        println__T_string(t163)
        return struct{}{}
    case B:
        var x155 bool = t__0.(B)._0
        var x156 bool = t__0.(B)._1
        switch x156 {
        case true:
            switch x155 {
            case true:
                var t167 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t167)
                return struct{}{}
            case false:
                var t169 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
                println__T_string(t169)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            switch x155 {
            case true:
                var t172 string = _goml_m_inherent_i_int_i_int_i_to__string(4)
                println__T_string(t172)
                return struct{}{}
            case false:
                var t174 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
                println__T_string(t174)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t177 T = B{
        _0: true,
        _1: true,
    }
    test(t177)
    var t178 T = B{
        _0: false,
        _1: true,
    }
    test(t178)
    var t179 T = B{
        _0: false,
        _1: false,
    }
    test(t179)
    test(A{})
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t185 string = _goml_runtime_core_int_to_string(self__5)
    return t185
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
