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

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var a__0 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: true,
    }
    var x58 bool = a__0._0
    var x59 bool = a__0._1
    switch x59 {
    case true:
        switch x58 {
        case true:
            var t63 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t63)
        case false:
            var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(456)
            println__T_string(t65)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x58 {
        case true:
            var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(123)
            println__T_string(t68)
        case false:
            var t70 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t70)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__2)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv79 string
    retv79 = self__34
    return retv79
}

func main() {
    main0()
}
