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
    var x4 bool = a__0._0
    var x5 bool = a__0._1
    switch x5 {
    case true:
        switch x4 {
        case true:
            var t9 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t9)
        case false:
            var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(456)
            println__T_string(t11)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x4 {
        case true:
            var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(123)
            println__T_string(t14)
        case false:
            var t16 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t16)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t19 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t19)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv22 string
    var t23 string = _goml_runtime_core_int32_to_string(self__2)
    retv22 = t23
    return retv22
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
