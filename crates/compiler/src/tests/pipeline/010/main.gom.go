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
    var x61 bool = a__0._0
    var x62 bool = a__0._1
    switch x62 {
    case true:
        switch x61 {
        case true:
            var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t66)
        case false:
            var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(456)
            println__T_string(t68)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x61 {
        case true:
            var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(123)
            println__T_string(t71)
        case false:
            var t73 string = _goml_m_inherent_i_int32_i_int32_i_to__string(789)
            println__T_string(t73)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv79 string
    var t80 string = _goml_runtime_core_int32_to_string(self__5)
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv82 string
    retv82 = self__37
    return retv82
}

func main() {
    main0()
}
