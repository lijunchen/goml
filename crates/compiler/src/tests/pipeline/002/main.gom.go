package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_int_to_string(x int) string {
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
        _1: false,
    }
    var x64 bool = a__0._0
    var x65 bool = a__0._1
    var jp73 Tuple2_4bool_4bool
    switch x65 {
    case true:
        var jp88 Tuple2_4bool_4bool
        switch x64 {
        case true:
            var t89 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp88 = t89
        case false:
            var t90 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp88 = t90
        default:
            panic("non-exhaustive match")
        }
        jp73 = jp88
    case false:
        var jp92 Tuple2_4bool_4bool
        switch x64 {
        case true:
            var t93 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp92 = t93
        case false:
            var t94 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp92 = t94
        default:
            panic("non-exhaustive match")
        }
        jp73 = jp92
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp73
    var x67 bool = b__1._1
    var w__2 bool = x67
    var b_1__3 bool = w__2
    var mtmp68 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x69 bool = mtmp68._0
    var x70 bool = mtmp68._1
    switch x70 {
    case true:
        switch x69 {
        case true:
            var t78 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t78)
        case false:
            var t80 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t80)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x69 {
        case true:
            var t83 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t83)
        case false:
            var t85 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t85)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t75 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int_to_string(self__5)
    retv99 = t100
    return retv99
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv102 string
    var t103 string = _goml_runtime_core_unit_to_string(self__36)
    retv102 = t103
    return retv102
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv105 string
    retv105 = self__38
    return retv105
}

func main() {
    main0()
}
