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
    var x68 bool = a__0._0
    var x69 bool = a__0._1
    var jp77 Tuple2_4bool_4bool
    switch x69 {
    case true:
        var jp92 Tuple2_4bool_4bool
        switch x68 {
        case true:
            var t93 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp92 = t93
        case false:
            var t94 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp92 = t94
        default:
            panic("non-exhaustive match")
        }
        jp77 = jp92
    case false:
        var jp96 Tuple2_4bool_4bool
        switch x68 {
        case true:
            var t97 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp96 = t97
        case false:
            var t98 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp96 = t98
        default:
            panic("non-exhaustive match")
        }
        jp77 = jp96
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp77
    var x71 bool = b__1._1
    var w__2 bool = x71
    var b_1__3 bool = w__2
    var mtmp72 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x73 bool = mtmp72._0
    var x74 bool = mtmp72._1
    switch x74 {
    case true:
        switch x73 {
        case true:
            var t82 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t82)
        case false:
            var t84 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t84)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x73 {
        case true:
            var t87 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t87)
        case false:
            var t89 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t89)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t79 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv103 string
    var t104 string = _goml_runtime_core_int_to_string(self__5)
    retv103 = t104
    return retv103
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv106 string
    var t107 string = _goml_runtime_core_unit_to_string(self__36)
    retv106 = t107
    return retv106
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv109 string
    retv109 = self__38
    return retv109
}

func main() {
    main0()
}
