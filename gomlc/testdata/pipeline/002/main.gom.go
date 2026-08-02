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
    var x155 bool = a__0._0
    var x156 bool = a__0._1
    var jp164 Tuple2_4bool_4bool
    switch x156 {
    case true:
        var jp179 Tuple2_4bool_4bool
        switch x155 {
        case true:
            var t180 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp179 = t180
        case false:
            var t181 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp179 = t181
        default:
            panic("non-exhaustive match")
        }
        jp164 = jp179
    case false:
        var jp183 Tuple2_4bool_4bool
        switch x155 {
        case true:
            var t184 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp183 = t184
        case false:
            var t185 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp183 = t185
        default:
            panic("non-exhaustive match")
        }
        jp164 = jp183
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp164
    var x158 bool = b__1._1
    var w__2 bool = x158
    var b_1__3 bool = w__2
    var mtmp159 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x160 bool = mtmp159._0
    var x161 bool = mtmp159._1
    switch x161 {
    case true:
        switch x160 {
        case true:
            var t169 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t169)
        case false:
            var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t171)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x160 {
        case true:
            var t174 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t174)
        case false:
            var t176 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t176)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t166 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t166)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv190 string
    var t191 string = _goml_runtime_core_int_to_string(self__5)
    retv190 = t191
    return retv190
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv193 string
    var t194 string = _goml_runtime_core_unit_to_string(self__36)
    retv193 = t194
    return retv193
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv196 string
    retv196 = self__38
    return retv196
}

func main() {
    main0()
}
