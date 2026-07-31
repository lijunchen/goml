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
    var x152 bool = a__0._0
    var x153 bool = a__0._1
    var jp161 Tuple2_4bool_4bool
    switch x153 {
    case true:
        var jp176 Tuple2_4bool_4bool
        switch x152 {
        case true:
            var t177 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp176 = t177
        case false:
            var t178 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp176 = t178
        default:
            panic("non-exhaustive match")
        }
        jp161 = jp176
    case false:
        var jp180 Tuple2_4bool_4bool
        switch x152 {
        case true:
            var t181 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp180 = t181
        case false:
            var t182 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp180 = t182
        default:
            panic("non-exhaustive match")
        }
        jp161 = jp180
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp161
    var x155 bool = b__1._1
    var w__2 bool = x155
    var b_1__3 bool = w__2
    var mtmp156 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x157 bool = mtmp156._0
    var x158 bool = mtmp156._1
    switch x158 {
    case true:
        switch x157 {
        case true:
            var t166 string = _goml_m_inherent_i_int_i_int_i_to__string(3)
            println__T_string(t166)
        case false:
            var t168 string = _goml_m_inherent_i_int_i_int_i_to__string(1)
            println__T_string(t168)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x157 {
        case true:
            var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
            println__T_string(t171)
        case false:
            var t173 string = _goml_m_inherent_i_int_i_int_i_to__string(0)
            println__T_string(t173)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t163 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(c__4)
    println__T_string(t163)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv187 string
    var t188 string = _goml_runtime_core_int_to_string(self__5)
    retv187 = t188
    return retv187
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv190 string
    var t191 string = _goml_runtime_core_unit_to_string(self__36)
    retv190 = t191
    return retv190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv193 string
    retv193 = self__38
    return retv193
}

func main() {
    main0()
}
