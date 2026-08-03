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
    var x136 bool = true
    var x137 bool = false
    var jp145 Tuple2_4bool_4bool
    switch x137 {
    case true:
        switch x136 {
        case true:
            var t161 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp145 = t161
        case false:
            var t162 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp145 = t162
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x136 {
        case true:
            var t165 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp145 = t165
        case false:
            var t166 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp145 = t166
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x139 bool = jp145._1
    var x141 bool = true
    switch x139 {
    case true:
        switch x141 {
        case true:
            var t150 string
            var inline182 int = 3
            var inline183 string = _goml_runtime_core_int_to_string(inline182)
            t150 = inline183
            var inline179 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
            _goml_runtime_core_string_println(inline179)
        case false:
            var t152 string
            var inline188 int = 1
            var inline189 string = _goml_runtime_core_int_to_string(inline188)
            t152 = inline189
            var inline185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t152)
            _goml_runtime_core_string_println(inline185)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x141 {
        case true:
            var t155 string
            var inline194 int = 2
            var inline195 string = _goml_runtime_core_int_to_string(inline194)
            t155 = inline195
            var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
            _goml_runtime_core_string_println(inline191)
        case false:
            var t157 string
            var inline200 int = 0
            var inline201 string = _goml_runtime_core_int_to_string(inline200)
            t157 = inline201
            var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
            _goml_runtime_core_string_println(inline197)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t147 string
    var inline206 string = _goml_runtime_core_unit_to_string(c__4)
    t147 = inline206
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline203)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
