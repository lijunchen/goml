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
    var x182 bool = true
    var x183 bool = false
    var jp191 Tuple2_4bool_4bool
    switch x183 {
    case true:
        switch x182 {
        case true:
            var t207 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp191 = t207
        case false:
            var t208 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp191 = t208
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x182 {
        case true:
            var t211 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp191 = t211
        case false:
            var t212 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp191 = t212
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x185 bool = jp191._1
    var x187 bool = true
    switch x185 {
    case true:
        switch x187 {
        case true:
            var t196 string
            var inline228 int = 3
            var inline229 string = _goml_runtime_core_int_to_string(inline228)
            t196 = inline229
            var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline225)
        case false:
            var t198 string
            var inline234 int = 1
            var inline235 string = _goml_runtime_core_int_to_string(inline234)
            t198 = inline235
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
            _goml_runtime_core_string_println(inline231)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x187 {
        case true:
            var t201 string
            var inline240 int = 2
            var inline241 string = _goml_runtime_core_int_to_string(inline240)
            t201 = inline241
            var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline237)
        case false:
            var t203 string
            var inline246 int = 0
            var inline247 string = _goml_runtime_core_int_to_string(inline246)
            t203 = inline247
            var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline243)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t193 string
    var inline252 string = _goml_runtime_core_unit_to_string(c__4)
    t193 = inline252
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline249)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
