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
    var x187 bool = true
    var x188 bool = false
    var jp196 Tuple2_4bool_4bool
    switch x188 {
    case true:
        switch x187 {
        case true:
            var t212 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp196 = t212
        case false:
            var t213 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp196 = t213
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x187 {
        case true:
            var t216 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp196 = t216
        case false:
            var t217 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp196 = t217
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var x190 bool = jp196._1
    var x192 bool = true
    switch x190 {
    case true:
        switch x192 {
        case true:
            var t201 string
            var inline233 int = 3
            var inline234 string = _goml_runtime_core_int_to_string(inline233)
            t201 = inline234
            var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
            _goml_runtime_core_string_println(inline230)
        case false:
            var t203 string
            var inline239 int = 1
            var inline240 string = _goml_runtime_core_int_to_string(inline239)
            t203 = inline240
            var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
            _goml_runtime_core_string_println(inline236)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x192 {
        case true:
            var t206 string
            var inline245 int = 2
            var inline246 string = _goml_runtime_core_int_to_string(inline245)
            t206 = inline246
            var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline242)
        case false:
            var t208 string
            var inline251 int = 0
            var inline252 string = _goml_runtime_core_int_to_string(inline251)
            t208 = inline252
            var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline248)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t198 string
    var inline257 string = _goml_runtime_core_unit_to_string(c__4)
    t198 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
