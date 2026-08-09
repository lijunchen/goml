package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Option__char interface {
    isOption__char()
}

type None struct {}

func (_ None) isOption__char() {}

type Some struct {
    _0 rune
}

func (_ Some) isOption__char() {}

func show_scalar(value__0 uint32) struct{} {
    var commute_field307 rune
    var inline239 bool = utf8_valid_scalar(value__0)
    if inline239 {
        var inline240 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline241 rune = inline240._1
        commute_field307 = inline241
        var t187 uint32 = uint32(rune(commute_field307))
        var inline236 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t187)
        _goml_runtime_core_string_println(inline236)
        return struct{}{}
    } else {
        var inline232 string = "none"
        var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline232)
        _goml_runtime_core_string_println(inline233)
        return struct{}{}
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    show_scalar(57343)
    var inline277 uint32 = 57344
    var inline278 Option__char = char_from_uint32(inline277)
    switch inline278.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline280 rune = inline278.(Some)._0
        var inline282 uint32 = uint32(rune(inline280))
        println__T_uint32(inline282)
    default:
        panic("non-exhaustive match")
    }
    var inline269 uint32 = 1114111
    var inline270 Option__char = char_from_uint32(inline269)
    switch inline270.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline272 rune = inline270.(Some)._0
        var inline274 uint32 = uint32(rune(inline272))
        println__T_uint32(inline274)
    default:
        panic("non-exhaustive match")
    }
    var inline261 uint32 = 1114112
    var inline262 Option__char = char_from_uint32(inline261)
    switch inline262.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline264 rune = inline262.(Some)._0
        var inline266 uint32 = uint32(rune(inline264))
        println__T_uint32(inline266)
    default:
        panic("non-exhaustive match")
    }
    var commute_field310 rune
    var inline254 uint32 = 128512
    var inline255 bool = utf8_valid_scalar(inline254)
    if inline255 {
        var inline256 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline254)
        var inline257 rune = inline256._1
        commute_field310 = inline257
        var t192 string
        var inline252 string = char_to_string(commute_field310)
        t192 = inline252
        var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
        _goml_runtime_core_string_println(inline249)
        return struct{}{}
    } else {
        var inline245 string = "none"
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
        _goml_runtime_core_string_println(inline246)
        return struct{}{}
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t198 bool
    var inline285 bool = value__32 <= 1114111
    if inline285 {
        var inline286 bool = value__32 >= 55296
        var inline288 bool
        if inline286 {
            var inline290 bool = value__32 <= 57343
            inline288 = inline290
        } else {
            inline288 = false
        }
        var inline289 bool = !inline288
        t198 = inline289
    } else {
        t198 = false
    }
    if t198 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t199 Option__char = Some{
            _0: x24,
        }
        return t199
    } else {
        return None{}
    }
}

func println__T_string(value__31 string) struct{} {
    var t201 string
    t201 = value__31
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t204 string
    var inline293 string = _goml_runtime_core_uint32_to_string(value__31)
    t204 = inline293
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t213 bool = value__4 <= 1114111
    if t213 {
        var t217 bool = value__4 >= 55296
        var jp215 bool
        if t217 {
            var t218 bool = value__4 <= 57343
            jp215 = t218
        } else {
            jp215 = false
        }
        var t216 bool = !jp215
        return t216
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__76 uint32) string {
    var t223 string = _goml_runtime_core_uint32_to_string(self__76)
    return t223
}

func char_to_string(value__29 rune) string {
    var t228 uint32 = uint32(rune(value__29))
    var t229 bool
    var inline300 bool = t228 <= 1114111
    if inline300 {
        var inline301 bool = t228 >= 55296
        var inline303 bool
        if inline301 {
            var inline305 bool = t228 <= 57343
            inline303 = inline305
        } else {
            inline303 = false
        }
        var inline304 bool = !inline303
        t229 = inline304
    } else {
        t229 = false
    }
    if t229 {
        var t230 string = _goml_runtime_core_char_to_string(value__29)
        return t230
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
