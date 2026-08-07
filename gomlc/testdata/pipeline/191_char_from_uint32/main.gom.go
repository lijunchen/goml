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
    var commute_field309 rune
    var inline239 bool = utf8_valid_scalar(value__0)
    if inline239 {
        var inline240 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline242 rune = inline240._1
        commute_field309 = inline242
        var t187 uint32 = uint32(rune(commute_field309))
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
    var inline279 uint32 = 57344
    var inline280 Option__char = char_from_uint32(inline279)
    switch inline280.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline282 rune = inline280.(Some)._0
        var inline284 uint32 = uint32(rune(inline282))
        println__T_uint32(inline284)
    default:
        panic("non-exhaustive match")
    }
    var inline271 uint32 = 1114111
    var inline272 Option__char = char_from_uint32(inline271)
    switch inline272.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline274 rune = inline272.(Some)._0
        var inline276 uint32 = uint32(rune(inline274))
        println__T_uint32(inline276)
    default:
        panic("non-exhaustive match")
    }
    var inline263 uint32 = 1114112
    var inline264 Option__char = char_from_uint32(inline263)
    switch inline264.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline266 rune = inline264.(Some)._0
        var inline268 uint32 = uint32(rune(inline266))
        println__T_uint32(inline268)
    default:
        panic("non-exhaustive match")
    }
    var commute_field312 rune
    var inline255 uint32 = 128512
    var inline256 bool = utf8_valid_scalar(inline255)
    if inline256 {
        var inline257 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline255)
        var inline259 rune = inline257._1
        commute_field312 = inline259
        var t192 string
        var inline253 string = char_to_string(commute_field312)
        t192 = inline253
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
        _goml_runtime_core_string_println(inline250)
        return struct{}{}
    } else {
        var inline246 string = "none"
        var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline246)
        _goml_runtime_core_string_println(inline247)
        return struct{}{}
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t198 bool
    var inline287 bool = value__32 <= 1114111
    if inline287 {
        var inline288 bool = value__32 >= 55296
        var inline290 bool
        if inline288 {
            var inline292 bool = value__32 <= 57343
            inline290 = inline292
        } else {
            inline290 = false
        }
        var inline291 bool = !inline290
        t198 = inline291
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
    var inline295 string = _goml_runtime_core_uint32_to_string(value__31)
    t204 = inline295
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
    var inline302 bool = t228 <= 1114111
    if inline302 {
        var inline303 bool = t228 >= 55296
        var inline305 bool
        if inline303 {
            var inline307 bool = t228 <= 57343
            inline305 = inline307
        } else {
            inline305 = false
        }
        var inline306 bool = !inline305
        t229 = inline306
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
