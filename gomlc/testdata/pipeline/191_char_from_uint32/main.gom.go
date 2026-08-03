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
    var commute_field314 rune
    var inline244 bool = utf8_valid_scalar(value__0)
    if inline244 {
        var inline245 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline247 rune = inline245._1
        commute_field314 = inline247
        var t192 uint32 = uint32(rune(commute_field314))
        var inline241 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t192)
        _goml_runtime_core_string_println(inline241)
        return struct{}{}
    } else {
        var inline237 string = "none"
        var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline237)
        _goml_runtime_core_string_println(inline238)
        return struct{}{}
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    show_scalar(57343)
    var inline284 uint32 = 57344
    var inline285 Option__char = char_from_uint32(inline284)
    switch inline285.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline287 rune = inline285.(Some)._0
        var inline289 uint32 = uint32(rune(inline287))
        println__T_uint32(inline289)
    default:
        panic("non-exhaustive match")
    }
    var inline276 uint32 = 1114111
    var inline277 Option__char = char_from_uint32(inline276)
    switch inline277.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline279 rune = inline277.(Some)._0
        var inline281 uint32 = uint32(rune(inline279))
        println__T_uint32(inline281)
    default:
        panic("non-exhaustive match")
    }
    var inline268 uint32 = 1114112
    var inline269 Option__char = char_from_uint32(inline268)
    switch inline269.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline271 rune = inline269.(Some)._0
        var inline273 uint32 = uint32(rune(inline271))
        println__T_uint32(inline273)
    default:
        panic("non-exhaustive match")
    }
    var commute_field317 rune
    var inline260 uint32 = 128512
    var inline261 bool = utf8_valid_scalar(inline260)
    if inline261 {
        var inline262 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline260)
        var inline264 rune = inline262._1
        commute_field317 = inline264
        var t197 string
        var inline258 string = char_to_string(commute_field317)
        t197 = inline258
        var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
        _goml_runtime_core_string_println(inline255)
        return struct{}{}
    } else {
        var inline251 string = "none"
        var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline251)
        _goml_runtime_core_string_println(inline252)
        return struct{}{}
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t203 bool
    var inline292 bool = value__32 <= 1114111
    if inline292 {
        var inline293 bool = value__32 >= 55296
        var inline295 bool
        if inline293 {
            var inline297 bool = value__32 <= 57343
            inline295 = inline297
        } else {
            inline295 = false
        }
        var inline296 bool = !inline295
        t203 = inline296
    } else {
        t203 = false
    }
    if t203 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t204 Option__char = Some{
            _0: x24,
        }
        return t204
    } else {
        return None{}
    }
}

func println__T_string(value__31 string) struct{} {
    var t206 string
    t206 = value__31
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t209 string
    var inline300 string = _goml_runtime_core_uint32_to_string(value__31)
    t209 = inline300
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t218 bool = value__4 <= 1114111
    if t218 {
        var t222 bool = value__4 >= 55296
        var jp220 bool
        if t222 {
            var t223 bool = value__4 <= 57343
            jp220 = t223
        } else {
            jp220 = false
        }
        var t221 bool = !jp220
        return t221
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__76 uint32) string {
    var t228 string = _goml_runtime_core_uint32_to_string(self__76)
    return t228
}

func char_to_string(value__29 rune) string {
    var t233 uint32 = uint32(rune(value__29))
    var t234 bool
    var inline307 bool = t233 <= 1114111
    if inline307 {
        var inline308 bool = t233 >= 55296
        var inline310 bool
        if inline308 {
            var inline312 bool = t233 <= 57343
            inline310 = inline312
        } else {
            inline310 = false
        }
        var inline311 bool = !inline310
        t234 = inline311
    } else {
        t234 = false
    }
    if t234 {
        var t235 string = _goml_runtime_core_char_to_string(value__29)
        return t235
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
