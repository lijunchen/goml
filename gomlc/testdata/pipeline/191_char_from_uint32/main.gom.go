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
    var mtmp172 Option__char
    var inline242 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp172 = inline242
    switch mtmp172.(type) {
    case None:
        var inline235 string = "none"
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
        _goml_runtime_core_string_println(inline236)
        return struct{}{}
    case Some:
        var x173 rune = mtmp172.(Some)._0
        var t187 uint32 = uint32(rune(x173))
        var inline239 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t187)
        _goml_runtime_core_string_println(inline239)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    var inline280 uint32 = 57343
    var inline281 Option__char = char_from_uint32(inline280)
    switch inline281.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline283 rune = inline281.(Some)._0
        var inline285 uint32 = uint32(rune(inline283))
        println__T_uint32(inline285)
    default:
        panic("non-exhaustive match")
    }
    var inline272 uint32 = 57344
    var inline273 Option__char = char_from_uint32(inline272)
    switch inline273.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline275 rune = inline273.(Some)._0
        var inline277 uint32 = uint32(rune(inline275))
        println__T_uint32(inline277)
    default:
        panic("non-exhaustive match")
    }
    var inline264 uint32 = 1114111
    var inline265 Option__char = char_from_uint32(inline264)
    switch inline265.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline267 rune = inline265.(Some)._0
        var inline269 uint32 = uint32(rune(inline267))
        println__T_uint32(inline269)
    default:
        panic("non-exhaustive match")
    }
    var inline256 uint32 = 1114112
    var inline257 Option__char = char_from_uint32(inline256)
    switch inline257.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline259 rune = inline257.(Some)._0
        var inline261 uint32 = uint32(rune(inline259))
        println__T_uint32(inline261)
    default:
        panic("non-exhaustive match")
    }
    var mtmp182 Option__char
    var inline253 uint32 = 128512
    var inline254 Option__char = __goml_builtin_char_from_uint32(inline253)
    mtmp182 = inline254
    switch mtmp182.(type) {
    case None:
        var inline244 string = "none"
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline244)
        _goml_runtime_core_string_println(inline245)
        return struct{}{}
    case Some:
        var x183 rune = mtmp182.(Some)._0
        var t192 string
        var inline251 string = char_to_string(x183)
        t192 = inline251
        var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
        _goml_runtime_core_string_println(inline248)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline288 bool = utf8_valid_scalar(value__2)
    if inline288 {
        var inline289 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline290 rune = inline289._1
        var inline292 Option__char = Some{
            _0: inline290,
        }
        return inline292
    } else {
        return None{}
    }
}

func println__T_string(value__1 string) struct{} {
    var t198 string
    t198 = value__1
    _goml_runtime_core_string_println(t198)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t201 string
    var inline295 string = _goml_runtime_core_uint32_to_string(value__1)
    t201 = inline295
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t210 bool
    var inline302 bool = value__30 <= 1114111
    if inline302 {
        var inline303 bool = value__30 >= 55296
        var inline305 bool
        if inline303 {
            var inline307 bool = value__30 <= 57343
            inline305 = inline307
        } else {
            inline305 = false
        }
        var inline306 bool = !inline305
        t210 = inline306
    } else {
        t210 = false
    }
    if t210 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t211 Option__char = Some{
            _0: x24,
        }
        return t211
    } else {
        return None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__74 uint32) string {
    var t216 string = _goml_runtime_core_uint32_to_string(self__74)
    return t216
}

func char_to_string(value__29 rune) string {
    var t221 uint32 = uint32(rune(value__29))
    var t222 bool
    var inline309 bool = t221 <= 1114111
    if inline309 {
        var inline310 bool = t221 >= 55296
        var inline312 bool
        if inline310 {
            var inline314 bool = t221 <= 57343
            inline312 = inline314
        } else {
            inline312 = false
        }
        var inline313 bool = !inline312
        t222 = inline313
    } else {
        t222 = false
    }
    if t222 {
        var t223 string = _goml_runtime_core_char_to_string(value__29)
        return t223
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t228 bool = value__4 <= 1114111
    if t228 {
        var t232 bool = value__4 >= 55296
        var jp230 bool
        if t232 {
            var t233 bool = value__4 <= 57343
            jp230 = t233
        } else {
            jp230 = false
        }
        var t231 bool = !jp230
        return t231
    } else {
        return false
    }
}

func main() {
    main0()
}
