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
    var mtmp182 Option__char
    var inline252 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp182 = inline252
    switch mtmp182.(type) {
    case None:
        var inline245 string = "none"
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
        _goml_runtime_core_string_println(inline246)
        return struct{}{}
    case Some:
        var x183 rune = mtmp182.(Some)._0
        var t197 uint32 = uint32(rune(x183))
        var inline249 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t197)
        _goml_runtime_core_string_println(inline249)
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
    var inline290 uint32 = 57343
    var inline291 Option__char = char_from_uint32(inline290)
    switch inline291.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline293 rune = inline291.(Some)._0
        var inline295 uint32 = uint32(rune(inline293))
        println__T_uint32(inline295)
    default:
        panic("non-exhaustive match")
    }
    var inline282 uint32 = 57344
    var inline283 Option__char = char_from_uint32(inline282)
    switch inline283.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline285 rune = inline283.(Some)._0
        var inline287 uint32 = uint32(rune(inline285))
        println__T_uint32(inline287)
    default:
        panic("non-exhaustive match")
    }
    var inline274 uint32 = 1114111
    var inline275 Option__char = char_from_uint32(inline274)
    switch inline275.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline277 rune = inline275.(Some)._0
        var inline279 uint32 = uint32(rune(inline277))
        println__T_uint32(inline279)
    default:
        panic("non-exhaustive match")
    }
    var inline266 uint32 = 1114112
    var inline267 Option__char = char_from_uint32(inline266)
    switch inline267.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline269 rune = inline267.(Some)._0
        var inline271 uint32 = uint32(rune(inline269))
        println__T_uint32(inline271)
    default:
        panic("non-exhaustive match")
    }
    var mtmp192 Option__char
    var inline263 uint32 = 128512
    var inline264 Option__char = __goml_builtin_char_from_uint32(inline263)
    mtmp192 = inline264
    switch mtmp192.(type) {
    case None:
        var inline254 string = "none"
        var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
        _goml_runtime_core_string_println(inline255)
        return struct{}{}
    case Some:
        var x193 rune = mtmp192.(Some)._0
        var t202 string
        var inline261 string = char_to_string(x193)
        t202 = inline261
        var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
        _goml_runtime_core_string_println(inline258)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline298 bool = utf8_valid_scalar(value__2)
    if inline298 {
        var inline299 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline300 rune = inline299._1
        var inline302 Option__char = Some{
            _0: inline300,
        }
        return inline302
    } else {
        return None{}
    }
}

func println__T_string(value__1 string) struct{} {
    var t208 string
    t208 = value__1
    _goml_runtime_core_string_println(t208)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t211 string
    var inline305 string = _goml_runtime_core_uint32_to_string(value__1)
    t211 = inline305
    _goml_runtime_core_string_println(t211)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t220 bool
    var inline312 bool = value__30 <= 1114111
    if inline312 {
        var inline313 bool = value__30 >= 55296
        var inline315 bool
        if inline313 {
            var inline317 bool = value__30 <= 57343
            inline315 = inline317
        } else {
            inline315 = false
        }
        var inline316 bool = !inline315
        t220 = inline316
    } else {
        t220 = false
    }
    if t220 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t221 Option__char = Some{
            _0: x24,
        }
        return t221
    } else {
        return None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__74 uint32) string {
    var t226 string = _goml_runtime_core_uint32_to_string(self__74)
    return t226
}

func char_to_string(value__29 rune) string {
    var t231 uint32 = uint32(rune(value__29))
    var t232 bool
    var inline319 bool = t231 <= 1114111
    if inline319 {
        var inline320 bool = t231 >= 55296
        var inline322 bool
        if inline320 {
            var inline324 bool = t231 <= 57343
            inline322 = inline324
        } else {
            inline322 = false
        }
        var inline323 bool = !inline322
        t232 = inline323
    } else {
        t232 = false
    }
    if t232 {
        var t233 string = _goml_runtime_core_char_to_string(value__29)
        return t233
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t238 bool = value__4 <= 1114111
    if t238 {
        var t242 bool = value__4 >= 55296
        var jp240 bool
        if t242 {
            var t243 bool = value__4 <= 57343
            jp240 = t243
        } else {
            jp240 = false
        }
        var t241 bool = !jp240
        return t241
    } else {
        return false
    }
}

func main() {
    main0()
}
