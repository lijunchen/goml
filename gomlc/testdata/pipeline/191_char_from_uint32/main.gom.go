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
    var mtmp187 Option__char
    var inline257 Option__char = __goml_builtin_char_from_uint32(value__0)
    mtmp187 = inline257
    switch mtmp187.(type) {
    case None:
        var inline250 string = "none"
        var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline250)
        _goml_runtime_core_string_println(inline251)
        return struct{}{}
    case Some:
        var x188 rune = mtmp187.(Some)._0
        var t202 uint32 = uint32(rune(x188))
        var inline254 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t202)
        _goml_runtime_core_string_println(inline254)
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
    var inline295 uint32 = 57343
    var inline296 Option__char = char_from_uint32(inline295)
    switch inline296.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline298 rune = inline296.(Some)._0
        var inline300 uint32 = uint32(rune(inline298))
        println__T_uint32(inline300)
    default:
        panic("non-exhaustive match")
    }
    var inline287 uint32 = 57344
    var inline288 Option__char = char_from_uint32(inline287)
    switch inline288.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline290 rune = inline288.(Some)._0
        var inline292 uint32 = uint32(rune(inline290))
        println__T_uint32(inline292)
    default:
        panic("non-exhaustive match")
    }
    var inline279 uint32 = 1114111
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
    var inline271 uint32 = 1114112
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
    var mtmp197 Option__char
    var inline268 uint32 = 128512
    var inline269 Option__char = __goml_builtin_char_from_uint32(inline268)
    mtmp197 = inline269
    switch mtmp197.(type) {
    case None:
        var inline259 string = "none"
        var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline259)
        _goml_runtime_core_string_println(inline260)
        return struct{}{}
    case Some:
        var x198 rune = mtmp197.(Some)._0
        var t207 string
        var inline266 string = char_to_string(x198)
        t207 = inline266
        var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
        _goml_runtime_core_string_println(inline263)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func char_from_uint32(value__2 uint32) Option__char {
    var inline303 bool = utf8_valid_scalar(value__2)
    if inline303 {
        var inline304 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline305 rune = inline304._1
        var inline307 Option__char = Some{
            _0: inline305,
        }
        return inline307
    } else {
        return None{}
    }
}

func println__T_string(value__1 string) struct{} {
    var t213 string
    t213 = value__1
    _goml_runtime_core_string_println(t213)
    return struct{}{}
}

func println__T_uint32(value__1 uint32) struct{} {
    var t216 string
    var inline310 string = _goml_runtime_core_uint32_to_string(value__1)
    t216 = inline310
    _goml_runtime_core_string_println(t216)
    return struct{}{}
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t225 bool
    var inline317 bool = value__30 <= 1114111
    if inline317 {
        var inline318 bool = value__30 >= 55296
        var inline320 bool
        if inline318 {
            var inline322 bool = value__30 <= 57343
            inline320 = inline322
        } else {
            inline320 = false
        }
        var inline321 bool = !inline320
        t225 = inline321
    } else {
        t225 = false
    }
    if t225 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t226 Option__char = Some{
            _0: x24,
        }
        return t226
    } else {
        return None{}
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__74 uint32) string {
    var t231 string = _goml_runtime_core_uint32_to_string(self__74)
    return t231
}

func char_to_string(value__29 rune) string {
    var t236 uint32 = uint32(rune(value__29))
    var t237 bool
    var inline324 bool = t236 <= 1114111
    if inline324 {
        var inline325 bool = t236 >= 55296
        var inline327 bool
        if inline325 {
            var inline329 bool = t236 <= 57343
            inline327 = inline329
        } else {
            inline327 = false
        }
        var inline328 bool = !inline327
        t237 = inline328
    } else {
        t237 = false
    }
    if t237 {
        var t238 string = _goml_runtime_core_char_to_string(value__29)
        return t238
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t243 bool = value__4 <= 1114111
    if t243 {
        var t247 bool = value__4 >= 55296
        var jp245 bool
        if t247 {
            var t248 bool = value__4 <= 57343
            jp245 = t248
        } else {
            jp245 = false
        }
        var t246 bool = !jp245
        return t246
    } else {
        return false
    }
}

func main() {
    main0()
}
