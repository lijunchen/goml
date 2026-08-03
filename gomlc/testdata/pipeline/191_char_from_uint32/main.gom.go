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
    var commute_field273 rune
    var inline203 bool = utf8_valid_scalar(value__0)
    if inline203 {
        var inline204 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline206 rune = inline204._1
        commute_field273 = inline206
        var t151 uint32 = uint32(rune(commute_field273))
        var inline200 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(t151)
        _goml_runtime_core_string_println(inline200)
        return struct{}{}
    } else {
        var inline196 string = "none"
        var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline196)
        _goml_runtime_core_string_println(inline197)
        return struct{}{}
    }
}

func main0() struct{} {
    show_scalar(0)
    show_scalar(65)
    show_scalar(55295)
    show_scalar(55296)
    show_scalar(57343)
    var inline243 uint32 = 57344
    var inline244 Option__char = char_from_uint32(inline243)
    switch inline244.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline246 rune = inline244.(Some)._0
        var inline248 uint32 = uint32(rune(inline246))
        println__T_uint32(inline248)
    default:
        panic("non-exhaustive match")
    }
    var inline235 uint32 = 1114111
    var inline236 Option__char = char_from_uint32(inline235)
    switch inline236.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline238 rune = inline236.(Some)._0
        var inline240 uint32 = uint32(rune(inline238))
        println__T_uint32(inline240)
    default:
        panic("non-exhaustive match")
    }
    var inline227 uint32 = 1114112
    var inline228 Option__char = char_from_uint32(inline227)
    switch inline228.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline230 rune = inline228.(Some)._0
        var inline232 uint32 = uint32(rune(inline230))
        println__T_uint32(inline232)
    default:
        panic("non-exhaustive match")
    }
    var commute_field276 rune
    var inline219 uint32 = 128512
    var inline220 bool = utf8_valid_scalar(inline219)
    if inline220 {
        var inline221 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(inline219)
        var inline223 rune = inline221._1
        commute_field276 = inline223
        var t156 string
        var inline217 string = char_to_string(commute_field276)
        t156 = inline217
        var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t156)
        _goml_runtime_core_string_println(inline214)
        return struct{}{}
    } else {
        var inline210 string = "none"
        var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline210)
        _goml_runtime_core_string_println(inline211)
        return struct{}{}
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t162 bool
    var inline251 bool = value__32 <= 1114111
    if inline251 {
        var inline252 bool = value__32 >= 55296
        var inline254 bool
        if inline252 {
            var inline256 bool = value__32 <= 57343
            inline254 = inline256
        } else {
            inline254 = false
        }
        var inline255 bool = !inline254
        t162 = inline255
    } else {
        t162 = false
    }
    if t162 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t163 Option__char = Some{
            _0: x24,
        }
        return t163
    } else {
        return None{}
    }
}

func println__T_string(value__31 string) struct{} {
    var t165 string
    t165 = value__31
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func println__T_uint32(value__31 uint32) struct{} {
    var t168 string
    var inline259 string = _goml_runtime_core_uint32_to_string(value__31)
    t168 = inline259
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t177 bool = value__4 <= 1114111
    if t177 {
        var t181 bool = value__4 >= 55296
        var jp179 bool
        if t181 {
            var t182 bool = value__4 <= 57343
            jp179 = t182
        } else {
            jp179 = false
        }
        var t180 bool = !jp179
        return t180
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__76 uint32) string {
    var t187 string = _goml_runtime_core_uint32_to_string(self__76)
    return t187
}

func char_to_string(value__29 rune) string {
    var t192 uint32 = uint32(rune(value__29))
    var t193 bool
    var inline266 bool = t192 <= 1114111
    if inline266 {
        var inline267 bool = t192 >= 55296
        var inline269 bool
        if inline267 {
            var inline271 bool = t192 <= 57343
            inline269 = inline271
        } else {
            inline269 = false
        }
        var inline270 bool = !inline269
        t193 = inline270
    } else {
        t193 = false
    }
    if t193 {
        var t194 string = _goml_runtime_core_char_to_string(value__29)
        return t194
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func main() {
    main0()
}
