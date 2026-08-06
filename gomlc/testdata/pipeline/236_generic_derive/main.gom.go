package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_33GenericChoice__NoTraits__NoTraits_33GenericChoice__NoTraits__NoTraits struct {
    _0 GenericChoice__NoTraits__NoTraits
    _1 GenericChoice__NoTraits__NoTraits
}

type NoTraits struct {
    value int
}

type Wrapper__NoTraits struct {
    value NoTraits
}

type Generic__NoTraits__NoTraits struct {
    first Wrapper__NoTraits
    second Wrapper__NoTraits
}

type GenericChoice__NoTraits__NoTraits interface {
    isGenericChoice__NoTraits__NoTraits()
}

type Empty struct {}

func (_ Empty) isGenericChoice__NoTraits__NoTraits() {}

type Value struct {
    _0 Wrapper__NoTraits
}

func (_ Value) isGenericChoice__NoTraits__NoTraits() {}

func main0() struct{} {
    var t154 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t154,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t155 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t155)
    var t156 bool = _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t156)
    var t157 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t158 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t159 bool
    var inline296 bool = t157 == t158
    t159 = inline296
    println__T_bool(t159)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t160 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline293 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline293)
    var t161 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline290)
    var t162 bool = _goml_m_trait__impl_i_Eq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t163 bool = !t162
    var inline287 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t163)
    _goml_runtime_core_string_println(inline287)
    var t164 uint64
    var inline278_source int = 0
    var inline278 uint64 = uint64(int(inline278_source))
    var inline279 uint64 = inline278 + 14695981039346656037
    var inline280 uint64 = inline279 + 2
    var inline281_source int = 0
    var inline281 uint64 = uint64(int(inline281_source))
    var inline282 uint64 = inline281 + 1099511628211
    var inline283 uint64 = inline280 * inline282
    var inline284 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline285 uint64 = inline283 + inline284
    t164 = inline285
    var t165 uint64
    var inline264_source int = 0
    var inline264 uint64 = uint64(int(inline264_source))
    var inline265 uint64 = inline264 + 14695981039346656037
    var inline266 uint64 = inline265 + 2
    var inline267_source int = 0
    var inline267 uint64 = uint64(int(inline267_source))
    var inline268 uint64 = inline267 + 1099511628211
    var inline269 uint64 = inline266 * inline268
    var inline270 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline271 uint64 = inline269 + inline270
    t165 = inline271
    var t166 bool
    var inline257 bool = t164 == t165
    t166 = inline257
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t166)
    _goml_runtime_core_string_println(inline254)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t169 string
    t169 = value__31
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t173 string = "Generic { " + "first: "
    var t174 string
    t174 = "wrapped"
    var t175 string = t173 + t174
    var t176 string = t175 + ", "
    var t177 string = t176 + "second: "
    var t178 string
    t178 = "wrapped"
    var t179 string = t177 + t178
    var t180 string = t179 + " }"
    return t180
}

func println__T_bool(value__31 bool) struct{} {
    var t182 string
    var inline301 string = _goml_runtime_core_bool_to_string(value__31)
    t182 = inline301
    _goml_runtime_core_string_println(t182)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp189 bool
    jp189 = true
    if jp189 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t198_source int = 0
    var t198 uint64 = uint64(int(t198_source))
    var h__10 uint64 = t198 + 14695981039346656037
    var t199_source int = 0
    var t199 uint64 = uint64(int(t199_source))
    var t200 uint64 = t199 + 1099511628211
    var t201 uint64 = h__10 * t200
    var t203 uint64
    t203 = 7
    var h__11 uint64 = t201 + t203
    var t204_source int = 0
    var t204 uint64 = uint64(int(t204_source))
    var t205 uint64 = t204 + 1099511628211
    var t206 uint64 = h__11 * t205
    var t208 uint64
    t208 = 7
    var h__12 uint64 = t206 + t208
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t216 string
        t216 = "wrapped"
        var t217 string = "GenericChoice::Value(" + t216
        var t218 string = t217 + ")"
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
    switch other__16.(type) {
    case Empty:
        switch self__15.(type) {
        case Empty:
            return true
        default:
            return false
        }
    case Value:
        switch self__15.(type) {
        case Value:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t248 string = _goml_runtime_core_bool_to_string(self__66)
    return t248
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
