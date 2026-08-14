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
    var t205 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t205,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t206 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t206)
    var t207 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t207)
    var t208 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t209 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t210 bool = t208 == t209
    var inline342 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t210)
    _goml_runtime_core_string_println(inline342)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t211 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline339 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline339)
    var t212 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline336 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline336)
    var t213 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t214 bool = !t213
    var inline333 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t214)
    _goml_runtime_core_string_println(inline333)
    var t215 uint64
    var inline324_source int = 0
    var inline324 uint64 = uint64(int(inline324_source))
    var inline325 uint64 = inline324 + 14695981039346656037
    var inline326 uint64 = inline325 + 2
    var inline327_source int = 0
    var inline327 uint64 = uint64(int(inline327_source))
    var inline328 uint64 = inline327 + 1099511628211
    var inline329 uint64 = inline326 * inline328
    var inline330 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline331 uint64 = inline329 + inline330
    t215 = inline331
    var t216 uint64
    var inline310_source int = 0
    var inline310 uint64 = uint64(int(inline310_source))
    var inline311 uint64 = inline310 + 14695981039346656037
    var inline312 uint64 = inline311 + 2
    var inline313_source int = 0
    var inline313 uint64 = uint64(int(inline313_source))
    var inline314 uint64 = inline313 + 1099511628211
    var inline315 uint64 = inline312 * inline314
    var inline316 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline317 uint64 = inline315 + inline316
    t216 = inline317
    var t217 bool = t215 == t216
    var inline302 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t217)
    _goml_runtime_core_string_println(inline302)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t220 string
    t220 = value__1
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t224 string = "Generic { " + "first: "
    var t225 string
    t225 = "wrapped"
    var t226 string = t224 + t225
    var t227 string = t226 + ", "
    var t228 string = t227 + "second: "
    var t229 string
    t229 = "wrapped"
    var t230 string = t228 + t229
    var t231 string = t230 + " }"
    return t231
}

func println__T_bool(value__1 bool) struct{} {
    var t233 string
    var inline348 string = _goml_runtime_core_bool_to_string(value__1)
    t233 = inline348
    _goml_runtime_core_string_println(t233)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp240 bool
    jp240 = true
    if jp240 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t249_source int = 0
    var t249 uint64 = uint64(int(t249_source))
    var h__10 uint64 = t249 + 14695981039346656037
    var t250_source int = 0
    var t250 uint64 = uint64(int(t250_source))
    var t251 uint64 = t250 + 1099511628211
    var t252 uint64 = h__10 * t251
    var t254 uint64
    t254 = 7
    var h__11 uint64 = t252 + t254
    var t255_source int = 0
    var t255 uint64 = uint64(int(t255_source))
    var t256 uint64 = t255 + 1099511628211
    var t257 uint64 = h__11 * t256
    var t259 uint64
    t259 = 7
    var h__12 uint64 = t257 + t259
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t264 string
        t264 = "wrapped"
        var t265 string = "GenericChoice::Value(" + t264
        var t266 string = t265 + ")"
        return t266
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
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

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t296 string = _goml_runtime_core_bool_to_string(self__64)
    return t296
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
