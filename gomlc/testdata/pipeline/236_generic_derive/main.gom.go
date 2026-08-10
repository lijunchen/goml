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
    var t190 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t190,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t191 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t191)
    var t192 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t192)
    var t193 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t194 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t195 bool = t193 == t194
    var inline327 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    _goml_runtime_core_string_println(inline327)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t196 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline324 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline324)
    var t197 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline321)
    var t198 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t199 bool = !t198
    var inline318 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t199)
    _goml_runtime_core_string_println(inline318)
    var t200 uint64
    var inline309_source int = 0
    var inline309 uint64 = uint64(int(inline309_source))
    var inline310 uint64 = inline309 + 14695981039346656037
    var inline311 uint64 = inline310 + 2
    var inline312_source int = 0
    var inline312 uint64 = uint64(int(inline312_source))
    var inline313 uint64 = inline312 + 1099511628211
    var inline314 uint64 = inline311 * inline313
    var inline315 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline316 uint64 = inline314 + inline315
    t200 = inline316
    var t201 uint64
    var inline295_source int = 0
    var inline295 uint64 = uint64(int(inline295_source))
    var inline296 uint64 = inline295 + 14695981039346656037
    var inline297 uint64 = inline296 + 2
    var inline298_source int = 0
    var inline298 uint64 = uint64(int(inline298_source))
    var inline299 uint64 = inline298 + 1099511628211
    var inline300 uint64 = inline297 * inline299
    var inline301 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline302 uint64 = inline300 + inline301
    t201 = inline302
    var t202 bool = t200 == t201
    var inline287 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t202)
    _goml_runtime_core_string_println(inline287)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t205 string
    t205 = value__1
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t209 string = "Generic { " + "first: "
    var t210 string
    t210 = "wrapped"
    var t211 string = t209 + t210
    var t212 string = t211 + ", "
    var t213 string = t212 + "second: "
    var t214 string
    t214 = "wrapped"
    var t215 string = t213 + t214
    var t216 string = t215 + " }"
    return t216
}

func println__T_bool(value__1 bool) struct{} {
    var t218 string
    var inline333 string = _goml_runtime_core_bool_to_string(value__1)
    t218 = inline333
    _goml_runtime_core_string_println(t218)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp225 bool
    jp225 = true
    if jp225 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t234_source int = 0
    var t234 uint64 = uint64(int(t234_source))
    var h__10 uint64 = t234 + 14695981039346656037
    var t235_source int = 0
    var t235 uint64 = uint64(int(t235_source))
    var t236 uint64 = t235 + 1099511628211
    var t237 uint64 = h__10 * t236
    var t239 uint64
    t239 = 7
    var h__11 uint64 = t237 + t239
    var t240_source int = 0
    var t240 uint64 = uint64(int(t240_source))
    var t241 uint64 = t240 + 1099511628211
    var t242 uint64 = h__11 * t241
    var t244 uint64
    t244 = 7
    var h__12 uint64 = t242 + t244
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t249 string
        t249 = "wrapped"
        var t250 string = "GenericChoice::Value(" + t249
        var t251 string = t250 + ")"
        return t251
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
    var t281 string = _goml_runtime_core_bool_to_string(self__64)
    return t281
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
