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
    var t200 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t200,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t201 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t201)
    var t202 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t202)
    var t203 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t204 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t205 bool = t203 == t204
    var inline337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t205)
    _goml_runtime_core_string_println(inline337)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t206 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline334 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline334)
    var t207 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline331 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline331)
    var t208 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t209 bool = !t208
    var inline328 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t209)
    _goml_runtime_core_string_println(inline328)
    var t210 uint64
    var inline319_source int = 0
    var inline319 uint64 = uint64(int(inline319_source))
    var inline320 uint64 = inline319 + 14695981039346656037
    var inline321 uint64 = inline320 + 2
    var inline322_source int = 0
    var inline322 uint64 = uint64(int(inline322_source))
    var inline323 uint64 = inline322 + 1099511628211
    var inline324 uint64 = inline321 * inline323
    var inline325 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline326 uint64 = inline324 + inline325
    t210 = inline326
    var t211 uint64
    var inline305_source int = 0
    var inline305 uint64 = uint64(int(inline305_source))
    var inline306 uint64 = inline305 + 14695981039346656037
    var inline307 uint64 = inline306 + 2
    var inline308_source int = 0
    var inline308 uint64 = uint64(int(inline308_source))
    var inline309 uint64 = inline308 + 1099511628211
    var inline310 uint64 = inline307 * inline309
    var inline311 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline312 uint64 = inline310 + inline311
    t211 = inline312
    var t212 bool = t210 == t211
    var inline297 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t212)
    _goml_runtime_core_string_println(inline297)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t215 string
    t215 = value__1
    _goml_runtime_core_string_println(t215)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t219 string = "Generic { " + "first: "
    var t220 string
    t220 = "wrapped"
    var t221 string = t219 + t220
    var t222 string = t221 + ", "
    var t223 string = t222 + "second: "
    var t224 string
    t224 = "wrapped"
    var t225 string = t223 + t224
    var t226 string = t225 + " }"
    return t226
}

func println__T_bool(value__1 bool) struct{} {
    var t228 string
    var inline343 string = _goml_runtime_core_bool_to_string(value__1)
    t228 = inline343
    _goml_runtime_core_string_println(t228)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp235 bool
    jp235 = true
    if jp235 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t244_source int = 0
    var t244 uint64 = uint64(int(t244_source))
    var h__10 uint64 = t244 + 14695981039346656037
    var t245_source int = 0
    var t245 uint64 = uint64(int(t245_source))
    var t246 uint64 = t245 + 1099511628211
    var t247 uint64 = h__10 * t246
    var t249 uint64
    t249 = 7
    var h__11 uint64 = t247 + t249
    var t250_source int = 0
    var t250 uint64 = uint64(int(t250_source))
    var t251 uint64 = t250 + 1099511628211
    var t252 uint64 = h__11 * t251
    var t254 uint64
    t254 = 7
    var h__12 uint64 = t252 + t254
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t259 string
        t259 = "wrapped"
        var t260 string = "GenericChoice::Value(" + t259
        var t261 string = t260 + ")"
        return t261
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
    var t291 string = _goml_runtime_core_bool_to_string(self__64)
    return t291
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
