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
    var t173 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t173,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t174 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t174)
    var t175 bool = _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t175)
    var t176 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t177 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t178 bool
    var inline298 bool = t176 == t177
    t178 = inline298
    println__T_bool(t178)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t179 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    println__T_string(t179)
    var t180 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline295)
    var t181 bool
    switch empty__27.(type) {
    case Value:
        var inline290 Wrapper__NoTraits = empty__27.(Value)._0
        var inline293 bool = _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(inline290, wrapped__24)
        t181 = inline293
    default:
        t181 = false
    }
    var t182 bool = !t181
    var inline283 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t182)
    _goml_runtime_core_string_println(inline283)
    var t183 uint64
    var inline278 uint64 = 14695981039346656037 + 2
    var inline279 uint64 = inline278 * 1099511628211
    var inline280 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline281 uint64 = inline279 + inline280
    t183 = inline281
    var t184 uint64
    var inline270 uint64 = 14695981039346656037 + 2
    var inline271 uint64 = inline270 * 1099511628211
    var inline272 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline273 uint64 = inline271 + inline272
    t184 = inline273
    var t185 bool
    var inline265 bool = t183 == t184
    t185 = inline265
    var inline262 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    _goml_runtime_core_string_println(inline262)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t188 string
    t188 = value__1
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t192 string = "Generic { " + "first: "
    var t193 string
    t193 = "wrapped"
    var t194 string = t192 + t193
    var t195 string = t194 + ", "
    var t196 string = t195 + "second: "
    var t197 string
    t197 = "wrapped"
    var t198 string = t196 + t197
    var t199 string = t198 + " }"
    return t199
}

func println__T_bool(value__1 bool) struct{} {
    var t201 string
    var inline303 string = _goml_runtime_core_bool_to_string(value__1)
    t201 = inline303
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp208 bool
    jp208 = true
    if jp208 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var h__10 uint64 = 14695981039346656037
    var t217 uint64 = h__10 * 1099511628211
    var t219 uint64
    t219 = 7
    var h__11 uint64 = t217 + t219
    var t220 uint64 = h__11 * 1099511628211
    var t222 uint64
    t222 = 7
    var h__12 uint64 = t220 + t222
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13.(type) {
    case Empty:
        return "GenericChoice::Empty"
    case Value:
        var t230 string
        t230 = "wrapped"
        var t231 string = "GenericChoice::Value(" + t230
        var t232 string = t231 + ")"
        return t232
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t256 string = _goml_runtime_core_bool_to_string(self__37)
    return t256
}

func _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(self__1 Wrapper__NoTraits, other__2 Wrapper__NoTraits) bool {
    return true
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
