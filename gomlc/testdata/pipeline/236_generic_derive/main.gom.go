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
    var t195 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t195,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t196 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t196)
    var t197 bool = _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t197)
    var t198 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t199 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t200 bool
    var inline320 bool = t198 == t199
    t200 = inline320
    println__T_bool(t200)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t201 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    println__T_string(t201)
    var t202 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline317)
    var t203 bool
    switch empty__27.(type) {
    case Value:
        var inline312 Wrapper__NoTraits = empty__27.(Value)._0
        var inline315 bool = _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(inline312, wrapped__24)
        t203 = inline315
    default:
        t203 = false
    }
    var t204 bool = !t203
    var inline305 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t204)
    _goml_runtime_core_string_println(inline305)
    var t205 uint64
    var inline300 uint64 = 14695981039346656037 + 2
    var inline301 uint64 = inline300 * 1099511628211
    var inline302 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline303 uint64 = inline301 + inline302
    t205 = inline303
    var t206 uint64
    var inline292 uint64 = 14695981039346656037 + 2
    var inline293 uint64 = inline292 * 1099511628211
    var inline294 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline295 uint64 = inline293 + inline294
    t206 = inline295
    var t207 bool
    var inline287 bool = t205 == t206
    t207 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t207)
    _goml_runtime_core_string_println(inline284)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t210 string
    t210 = value__31
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t214 string = "Generic { " + "first: "
    var t215 string
    t215 = "wrapped"
    var t216 string = t214 + t215
    var t217 string = t216 + ", "
    var t218 string = t217 + "second: "
    var t219 string
    t219 = "wrapped"
    var t220 string = t218 + t219
    var t221 string = t220 + " }"
    return t221
}

func println__T_bool(value__31 bool) struct{} {
    var t223 string
    var inline325 string = _goml_runtime_core_bool_to_string(value__31)
    t223 = inline325
    _goml_runtime_core_string_println(t223)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp230 bool
    jp230 = true
    if jp230 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var h__10 uint64 = 14695981039346656037
    var t239 uint64 = h__10 * 1099511628211
    var t241 uint64
    t241 = 7
    var h__11 uint64 = t239 + t241
    var t242 uint64 = h__11 * 1099511628211
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
        var t252 string
        t252 = "wrapped"
        var t253 string = "GenericChoice::Value(" + t252
        var t254 string = t253 + ")"
        return t254
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t278 string = _goml_runtime_core_bool_to_string(self__66)
    return t278
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
