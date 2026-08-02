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
    var t178 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t176, t177)
    println__T_bool(t178)
    var empty__27 GenericChoice__NoTraits__NoTraits = Empty{}
    var value__28 GenericChoice__NoTraits__NoTraits = Value{
        _0: wrapped__24,
    }
    var t179 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    println__T_string(t179)
    var t180 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    println__T_string(t180)
    var t181 bool = _goml_m_trait__impl_i_Eq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t182 bool = !t181
    println__T_bool(t182)
    var t183 uint64 = _goml_m_trait__impl_i_Hash_i_GenericChoice____NoTraits____NoTraits_i_hash(value__28)
    var t184 uint64 = _goml_m_trait__impl_i_Hash_i_GenericChoice____NoTraits____NoTraits_i_hash(value__28)
    var t185 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t183, t184)
    println__T_bool(t185)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var retv191 string
    var mtmp155 Generic__NoTraits__NoTraits = self__4
    var x156 Wrapper__NoTraits = mtmp155.first
    var x157 Wrapper__NoTraits = mtmp155.second
    var second__6 Wrapper__NoTraits = x157
    var first__5 Wrapper__NoTraits = x156
    var t192 string = "Generic { " + "first: "
    var t193 string = _goml_m_trait__impl_i_ToString_i_Wrapper____NoTraits_i_to__string(first__5)
    var t194 string = t192 + t193
    var t195 string = t194 + ", "
    var t196 string = t195 + "second: "
    var t197 string = _goml_m_trait__impl_i_ToString_i_Wrapper____NoTraits_i_to__string(second__6)
    var t198 string = t196 + t197
    var t199 string = t198 + " }"
    retv191 = t199
    return retv191
}

func println__T_bool(value__1 bool) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var retv204 bool
    var jp208 bool
    if true {
        var t212 Wrapper__NoTraits = self__7.first
        var t213 Wrapper__NoTraits = other__8.first
        var t214 bool = _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(t212, t213)
        jp208 = t214
    } else {
        jp208 = false
    }
    var jp206 bool
    if jp208 {
        var t209 Wrapper__NoTraits = self__7.second
        var t210 Wrapper__NoTraits = other__8.second
        var t211 bool = _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(t209, t210)
        jp206 = t211
    } else {
        jp206 = false
    }
    retv204 = jp206
    return retv204
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var retv216 uint64
    var h__10 uint64 = 14695981039346656037
    var t217 uint64 = h__10 * 1099511628211
    var t218 Wrapper__NoTraits = self__9.first
    var t219 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(t218)
    var h__11 uint64 = t217 + t219
    var t220 uint64 = h__11 * 1099511628211
    var t221 Wrapper__NoTraits = self__9.second
    var t222 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(t221)
    var h__12 uint64 = t220 + t222
    retv216 = h__12
    return retv216
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__75 uint64, other__76 uint64) bool {
    var retv224 bool
    var t225 bool = self__75 == other__76
    retv224 = t225
    return retv224
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    var retv227 string
    var jp229 string
    switch self__13.(type) {
    case Empty:
        jp229 = "GenericChoice::Empty"
    case Value:
        var x158 Wrapper__NoTraits = self__13.(Value)._0
        var __field0__14 Wrapper__NoTraits = x158
        var t230 string = _goml_m_trait__impl_i_ToString_i_Wrapper____NoTraits_i_to__string(__field0__14)
        var t231 string = "GenericChoice::Value(" + t230
        var t232 string = t231 + ")"
        jp229 = t232
    default:
        panic("non-exhaustive match")
    }
    retv227 = jp229
    return retv227
}

func _goml_m_trait__impl_i_Eq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
    var retv234 bool
    var mtmp159 Tuple2_33GenericChoice__NoTraits__NoTraits_33GenericChoice__NoTraits__NoTraits = Tuple2_33GenericChoice__NoTraits__NoTraits_33GenericChoice__NoTraits__NoTraits{
        _0: self__15,
        _1: other__16,
    }
    var x160 GenericChoice__NoTraits__NoTraits = mtmp159._0
    var x161 GenericChoice__NoTraits__NoTraits = mtmp159._1
    var jp236 bool
    switch x161.(type) {
    case Empty:
        var jp238 bool
        switch x160.(type) {
        case Empty:
            jp238 = true
        default:
            jp238 = false
        }
        jp236 = jp238
    case Value:
        var x162 Wrapper__NoTraits = x161.(Value)._0
        var jp240 bool
        switch x160.(type) {
        case Value:
            var x164 Wrapper__NoTraits = x160.(Value)._0
            var __l1_0__17 Wrapper__NoTraits = x164
            var __r1_0__18 Wrapper__NoTraits = x162
            var jp242 bool
            if true {
                var t243 bool = _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(__l1_0__17, __r1_0__18)
                jp242 = t243
            } else {
                jp242 = false
            }
            jp240 = jp242
        default:
            jp240 = false
        }
        jp236 = jp240
    default:
        panic("non-exhaustive match")
    }
    retv234 = jp236
    return retv234
}

func _goml_m_trait__impl_i_Hash_i_GenericChoice____NoTraits____NoTraits_i_hash(self__19 GenericChoice__NoTraits__NoTraits) uint64 {
    var retv245 uint64
    var jp247 uint64
    switch self__19.(type) {
    case Empty:
        var h__20 uint64 = 14695981039346656037 + 1
        jp247 = h__20
    case Value:
        var x165 Wrapper__NoTraits = self__19.(Value)._0
        var __field1_0__21 Wrapper__NoTraits = x165
        var h__22 uint64 = 14695981039346656037 + 2
        var t248 uint64 = h__22 * 1099511628211
        var t249 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(__field1_0__21)
        var h__23 uint64 = t248 + t249
        jp247 = h__23
    default:
        panic("non-exhaustive match")
    }
    retv245 = jp247
    return retv245
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv251 string
    retv251 = self__38
    return retv251
}

func _goml_m_trait__impl_i_ToString_i_Wrapper____NoTraits_i_to__string(self__0 Wrapper__NoTraits) string {
    var retv253 string
    retv253 = "wrapped"
    return retv253
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv255 string
    var t256 string = _goml_runtime_core_bool_to_string(self__37)
    retv255 = t256
    return retv255
}

func _goml_m_trait__impl_i_Eq_i_Wrapper____NoTraits_i_eq(self__1 Wrapper__NoTraits, other__2 Wrapper__NoTraits) bool {
    var retv258 bool
    retv258 = true
    return retv258
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    var retv260 uint64
    retv260 = 7
    return retv260
}

func main() {
    main0()
}
