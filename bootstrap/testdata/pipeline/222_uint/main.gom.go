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

func _goml_runtime_core_uint_to_string(x uint) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint_hash(x uint) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type hashmap_uint_string_x_entry struct {
    active bool
    key uint
    value string
}

type hashmap_uint_string_x struct {
    buckets map[uint64][]hashmap_uint_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_4uint_6string() *hashmap_uint_string_x {
    return &hashmap_uint_string_x{
        buckets: make(map[uint64][]hashmap_uint_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_uint_i_hash(key)
    var bucket []hashmap_uint_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_uint_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_uint_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_4uint_6string(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_uint_i_hash(key)
    var bucket []hashmap_uint_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_uint_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_uint_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_uint_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_uint_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func combine(left__0 uint, right__1 uint) uint {
    var retv73 uint
    var t74 uint = left__0 + right__1
    var t75 uint = t74 * 2
    retv73 = t75
    return retv73
}

func classify(value__2 uint) string {
    var retv77 string
    var jp79 string
    switch value__2 {
    case 0:
        jp79 = "zero"
    case 42:
        jp79 = "answer"
    default:
        jp79 = "other"
    }
    retv77 = jp79
    return retv77
}

func main0() struct{} {
    var left__3 uint = 19
    var right__4_source int = 2
    var right__4 uint = uint(int(right__4_source))
    var result__5 uint = combine(left__3, right__4)
    println__T_uint(result__5)
    var t81 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    println__T_string(t81)
    var t82 string = classify(result__5)
    println__T_string(t82)
    var t83 bool = result__5 > left__3
    println__T_bool(t83)
    var t84_rhs uint = 15
    var t84 uint = result__5 & t84_rhs
    var t85 uint64 = uint64(uint(t84))
    println__T_uint64(t85)
    var values__6 *hashmap_uint_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__uint____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__uint____V__string(values__6, result__5, "stored")
    var mtmp70 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__uint____V__string(values__6, 42)
    switch mtmp70.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var x71 string = mtmp70.(Some)._0
        var value__7 string = x71
        println__T_string(value__7)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_uint(value__1 uint) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__226 uint) string {
    var retv96 string
    var t97 string = _goml_runtime_core_uint_to_string(self__226)
    retv96 = t97
    return retv96
}

func println__T_bool(value__1 bool) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__uint____V__string() *hashmap_uint_string_x {
    var retv105 *hashmap_uint_string_x
    var t106 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    retv105 = t106
    return retv105
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__uint____V__string(self__200 *hashmap_uint_string_x, key__201 uint, value__202 string) struct{} {
    hashmap_set__HashMap_4uint_6string(self__200, key__201, value__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__uint____V__string(self__198 *hashmap_uint_string_x, key__199 uint) Option__string {
    var retv110 Option__string
    var t111 Option__string = hashmap_get__HashMap_4uint_6string(self__198, key__199)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv113 string
    retv113 = self__38
    return retv113
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv115 string
    var t116 string = _goml_runtime_core_bool_to_string(self__37)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv118 string
    var t119 string = _goml_runtime_core_uint64_to_string(self__48)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_Eq_i_uint_i_eq(self__227 uint, other__228 uint) bool {
    var retv121 bool
    var t122 bool = self__227 == other__228
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__229 uint) uint64 {
    var retv124 uint64
    var t125 uint64 = _goml_runtime_core_uint_hash(self__229)
    retv124 = t125
    return retv124
}

func main() {
    main0()
}
