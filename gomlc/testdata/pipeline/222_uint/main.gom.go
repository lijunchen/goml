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
    var retv117 uint
    var t118 uint = left__0 + right__1
    var t119 uint = t118 * 2
    retv117 = t119
    return retv117
}

func classify(value__2 uint) string {
    var retv121 string
    var jp123 string
    switch value__2 {
    case 0:
        jp123 = "zero"
    case 42:
        jp123 = "answer"
    default:
        jp123 = "other"
    }
    retv121 = jp123
    return retv121
}

func main0() struct{} {
    var left__3 uint = 19
    var right__4_source int = 2
    var right__4 uint = uint(int(right__4_source))
    var result__5 uint = combine(left__3, right__4)
    println__T_uint(result__5)
    var t125 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    println__T_string(t125)
    var t126 string = classify(result__5)
    println__T_string(t126)
    var t127 bool = result__5 > left__3
    println__T_bool(t127)
    var t128_rhs uint = 15
    var t128 uint = result__5 & t128_rhs
    var t129 uint64 = uint64(uint(t128))
    println__T_uint64(t129)
    var values__6 *hashmap_uint_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__uint____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__uint____V__string(values__6, result__5, "stored")
    var mtmp114 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__uint____V__string(values__6, 42)
    switch mtmp114.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var x115 string = mtmp114.(Some)._0
        var value__7 string = x115
        println__T_string(value__7)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func println__T_uint(value__1 uint) struct{} {
    var t134 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(value__1)
    _goml_runtime_core_string_println(t134)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t137)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__224 uint) string {
    var retv140 string
    var t141 string = _goml_runtime_core_uint_to_string(self__224)
    retv140 = t141
    return retv140
}

func println__T_bool(value__1 bool) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func println__T_uint64(value__1 uint64) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__uint____V__string() *hashmap_uint_string_x {
    var retv149 *hashmap_uint_string_x
    var t150 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    retv149 = t150
    return retv149
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__uint____V__string(self__198 *hashmap_uint_string_x, key__199 uint, value__200 string) struct{} {
    hashmap_set__HashMap_4uint_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__uint____V__string(self__196 *hashmap_uint_string_x, key__197 uint) Option__string {
    var retv154 Option__string
    var t155 Option__string = hashmap_get__HashMap_4uint_6string(self__196, key__197)
    retv154 = t155
    return retv154
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv157 string
    retv157 = self__38
    return retv157
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv159 string
    var t160 string = _goml_runtime_core_bool_to_string(self__37)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var retv162 string
    var t163 string = _goml_runtime_core_uint64_to_string(self__48)
    retv162 = t163
    return retv162
}

func _goml_m_trait__impl_i_Eq_i_uint_i_eq(self__225 uint, other__226 uint) bool {
    var retv165 bool
    var t166 bool = self__225 == other__226
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__227 uint) uint64 {
    var retv168 uint64
    var t169 uint64 = _goml_runtime_core_uint_hash(self__227)
    retv168 = t169
    return retv168
}

func main() {
    main0()
}
