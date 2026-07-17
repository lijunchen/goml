package main

import (
    _goml_fmt "fmt"
    _goml_math "math"
    _goml_reflect "reflect"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_float32_hash(x float32) uint64 {
    if x == 0 {
        return 0
    }
    return uint64(_goml_math.Float32bits(x))
}

func _goml_runtime_core_float64_hash(x float64) uint64 {
    if x == 0 {
        return 0
    }
    return _goml_math.Float64bits(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_CollisionKey_x struct {
    value CollisionKey
}

func ref__Ref_12CollisionKey(value CollisionKey) *ref_CollisionKey_x {
    return &ref_CollisionKey_x{
        value: value,
    }
}

func ref_set__Ref_12CollisionKey(reference *ref_CollisionKey_x, value CollisionKey) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_12CollisionKey(a *ref_CollisionKey_x, b *ref_CollisionKey_x) bool {
    return a == b
}

func ptr_hash__Ref_12CollisionKey(reference *ref_CollisionKey_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type hashmap_float32_string_x_entry struct {
    active bool
    key float32
    value string
}

type hashmap_float32_string_x struct {
    buckets map[uint64][]hashmap_float32_string_x_entry
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_7float32_6string() *hashmap_float32_string_x {
    return &hashmap_float32_string_x{
        buckets: make(map[uint64][]hashmap_float32_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_7float32_6string(m *hashmap_float32_string_x) int32 {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_7float32_6string(m *hashmap_float32_string_x, key float32) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(key)
    var bucket []hashmap_float32_string_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_float32_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_float32_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_7float32_6string(m *hashmap_float32_string_x, key float32) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_7float32_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_7float32_6string(m *hashmap_float32_string_x, key float32, value string) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(key)
    var bucket []hashmap_float32_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_float32_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_float32_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_float32_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_float32_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_7float32_6string(m *hashmap_float32_string_x, key float32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(key)
    var bucket []hashmap_float32_string_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_float32_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_float32_i_eq(entry.key, key) {
            var zero hashmap_float32_string_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

type hashmap_float64_string_x_entry struct {
    active bool
    key float64
    value string
}

type hashmap_float64_string_x struct {
    buckets map[uint64][]hashmap_float64_string_x_entry
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_7float64_6string() *hashmap_float64_string_x {
    return &hashmap_float64_string_x{
        buckets: make(map[uint64][]hashmap_float64_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_7float64_6string(m *hashmap_float64_string_x, key float64) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(key)
    var bucket []hashmap_float64_string_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_float64_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_float64_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_7float64_6string(m *hashmap_float64_string_x, key float64) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_7float64_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_7float64_6string(m *hashmap_float64_string_x, key float64, value string) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(key)
    var bucket []hashmap_float64_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_float64_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_float64_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_float64_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_float64_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_FloatKey_string_x_entry struct {
    active bool
    key FloatKey
    value string
}

type hashmap_FloatKey_string_x struct {
    buckets map[uint64][]hashmap_FloatKey_string_x_entry
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_8FloatKey_6string() *hashmap_FloatKey_string_x {
    return &hashmap_FloatKey_string_x{
        buckets: make(map[uint64][]hashmap_FloatKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_8FloatKey_6string(m *hashmap_FloatKey_string_x, key FloatKey) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(key)
    var bucket []hashmap_FloatKey_string_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_FloatKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_FloatKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_8FloatKey_6string(m *hashmap_FloatKey_string_x, key FloatKey) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_8FloatKey_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_8FloatKey_6string(m *hashmap_FloatKey_string_x, key FloatKey, value string) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(key)
    var bucket []hashmap_FloatKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_FloatKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_FloatKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_FloatKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_FloatKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_CollisionKey_int32_x_entry struct {
    active bool
    key CollisionKey
    value int32
}

type hashmap_CollisionKey_int32_x struct {
    buckets map[uint64][]hashmap_CollisionKey_int32_x_entry
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_12CollisionKey_5int32() *hashmap_CollisionKey_int32_x {
    return &hashmap_CollisionKey_int32_x{
        buckets: make(map[uint64][]hashmap_CollisionKey_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x) int32 {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_12CollisionKey_5int32(m, key)
    if ok {
        return Option__int32_Some{
            _0: value,
        }
    }
    return Option__int32_None{}
}

func hashmap_set__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey, value int32) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_CollisionKey_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_CollisionKey_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(entry.key, key) {
            var zero hashmap_CollisionKey_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

type hashmap_Ref_12CollisionKey_string_x_entry struct {
    active bool
    key *ref_CollisionKey_x
    value string
}

type hashmap_Ref_12CollisionKey_string_x struct {
    buckets map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_18Ref_12CollisionKey_6string() *hashmap_Ref_12CollisionKey_string_x {
    return &hashmap_Ref_12CollisionKey_string_x{
        buckets: make(map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x, value string) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_12CollisionKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_12CollisionKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type FloatKey struct {
    value float64
}

type CollisionKey struct {
    value int32
}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func _goml_m_trait__impl_i_Eq_i_FloatKey_i_eq(self__0 FloatKey, other__1 FloatKey) bool {
    var retv106 bool
    var jp108 bool
    if true {
        var t109 float64 = self__0.value
        var t110 float64 = other__1.value
        var t111 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(t109, t110)
        jp108 = t111
    } else {
        jp108 = false
    }
    retv106 = jp108
    return retv106
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var retv113 uint64
    var h__3 uint64 = 14695981039346656037
    var t114 uint64 = h__3 * 1099511628211
    var t115 float64 = self__2.value
    var t116 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(t115)
    var h__4 uint64 = t114 + t116
    retv113 = h__4
    return retv113
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var retv118 bool
    var t119 int32 = self__5.value
    var t120 int32 = other__6.value
    var t121 bool = t119 == t120
    retv118 = t121
    return retv118
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    var retv123 uint64
    retv123 = 1
    return retv123
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var x58 string = value__8.(Option__string_Some)._0
        var text__9 string = x58
        println__T_string(text__9)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var x59 int32 = value__10.(Option__int32_Some)._0
        var number__11 int32 = x59
        println__T_int32(number__11)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t133 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t134 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t135 bool = t133 == t134
    var t136 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t135)
    println__T_string(t136)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t137 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t137)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t138 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t138)
    var t139 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, zero32__12)
    print_opt_string(t139)
    _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(values32__14, negative_zero32__13)
    var t140 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t140)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t141 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(zero64__15)
    var t142 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(negative_zero64__16)
    var t143 bool = t141 == t142
    var t144 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t143)
    println__T_string(t144)
    var values64__17 *hashmap_float64_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(values64__17, zero64__15, "f64")
    var t145 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(values64__17, negative_zero64__16)
    print_opt_string(t145)
    var derived__18 *hashmap_FloatKey_string_x = _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string()
    var t146 FloatKey = FloatKey{
        value: zero64__15,
    }
    _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(derived__18, t146, "derived")
    var t147 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t148 Option__string = _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(derived__18, t147)
    print_opt_string(t148)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t150 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t150, 10)
    var t151 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t151, 20)
    var t152 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t152, 30)
    var t153 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t153)
    var t154 CollisionKey = CollisionKey{
        value: 1,
    }
    var t155 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t154)
    print_opt_int(t155)
    var t156 CollisionKey = CollisionKey{
        value: 2,
    }
    var t157 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t156)
    print_opt_int(t157)
    var t158 CollisionKey = CollisionKey{
        value: 3,
    }
    var t159 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t158)
    print_opt_int(t159)
    var t160 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t160, 40)
    var t161 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t161)
    var t162 CollisionKey = CollisionKey{
        value: 4,
    }
    var t163 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t162)
    print_opt_int(t163)
    var t164 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t164, 41)
    var t165 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t165)
    var t166 CollisionKey = CollisionKey{
        value: 4,
    }
    var t167 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t166)
    print_opt_int(t167)
    var t168 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t168)
    var t169 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t169)
    var t170 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t170)
    var index__20 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop173:
    for {
        var t174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
        var t175 bool = t174 < 2000
        if t175 {
            var t176 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t177 int32 = 1000 + t176
            var key__21 CollisionKey = CollisionKey{
                value: t177,
            }
            var t178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, key__21, t178)
            _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, key__21)
            var t179 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t180 int32 = t179 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__20, t180)
            continue
        } else {
            break Loop_loop173
        }
    }
    var t172 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t172)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t182 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t182)
    var alias__24 *ref_CollisionKey_x = key__23
    var t183 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t183)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t184 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, alias__24)
    println__T_bool(t184)
    var t185 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, equal_value__25)
    println__T_bool(t185)
    var t186 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key__23)
    var t187 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(alias__24)
    var t188 bool = t186 == t187
    println__T_bool(t188)
    var t189 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, alias__24)
    print_opt_string(t189)
    var t190 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, equal_value__25)
    print_opt_string(t190)
    var t191 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(key__23, t191)
    var t192 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, key__23)
    print_opt_string(t192)
    return struct{}{}
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__72 float64, other__73 float64) bool {
    var retv195 bool
    var t196 bool = self__72 == other__73
    retv195 = t196
    return retv195
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__87 float64) uint64 {
    var retv198 uint64
    var t199 uint64 = _goml_runtime_core_float64_hash(self__87)
    retv198 = t199
    return retv198
}

func println__T_string(value__1 string) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__86 float32) uint64 {
    var retv207 uint64
    var t208 uint64 = _goml_runtime_core_float32_hash(self__86)
    retv207 = t208
    return retv207
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv210 string
    var t211 string = _goml_runtime_core_bool_to_string(self__33)
    retv210 = t211
    return retv210
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var retv213 *hashmap_float32_string_x
    var t214 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__192 *hashmap_float32_string_x, key__193 float32, value__194 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__190 *hashmap_float32_string_x, key__191 float32) Option__string {
    var retv218 Option__string
    var t219 Option__string = hashmap_get__HashMap_7float32_6string(self__190, key__191)
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(self__197 *hashmap_float32_string_x) int32 {
    var retv221 int32
    var t222 int32 = hashmap_len__HashMap_7float32_6string(self__197)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(self__195 *hashmap_float32_string_x, key__196 float32) struct{} {
    hashmap_remove__HashMap_7float32_6string(self__195, key__196)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string() *hashmap_float64_string_x {
    var retv226 *hashmap_float64_string_x
    var t227 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(self__192 *hashmap_float64_string_x, key__193 float64, value__194 string) struct{} {
    hashmap_set__HashMap_7float64_6string(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(self__190 *hashmap_float64_string_x, key__191 float64) Option__string {
    var retv231 Option__string
    var t232 Option__string = hashmap_get__HashMap_7float64_6string(self__190, key__191)
    retv231 = t232
    return retv231
}

func _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string() *hashmap_FloatKey_string_x {
    var retv234 *hashmap_FloatKey_string_x
    var t235 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(self__192 *hashmap_FloatKey_string_x, key__193 FloatKey, value__194 string) struct{} {
    hashmap_set__HashMap_8FloatKey_6string(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(self__190 *hashmap_FloatKey_string_x, key__191 FloatKey) Option__string {
    var retv239 Option__string
    var t240 Option__string = hashmap_get__HashMap_8FloatKey_6string(self__190, key__191)
    retv239 = t240
    return retv239
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var retv242 *hashmap_CollisionKey_int32_x
    var t243 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    retv242 = t243
    return retv242
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__192 *hashmap_CollisionKey_int32_x, key__193 CollisionKey, value__194 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__195 *hashmap_CollisionKey_int32_x, key__196 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__195, key__196)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__190 *hashmap_CollisionKey_int32_x, key__191 CollisionKey) Option__int32 {
    var retv249 Option__int32
    var t250 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__190, key__191)
    retv249 = t250
    return retv249
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__197 *hashmap_CollisionKey_int32_x) int32 {
    var retv252 int32
    var t253 int32 = hashmap_len__HashMap_12CollisionKey_5int32(self__197)
    retv252 = t253
    return retv252
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv255 *ref_int32_x
    var t256 *ref_int32_x = ref__Ref_5int32(value__201)
    retv255 = t256
    return retv255
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv258 int32
    var t259 int32 = ref_get__Ref_5int32(self__202)
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var retv263 *hashmap_Ref_12CollisionKey_string_x
    var t264 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    retv263 = t264
    return retv263
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__201 CollisionKey) *ref_CollisionKey_x {
    var retv266 *ref_CollisionKey_x
    var t267 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__201)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__192 *hashmap_Ref_12CollisionKey_string_x, key__193 *ref_CollisionKey_x, value__194 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__192, key__193, value__194)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t271 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t271)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__88 *ref_CollisionKey_x, other__89 *ref_CollisionKey_x) bool {
    var retv274 bool
    var t275 bool = ptr_eq__Ref_12CollisionKey(self__88, other__89)
    retv274 = t275
    return retv274
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__90 *ref_CollisionKey_x) uint64 {
    var retv277 uint64
    var t278 uint64 = ptr_hash__Ref_12CollisionKey(self__90)
    retv277 = t278
    return retv277
}

func _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(self__190 *hashmap_Ref_12CollisionKey_string_x, key__191 *ref_CollisionKey_x) Option__string {
    var retv280 Option__string
    var t281 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(self__190, key__191)
    retv280 = t281
    return retv280
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(self__203 *ref_CollisionKey_x, value__204 CollisionKey) struct{} {
    ref_set__Ref_12CollisionKey(self__203, value__204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv285 string
    retv285 = self__34
    return retv285
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv287 string
    var t288 string = _goml_runtime_core_int32_to_string(self__38)
    retv287 = t288
    return retv287
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__70 float32, other__71 float32) bool {
    var retv290 bool
    var t291 bool = self__70 == other__71
    retv290 = t291
    return retv290
}

func main() {
    main0()
}
