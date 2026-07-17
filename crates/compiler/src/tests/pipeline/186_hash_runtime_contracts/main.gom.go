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
    var retv109 bool
    var jp111 bool
    if true {
        var t112 float64 = self__0.value
        var t113 float64 = other__1.value
        var t114 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(t112, t113)
        jp111 = t114
    } else {
        jp111 = false
    }
    retv109 = jp111
    return retv109
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var retv116 uint64
    var h__3 uint64 = 14695981039346656037
    var t117 uint64 = h__3 * 1099511628211
    var t118 float64 = self__2.value
    var t119 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(t118)
    var h__4 uint64 = t117 + t119
    retv116 = h__4
    return retv116
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var retv121 bool
    var t122 int32 = self__5.value
    var t123 int32 = other__6.value
    var t124 bool = t122 == t123
    retv121 = t124
    return retv121
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    var retv126 uint64
    retv126 = 1
    return retv126
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var x61 string = value__8.(Option__string_Some)._0
        var text__9 string = x61
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
        var x62 int32 = value__10.(Option__int32_Some)._0
        var number__11 int32 = x62
        println__T_int32(number__11)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t136 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t137 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t138 bool = t136 == t137
    var t139 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t138)
    println__T_string(t139)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t140 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t140)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t141 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t141)
    var t142 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, zero32__12)
    print_opt_string(t142)
    _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(values32__14, negative_zero32__13)
    var t143 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t143)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t144 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(zero64__15)
    var t145 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(negative_zero64__16)
    var t146 bool = t144 == t145
    var t147 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t146)
    println__T_string(t147)
    var values64__17 *hashmap_float64_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(values64__17, zero64__15, "f64")
    var t148 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(values64__17, negative_zero64__16)
    print_opt_string(t148)
    var derived__18 *hashmap_FloatKey_string_x = _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string()
    var t149 FloatKey = FloatKey{
        value: zero64__15,
    }
    _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(derived__18, t149, "derived")
    var t150 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t151 Option__string = _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(derived__18, t150)
    print_opt_string(t151)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t153 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t153, 10)
    var t154 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t154, 20)
    var t155 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t155, 30)
    var t156 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t156)
    var t157 CollisionKey = CollisionKey{
        value: 1,
    }
    var t158 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t157)
    print_opt_int(t158)
    var t159 CollisionKey = CollisionKey{
        value: 2,
    }
    var t160 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t159)
    print_opt_int(t160)
    var t161 CollisionKey = CollisionKey{
        value: 3,
    }
    var t162 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t161)
    print_opt_int(t162)
    var t163 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t163, 40)
    var t164 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t164)
    var t165 CollisionKey = CollisionKey{
        value: 4,
    }
    var t166 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t165)
    print_opt_int(t166)
    var t167 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t167, 41)
    var t168 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t168)
    var t169 CollisionKey = CollisionKey{
        value: 4,
    }
    var t170 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t169)
    print_opt_int(t170)
    var t171 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t171)
    var t172 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t172)
    var t173 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t173)
    var index__20 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop176:
    for {
        var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
        var t178 bool = t177 < 2000
        if t178 {
            var t179 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t180 int32 = 1000 + t179
            var key__21 CollisionKey = CollisionKey{
                value: t180,
            }
            var t181 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, key__21, t181)
            _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, key__21)
            var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t183 int32 = t182 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__20, t183)
            continue
        } else {
            break Loop_loop176
        }
    }
    var t175 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t175)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t185 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t185)
    var alias__24 *ref_CollisionKey_x = key__23
    var t186 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t186)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t187 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, alias__24)
    println__T_bool(t187)
    var t188 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, equal_value__25)
    println__T_bool(t188)
    var t189 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key__23)
    var t190 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(alias__24)
    var t191 bool = t189 == t190
    println__T_bool(t191)
    var t192 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, alias__24)
    print_opt_string(t192)
    var t193 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, equal_value__25)
    print_opt_string(t193)
    var t194 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(key__23, t194)
    var t195 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, key__23)
    print_opt_string(t195)
    return struct{}{}
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__75 float64, other__76 float64) bool {
    var retv198 bool
    var t199 bool = self__75 == other__76
    retv198 = t199
    return retv198
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__90 float64) uint64 {
    var retv201 uint64
    var t202 uint64 = _goml_runtime_core_float64_hash(self__90)
    retv201 = t202
    return retv201
}

func println__T_string(value__1 string) struct{} {
    var t204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t207 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__89 float32) uint64 {
    var retv210 uint64
    var t211 uint64 = _goml_runtime_core_float32_hash(self__89)
    retv210 = t211
    return retv210
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv213 string
    var t214 string = _goml_runtime_core_bool_to_string(self__36)
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var retv216 *hashmap_float32_string_x
    var t217 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    retv216 = t217
    return retv216
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__195 *hashmap_float32_string_x, key__196 float32, value__197 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__193 *hashmap_float32_string_x, key__194 float32) Option__string {
    var retv221 Option__string
    var t222 Option__string = hashmap_get__HashMap_7float32_6string(self__193, key__194)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(self__200 *hashmap_float32_string_x) int32 {
    var retv224 int32
    var t225 int32 = hashmap_len__HashMap_7float32_6string(self__200)
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(self__198 *hashmap_float32_string_x, key__199 float32) struct{} {
    hashmap_remove__HashMap_7float32_6string(self__198, key__199)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string() *hashmap_float64_string_x {
    var retv229 *hashmap_float64_string_x
    var t230 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(self__195 *hashmap_float64_string_x, key__196 float64, value__197 string) struct{} {
    hashmap_set__HashMap_7float64_6string(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(self__193 *hashmap_float64_string_x, key__194 float64) Option__string {
    var retv234 Option__string
    var t235 Option__string = hashmap_get__HashMap_7float64_6string(self__193, key__194)
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string() *hashmap_FloatKey_string_x {
    var retv237 *hashmap_FloatKey_string_x
    var t238 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    retv237 = t238
    return retv237
}

func _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(self__195 *hashmap_FloatKey_string_x, key__196 FloatKey, value__197 string) struct{} {
    hashmap_set__HashMap_8FloatKey_6string(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(self__193 *hashmap_FloatKey_string_x, key__194 FloatKey) Option__string {
    var retv242 Option__string
    var t243 Option__string = hashmap_get__HashMap_8FloatKey_6string(self__193, key__194)
    retv242 = t243
    return retv242
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var retv245 *hashmap_CollisionKey_int32_x
    var t246 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    retv245 = t246
    return retv245
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__195 *hashmap_CollisionKey_int32_x, key__196 CollisionKey, value__197 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__198 *hashmap_CollisionKey_int32_x, key__199 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__198, key__199)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__193 *hashmap_CollisionKey_int32_x, key__194 CollisionKey) Option__int32 {
    var retv252 Option__int32
    var t253 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__193, key__194)
    retv252 = t253
    return retv252
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__200 *hashmap_CollisionKey_int32_x) int32 {
    var retv255 int32
    var t256 int32 = hashmap_len__HashMap_12CollisionKey_5int32(self__200)
    retv255 = t256
    return retv255
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv258 *ref_int32_x
    var t259 *ref_int32_x = ref__Ref_5int32(value__204)
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv261 int32
    var t262 int32 = ref_get__Ref_5int32(self__205)
    retv261 = t262
    return retv261
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var retv266 *hashmap_Ref_12CollisionKey_string_x
    var t267 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__204 CollisionKey) *ref_CollisionKey_x {
    var retv269 *ref_CollisionKey_x
    var t270 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__204)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__195 *hashmap_Ref_12CollisionKey_string_x, key__196 *ref_CollisionKey_x, value__197 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__195, key__196, value__197)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t274 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t274)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__91 *ref_CollisionKey_x, other__92 *ref_CollisionKey_x) bool {
    var retv277 bool
    var t278 bool = ptr_eq__Ref_12CollisionKey(self__91, other__92)
    retv277 = t278
    return retv277
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__93 *ref_CollisionKey_x) uint64 {
    var retv280 uint64
    var t281 uint64 = ptr_hash__Ref_12CollisionKey(self__93)
    retv280 = t281
    return retv280
}

func _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(self__193 *hashmap_Ref_12CollisionKey_string_x, key__194 *ref_CollisionKey_x) Option__string {
    var retv283 Option__string
    var t284 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(self__193, key__194)
    retv283 = t284
    return retv283
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(self__206 *ref_CollisionKey_x, value__207 CollisionKey) struct{} {
    ref_set__Ref_12CollisionKey(self__206, value__207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv288 string
    retv288 = self__37
    return retv288
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv290 string
    var t291 string = _goml_runtime_core_int32_to_string(self__41)
    retv290 = t291
    return retv290
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__73 float32, other__74 float32) bool {
    var retv293 bool
    var t294 bool = self__73 == other__74
    retv293 = t294
    return retv293
}

func main() {
    main0()
}
