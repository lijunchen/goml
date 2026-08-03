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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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
    len int
}

func hashmap_new__HashMap_7float32_6string() *hashmap_float32_string_x {
    return &hashmap_float32_string_x{
        buckets: make(map[uint64][]hashmap_float32_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_7float32_6string(m *hashmap_float32_string_x) int {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(key)
    var bucket []hashmap_float32_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(key)
    var bucket []hashmap_float64_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(key)
    var bucket []hashmap_FloatKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
}

func hashmap_new__HashMap_12CollisionKey_5int32() *hashmap_CollisionKey_int32_x {
    return &hashmap_CollisionKey_int32_x{
        buckets: make(map[uint64][]hashmap_CollisionKey_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x) int {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var t228 float64 = self__0.value
    var t229 float64 = other__1.value
    var inline427 bool = t228 == t229
    return inline427
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var h__3 uint64 = 14695981039346656037
    var t233 uint64 = h__3 * 1099511628211
    var t234 float64 = self__2.value
    var t235 uint64
    var inline429 uint64 = _goml_runtime_core_float64_hash(t234)
    t235 = inline429
    var h__4 uint64 = t233 + t235
    return h__4
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t238 int32 = self__5.value
    var t239 int32 = other__6.value
    var inline431 bool = t238 == t239
    return inline431
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        var inline433 string = "none"
        var inline434 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline433)
        _goml_runtime_core_string_println(inline434)
        return struct{}{}
    case Option__string_Some:
        var x177 string = value__8.(Option__string_Some)._0
        var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x177)
        _goml_runtime_core_string_println(inline437)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline440 string = "none"
        var inline441 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline440)
        _goml_runtime_core_string_println(inline441)
        return struct{}{}
    case Option__int32_Some:
        var x178 int32 = value__10.(Option__int32_Some)._0
        var inline444 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x178)
        _goml_runtime_core_string_println(inline444)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t252 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t253 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t254 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t252, t253)
    var t255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t254)
    println__T_string(t255)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t256 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t256)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t257 int
    var inline491 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t257 = inline491
    println__T_int(t257)
    var t258 Option__string
    var inline489 Option__string = hashmap_get__HashMap_7float32_6string(values32__14, zero32__12)
    t258 = inline489
    print_opt_string(t258)
    hashmap_remove__HashMap_7float32_6string(values32__14, negative_zero32__13)
    var t259 int
    var inline485 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t259 = inline485
    var inline482 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t259)
    _goml_runtime_core_string_println(inline482)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t260 uint64
    var inline480 uint64 = _goml_runtime_core_float64_hash(zero64__15)
    t260 = inline480
    var t261 uint64
    var inline478 uint64 = _goml_runtime_core_float64_hash(negative_zero64__16)
    t261 = inline478
    var t262 bool
    var inline476 bool = t260 == t261
    t262 = inline476
    var t263 string
    var inline474 string = _goml_runtime_core_bool_to_string(t262)
    t263 = inline474
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t263)
    _goml_runtime_core_string_println(inline471)
    var values64__17 *hashmap_float64_string_x
    var inline469 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    values64__17 = inline469
    var inline466 string = "f64"
    hashmap_set__HashMap_7float64_6string(values64__17, zero64__15, inline466)
    var t264 Option__string
    var inline464 Option__string = hashmap_get__HashMap_7float64_6string(values64__17, negative_zero64__16)
    t264 = inline464
    switch t264.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline460 string = t264.(Option__string_Some)._0
        println__T_string(inline460)
    default:
        panic("non-exhaustive match")
    }
    var derived__18 *hashmap_FloatKey_string_x
    var inline457 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    derived__18 = inline457
    var t265 FloatKey = FloatKey{
        value: zero64__15,
    }
    var inline454 string = "derived"
    hashmap_set__HashMap_8FloatKey_6string(derived__18, t265, inline454)
    var t266 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t267 Option__string
    var inline452 Option__string = hashmap_get__HashMap_8FloatKey_6string(derived__18, t266)
    t267 = inline452
    switch t267.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline448 string = t267.(Option__string_Some)._0
        println__T_string(inline448)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t269 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t269, 10)
    var t270 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t270, 20)
    var t271 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t271, 30)
    var t272 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t272)
    var t273 CollisionKey = CollisionKey{
        value: 1,
    }
    var t274 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t273)
    print_opt_int(t274)
    var t275 CollisionKey = CollisionKey{
        value: 2,
    }
    var t276 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t275)
    print_opt_int(t276)
    var t277 CollisionKey = CollisionKey{
        value: 3,
    }
    var t278 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t277)
    print_opt_int(t278)
    var t279 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t279, 40)
    var t280 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t280)
    var t281 CollisionKey = CollisionKey{
        value: 4,
    }
    var t282 Option__int32
    var inline539 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t281)
    t282 = inline539
    print_opt_int(t282)
    var t283 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline536 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__19, t283, inline536)
    var t284 int
    var inline534 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t284 = inline534
    var inline531 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t284)
    _goml_runtime_core_string_println(inline531)
    var t285 CollisionKey = CollisionKey{
        value: 4,
    }
    var t286 Option__int32
    var inline529 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t285)
    t286 = inline529
    switch t286.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline525 int32 = t286.(Option__int32_Some)._0
        println__T_int32(inline525)
    default:
        panic("non-exhaustive match")
    }
    var t287 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t287)
    var t288 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t288)
    var t289 int
    var inline518 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t289 = inline518
    var inline515 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t289)
    _goml_runtime_core_string_println(inline515)
    var index__20 *ref_int32_x
    var inline512 int32 = 0
    var inline513 *ref_int32_x = ref__Ref_5int32(inline512)
    index__20 = inline513
    Loop_loop292:
    for {
        var t293 int32
        var inline505 int32 = ref_get__Ref_5int32(index__20)
        t293 = inline505
        var t294 bool = t293 < 2000
        if t294 {
            var t295 int32
            var inline503 int32 = ref_get__Ref_5int32(index__20)
            t295 = inline503
            var t296 int32 = 1000 + t295
            var key__21 CollisionKey = CollisionKey{
                value: t296,
            }
            var t297 int32
            var inline501 int32 = ref_get__Ref_5int32(index__20)
            t297 = inline501
            hashmap_set__HashMap_12CollisionKey_5int32(values__19, key__21, t297)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__19, key__21)
            var t298 int32
            var inline495 int32 = ref_get__Ref_5int32(index__20)
            t298 = inline495
            var t299 int32 = t298 + 1
            ref_set__Ref_5int32(index__20, t299)
            continue
        } else {
            break Loop_loop292
        }
    }
    var t291 int
    var inline510 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t291 = inline510
    var inline507 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t291)
    _goml_runtime_core_string_println(inline507)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t301 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t301)
    var t302 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t302)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t303 bool
    var inline581 bool = ptr_eq__Ref_12CollisionKey(key__23, key__23)
    t303 = inline581
    var inline578 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t303)
    _goml_runtime_core_string_println(inline578)
    var t304 bool
    var inline576 bool = ptr_eq__Ref_12CollisionKey(key__23, equal_value__25)
    t304 = inline576
    var inline573 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t304)
    _goml_runtime_core_string_println(inline573)
    var t305 uint64
    var inline571 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t305 = inline571
    var t306 uint64
    var inline569 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t306 = inline569
    var t307 bool
    var inline567 bool = t305 == t306
    t307 = inline567
    var inline564 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t307)
    _goml_runtime_core_string_println(inline564)
    var t308 Option__string
    var inline562 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t308 = inline562
    switch t308.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline558 string = t308.(Option__string_Some)._0
        println__T_string(inline558)
    default:
        panic("non-exhaustive match")
    }
    var t309 Option__string
    var inline555 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, equal_value__25)
    t309 = inline555
    switch t309.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline551 string = t309.(Option__string_Some)._0
        println__T_string(inline551)
    default:
        panic("non-exhaustive match")
    }
    var t310 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__23, t310)
    var t311 Option__string
    var inline546 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t311 = inline546
    switch t311.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline542 string = t311.(Option__string_Some)._0
        println__T_string(inline542)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__108 float64, other__109 float64) bool {
    var t315 bool = self__108 == other__109
    return t315
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__124 float64) uint64 {
    var t318 uint64 = _goml_runtime_core_float64_hash(self__124)
    return t318
}

func println__T_string(value__31 string) struct{} {
    var t323 string
    t323 = value__31
    _goml_runtime_core_string_println(t323)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t326 string
    var inline584 string = _goml_runtime_core_int32_to_string(value__31)
    t326 = inline584
    _goml_runtime_core_string_println(t326)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__123 float32) uint64 {
    var t330 uint64 = _goml_runtime_core_float32_hash(self__123)
    return t330
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__104 uint64, other__105 uint64) bool {
    var t333 bool = self__104 == other__105
    return t333
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t336 string = _goml_runtime_core_bool_to_string(self__66)
    return t336
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var t339 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    return t339
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__227 *hashmap_float32_string_x, key__228 float32, value__229 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__227, key__228, value__229)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__225 *hashmap_float32_string_x, key__226 float32) Option__string {
    var t344 Option__string = hashmap_get__HashMap_7float32_6string(self__225, key__226)
    return t344
}

func println__T_int(value__31 int) struct{} {
    var t346 string
    var inline586 string = _goml_runtime_core_int_to_string(value__31)
    t346 = inline586
    _goml_runtime_core_string_println(t346)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t371 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t371
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__227 *hashmap_CollisionKey_int32_x, key__228 CollisionKey, value__229 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__227, key__228, value__229)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__230 *hashmap_CollisionKey_int32_x, key__231 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__230, key__231)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__225 *hashmap_CollisionKey_int32_x, key__226 CollisionKey) Option__int32 {
    var t378 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__225, key__226)
    return t378
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__232 *hashmap_CollisionKey_int32_x) int {
    var t381 int = hashmap_len__HashMap_12CollisionKey_5int32(self__232)
    return t381
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t392 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t392
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__236 CollisionKey) *ref_CollisionKey_x {
    var t395 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__236)
    return t395
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__227 *hashmap_Ref_12CollisionKey_string_x, key__228 *ref_CollisionKey_x, value__229 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__227, key__228, value__229)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__125 *ref_CollisionKey_x, other__126 *ref_CollisionKey_x) bool {
    var t403 bool = ptr_eq__Ref_12CollisionKey(self__125, other__126)
    return t403
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__127 *ref_CollisionKey_x) uint64 {
    var t406 uint64 = ptr_hash__Ref_12CollisionKey(self__127)
    return t406
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t416 string = _goml_runtime_core_int32_to_string(self__72)
    return t416
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t419 string = _goml_runtime_core_int_to_string(self__69)
    return t419
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__106 float32, other__107 float32) bool {
    var t422 bool = self__106 == other__107
    return t422
}

func main() {
    main0()
}
