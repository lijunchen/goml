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

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__0 FloatKey) uint64 {
    var h__1 uint64 = 14695981039346656037
    var t185 uint64 = h__1 * 1099511628211
    var t186 float64 = self__0.value
    var t187 uint64
    var inline386 uint64 = _goml_runtime_core_float64_hash(t186)
    t187 = inline386
    var h__2 uint64 = t185 + t187
    return h__2
}

func _goml_m_trait__impl_i_Eq_i_FloatKey_i_eq(self__3 FloatKey, other__4 FloatKey) bool {
    var t192 float64 = self__3.value
    var t193 float64 = other__4.value
    var inline388 bool = t192 == t193
    return inline388
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t197 int32 = self__5.value
    var t198 int32 = other__6.value
    var inline390 bool = t197 == t198
    return inline390
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        var inline392 string = "none"
        var inline393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline392)
        _goml_runtime_core_string_println(inline393)
        return struct{}{}
    case Option__string_Some:
        var x136 string = value__8.(Option__string_Some)._0
        var inline396 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x136)
        _goml_runtime_core_string_println(inline396)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline399 string = "none"
        var inline400 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline399)
        _goml_runtime_core_string_println(inline400)
        return struct{}{}
    case Option__int32_Some:
        var x137 int32 = value__10.(Option__int32_Some)._0
        var inline403 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x137)
        _goml_runtime_core_string_println(inline403)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t211 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t212 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t213 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t211, t212)
    var t214 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t213)
    println__T_string(t214)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t215 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t215)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t216 int
    var inline450 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t216 = inline450
    println__T_int(t216)
    var t217 Option__string
    var inline448 Option__string = hashmap_get__HashMap_7float32_6string(values32__14, zero32__12)
    t217 = inline448
    print_opt_string(t217)
    hashmap_remove__HashMap_7float32_6string(values32__14, negative_zero32__13)
    var t218 int
    var inline444 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t218 = inline444
    var inline441 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t218)
    _goml_runtime_core_string_println(inline441)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t219 uint64
    var inline439 uint64 = _goml_runtime_core_float64_hash(zero64__15)
    t219 = inline439
    var t220 uint64
    var inline437 uint64 = _goml_runtime_core_float64_hash(negative_zero64__16)
    t220 = inline437
    var t221 bool
    var inline435 bool = t219 == t220
    t221 = inline435
    var t222 string
    var inline433 string = _goml_runtime_core_bool_to_string(t221)
    t222 = inline433
    var inline430 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline430)
    var values64__17 *hashmap_float64_string_x
    var inline428 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    values64__17 = inline428
    var inline425 string = "f64"
    hashmap_set__HashMap_7float64_6string(values64__17, zero64__15, inline425)
    var t223 Option__string
    var inline423 Option__string = hashmap_get__HashMap_7float64_6string(values64__17, negative_zero64__16)
    t223 = inline423
    switch t223.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline419 string = t223.(Option__string_Some)._0
        println__T_string(inline419)
    default:
        panic("non-exhaustive match")
    }
    var derived__18 *hashmap_FloatKey_string_x
    var inline416 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    derived__18 = inline416
    var t224 FloatKey = FloatKey{
        value: zero64__15,
    }
    var inline413 string = "derived"
    hashmap_set__HashMap_8FloatKey_6string(derived__18, t224, inline413)
    var t225 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t226 Option__string
    var inline411 Option__string = hashmap_get__HashMap_8FloatKey_6string(derived__18, t225)
    t226 = inline411
    switch t226.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline407 string = t226.(Option__string_Some)._0
        println__T_string(inline407)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t228 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t228, 10)
    var t229 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t229, 20)
    var t230 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t230, 30)
    var t231 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t231)
    var t232 CollisionKey = CollisionKey{
        value: 1,
    }
    var t233 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t232)
    print_opt_int(t233)
    var t234 CollisionKey = CollisionKey{
        value: 2,
    }
    var t235 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t234)
    print_opt_int(t235)
    var t236 CollisionKey = CollisionKey{
        value: 3,
    }
    var t237 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t236)
    print_opt_int(t237)
    var t238 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t238, 40)
    var t239 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t239)
    var t240 CollisionKey = CollisionKey{
        value: 4,
    }
    var t241 Option__int32
    var inline498 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t240)
    t241 = inline498
    print_opt_int(t241)
    var t242 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline495 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__19, t242, inline495)
    var t243 int
    var inline493 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t243 = inline493
    var inline490 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t243)
    _goml_runtime_core_string_println(inline490)
    var t244 CollisionKey = CollisionKey{
        value: 4,
    }
    var t245 Option__int32
    var inline488 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t244)
    t245 = inline488
    switch t245.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline484 int32 = t245.(Option__int32_Some)._0
        println__T_int32(inline484)
    default:
        panic("non-exhaustive match")
    }
    var t246 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t246)
    var t247 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t247)
    var t248 int
    var inline477 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t248 = inline477
    var inline474 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t248)
    _goml_runtime_core_string_println(inline474)
    var index__20 *ref_int32_x
    var inline471 int32 = 0
    var inline472 *ref_int32_x = ref__Ref_5int32(inline471)
    index__20 = inline472
    Loop_loop251:
    for {
        var t252 int32
        var inline464 int32 = ref_get__Ref_5int32(index__20)
        t252 = inline464
        var t253 bool = t252 < 2000
        if t253 {
            var t254 int32
            var inline462 int32 = ref_get__Ref_5int32(index__20)
            t254 = inline462
            var t255 int32 = 1000 + t254
            var key__21 CollisionKey = CollisionKey{
                value: t255,
            }
            var t256 int32
            var inline460 int32 = ref_get__Ref_5int32(index__20)
            t256 = inline460
            hashmap_set__HashMap_12CollisionKey_5int32(values__19, key__21, t256)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__19, key__21)
            var t257 int32
            var inline454 int32 = ref_get__Ref_5int32(index__20)
            t257 = inline454
            var t258 int32 = t257 + 1
            ref_set__Ref_5int32(index__20, t258)
            continue
        } else {
            break Loop_loop251
        }
    }
    var t250 int
    var inline469 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t250 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t250)
    _goml_runtime_core_string_println(inline466)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t260 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t260)
    var t261 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t261)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t262 bool
    var inline540 bool = ptr_eq__Ref_12CollisionKey(key__23, key__23)
    t262 = inline540
    var inline537 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t262)
    _goml_runtime_core_string_println(inline537)
    var t263 bool
    var inline535 bool = ptr_eq__Ref_12CollisionKey(key__23, equal_value__25)
    t263 = inline535
    var inline532 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t263)
    _goml_runtime_core_string_println(inline532)
    var t264 uint64
    var inline530 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t264 = inline530
    var t265 uint64
    var inline528 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t265 = inline528
    var t266 bool
    var inline526 bool = t264 == t265
    t266 = inline526
    var inline523 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t266)
    _goml_runtime_core_string_println(inline523)
    var t267 Option__string
    var inline521 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t267 = inline521
    switch t267.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline517 string = t267.(Option__string_Some)._0
        println__T_string(inline517)
    default:
        panic("non-exhaustive match")
    }
    var t268 Option__string
    var inline514 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, equal_value__25)
    t268 = inline514
    switch t268.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline510 string = t268.(Option__string_Some)._0
        println__T_string(inline510)
    default:
        panic("non-exhaustive match")
    }
    var t269 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__23, t269)
    var t270 Option__string
    var inline505 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t270 = inline505
    switch t270.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline501 string = t270.(Option__string_Some)._0
        println__T_string(inline501)
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

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__124 float64) uint64 {
    var t274 uint64 = _goml_runtime_core_float64_hash(self__124)
    return t274
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__108 float64, other__109 float64) bool {
    var t277 bool = self__108 == other__109
    return t277
}

func println__T_string(value__31 string) struct{} {
    var t282 string
    t282 = value__31
    _goml_runtime_core_string_println(t282)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t285 string
    var inline543 string = _goml_runtime_core_int32_to_string(value__31)
    t285 = inline543
    _goml_runtime_core_string_println(t285)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__123 float32) uint64 {
    var t289 uint64 = _goml_runtime_core_float32_hash(self__123)
    return t289
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__104 uint64, other__105 uint64) bool {
    var t292 bool = self__104 == other__105
    return t292
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t295 string = _goml_runtime_core_bool_to_string(self__66)
    return t295
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var t298 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    return t298
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__206 *hashmap_float32_string_x, key__207 float32, value__208 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__206, key__207, value__208)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__204 *hashmap_float32_string_x, key__205 float32) Option__string {
    var t303 Option__string = hashmap_get__HashMap_7float32_6string(self__204, key__205)
    return t303
}

func println__T_int(value__31 int) struct{} {
    var t305 string
    var inline545 string = _goml_runtime_core_int_to_string(value__31)
    t305 = inline545
    _goml_runtime_core_string_println(t305)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t330 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t330
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__206 *hashmap_CollisionKey_int32_x, key__207 CollisionKey, value__208 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__206, key__207, value__208)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__209 *hashmap_CollisionKey_int32_x, key__210 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__209, key__210)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__204 *hashmap_CollisionKey_int32_x, key__205 CollisionKey) Option__int32 {
    var t337 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__204, key__205)
    return t337
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__211 *hashmap_CollisionKey_int32_x) int {
    var t340 int = hashmap_len__HashMap_12CollisionKey_5int32(self__211)
    return t340
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t351 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t351
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__215 CollisionKey) *ref_CollisionKey_x {
    var t354 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__215)
    return t354
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__206 *hashmap_Ref_12CollisionKey_string_x, key__207 *ref_CollisionKey_x, value__208 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__206, key__207, value__208)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__125 *ref_CollisionKey_x, other__126 *ref_CollisionKey_x) bool {
    var t362 bool = ptr_eq__Ref_12CollisionKey(self__125, other__126)
    return t362
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__127 *ref_CollisionKey_x) uint64 {
    var t365 uint64 = ptr_hash__Ref_12CollisionKey(self__127)
    return t365
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t375 string = _goml_runtime_core_int32_to_string(self__72)
    return t375
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t378 string = _goml_runtime_core_int_to_string(self__69)
    return t378
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__106 float32, other__107 float32) bool {
    var t381 bool = self__106 == other__107
    return t381
}

func main() {
    main0()
}
