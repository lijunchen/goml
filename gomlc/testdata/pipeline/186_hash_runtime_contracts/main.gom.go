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
    var t185_source int = 0
    var t185 uint64 = uint64(int(t185_source))
    var h__1 uint64 = t185 + 14695981039346656037
    var t186_source int = 0
    var t186 uint64 = uint64(int(t186_source))
    var t187 uint64 = t186 + 1099511628211
    var t188 uint64 = h__1 * t187
    var t189 float64 = self__0.value
    var t190 uint64
    var inline389 uint64 = _goml_runtime_core_float64_hash(t189)
    t190 = inline389
    var h__2 uint64 = t188 + t190
    return h__2
}

func _goml_m_trait__impl_i_Eq_i_FloatKey_i_eq(self__3 FloatKey, other__4 FloatKey) bool {
    var t195 float64 = self__3.value
    var t196 float64 = other__4.value
    var inline391 bool = t195 == t196
    return inline391
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t200 int32 = self__5.value
    var t201 int32 = other__6.value
    var inline393 bool = t200 == t201
    return inline393
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        var inline395 string = "none"
        var inline396 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline395)
        _goml_runtime_core_string_println(inline396)
        return struct{}{}
    case Option__string_Some:
        var x136 string = value__8.(Option__string_Some)._0
        var inline399 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x136)
        _goml_runtime_core_string_println(inline399)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline402 string = "none"
        var inline403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline402)
        _goml_runtime_core_string_println(inline403)
        return struct{}{}
    case Option__int32_Some:
        var x137 int32 = value__10.(Option__int32_Some)._0
        var inline406 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x137)
        _goml_runtime_core_string_println(inline406)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t214 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t215 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t216 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t214, t215)
    var t217 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t216)
    println__T_string(t217)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t218 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t218)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t219 int
    var inline453 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t219 = inline453
    println__T_int(t219)
    var t220 Option__string
    var inline451 Option__string = hashmap_get__HashMap_7float32_6string(values32__14, zero32__12)
    t220 = inline451
    print_opt_string(t220)
    hashmap_remove__HashMap_7float32_6string(values32__14, negative_zero32__13)
    var t221 int
    var inline447 int = hashmap_len__HashMap_7float32_6string(values32__14)
    t221 = inline447
    var inline444 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t221)
    _goml_runtime_core_string_println(inline444)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t222 uint64
    var inline442 uint64 = _goml_runtime_core_float64_hash(zero64__15)
    t222 = inline442
    var t223 uint64
    var inline440 uint64 = _goml_runtime_core_float64_hash(negative_zero64__16)
    t223 = inline440
    var t224 bool
    var inline438 bool = t222 == t223
    t224 = inline438
    var t225 string
    var inline436 string = _goml_runtime_core_bool_to_string(t224)
    t225 = inline436
    var inline433 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline433)
    var values64__17 *hashmap_float64_string_x
    var inline431 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    values64__17 = inline431
    var inline428 string = "f64"
    hashmap_set__HashMap_7float64_6string(values64__17, zero64__15, inline428)
    var t226 Option__string
    var inline426 Option__string = hashmap_get__HashMap_7float64_6string(values64__17, negative_zero64__16)
    t226 = inline426
    switch t226.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline422 string = t226.(Option__string_Some)._0
        println__T_string(inline422)
    default:
        panic("non-exhaustive match")
    }
    var derived__18 *hashmap_FloatKey_string_x
    var inline419 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    derived__18 = inline419
    var t227 FloatKey = FloatKey{
        value: zero64__15,
    }
    var inline416 string = "derived"
    hashmap_set__HashMap_8FloatKey_6string(derived__18, t227, inline416)
    var t228 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t229 Option__string
    var inline414 Option__string = hashmap_get__HashMap_8FloatKey_6string(derived__18, t228)
    t229 = inline414
    switch t229.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline410 string = t229.(Option__string_Some)._0
        println__T_string(inline410)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t231 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t231, 10)
    var t232 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t232, 20)
    var t233 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t233, 30)
    var t234 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t234)
    var t235 CollisionKey = CollisionKey{
        value: 1,
    }
    var t236 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t235)
    print_opt_int(t236)
    var t237 CollisionKey = CollisionKey{
        value: 2,
    }
    var t238 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t237)
    print_opt_int(t238)
    var t239 CollisionKey = CollisionKey{
        value: 3,
    }
    var t240 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t239)
    print_opt_int(t240)
    var t241 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t241, 40)
    var t242 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t242)
    var t243 CollisionKey = CollisionKey{
        value: 4,
    }
    var t244 Option__int32
    var inline501 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t243)
    t244 = inline501
    print_opt_int(t244)
    var t245 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline498 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__19, t245, inline498)
    var t246 int
    var inline496 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t246 = inline496
    var inline493 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t246)
    _goml_runtime_core_string_println(inline493)
    var t247 CollisionKey = CollisionKey{
        value: 4,
    }
    var t248 Option__int32
    var inline491 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__19, t247)
    t248 = inline491
    switch t248.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline487 int32 = t248.(Option__int32_Some)._0
        println__T_int32(inline487)
    default:
        panic("non-exhaustive match")
    }
    var t249 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t249)
    var t250 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__19, t250)
    var t251 int
    var inline480 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t251 = inline480
    var inline477 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t251)
    _goml_runtime_core_string_println(inline477)
    var index__20 *ref_int32_x
    var inline474 int32 = 0
    var inline475 *ref_int32_x = ref__Ref_5int32(inline474)
    index__20 = inline475
    Loop_loop254:
    for {
        var t255 int32
        var inline467 int32 = ref_get__Ref_5int32(index__20)
        t255 = inline467
        var t256 bool = t255 < 2000
        if t256 {
            var t257 int32
            var inline465 int32 = ref_get__Ref_5int32(index__20)
            t257 = inline465
            var t258 int32 = 1000 + t257
            var key__21 CollisionKey = CollisionKey{
                value: t258,
            }
            var t259 int32
            var inline463 int32 = ref_get__Ref_5int32(index__20)
            t259 = inline463
            hashmap_set__HashMap_12CollisionKey_5int32(values__19, key__21, t259)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__19, key__21)
            var t260 int32
            var inline457 int32 = ref_get__Ref_5int32(index__20)
            t260 = inline457
            var t261 int32 = t260 + 1
            ref_set__Ref_5int32(index__20, t261)
            continue
        } else {
            break Loop_loop254
        }
    }
    var t253 int
    var inline472 int = hashmap_len__HashMap_12CollisionKey_5int32(values__19)
    t253 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t253)
    _goml_runtime_core_string_println(inline469)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t263 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t263)
    var t264 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t264)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t265 bool
    var inline543 bool = ptr_eq__Ref_12CollisionKey(key__23, key__23)
    t265 = inline543
    var inline540 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t265)
    _goml_runtime_core_string_println(inline540)
    var t266 bool
    var inline538 bool = ptr_eq__Ref_12CollisionKey(key__23, equal_value__25)
    t266 = inline538
    var inline535 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t266)
    _goml_runtime_core_string_println(inline535)
    var t267 uint64
    var inline533 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t267 = inline533
    var t268 uint64
    var inline531 uint64 = ptr_hash__Ref_12CollisionKey(key__23)
    t268 = inline531
    var t269 bool
    var inline529 bool = t267 == t268
    t269 = inline529
    var inline526 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t269)
    _goml_runtime_core_string_println(inline526)
    var t270 Option__string
    var inline524 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t270 = inline524
    switch t270.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline520 string = t270.(Option__string_Some)._0
        println__T_string(inline520)
    default:
        panic("non-exhaustive match")
    }
    var t271 Option__string
    var inline517 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, equal_value__25)
    t271 = inline517
    switch t271.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline513 string = t271.(Option__string_Some)._0
        println__T_string(inline513)
    default:
        panic("non-exhaustive match")
    }
    var t272 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__23, t272)
    var t273 Option__string
    var inline508 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__22, key__23)
    t273 = inline508
    switch t273.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline504 string = t273.(Option__string_Some)._0
        println__T_string(inline504)
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

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__139 float64) uint64 {
    var t277 uint64 = _goml_runtime_core_float64_hash(self__139)
    return t277
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__123 float64, other__124 float64) bool {
    var t280 bool = self__123 == other__124
    return t280
}

func println__T_string(value__31 string) struct{} {
    var t285 string
    t285 = value__31
    _goml_runtime_core_string_println(t285)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t288 string
    var inline546 string = _goml_runtime_core_int32_to_string(value__31)
    t288 = inline546
    _goml_runtime_core_string_println(t288)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__138 float32) uint64 {
    var t292 uint64 = _goml_runtime_core_float32_hash(self__138)
    return t292
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__119 uint64, other__120 uint64) bool {
    var t295 bool = self__119 == other__120
    return t295
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t298 string = _goml_runtime_core_bool_to_string(self__66)
    return t298
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var t301 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    return t301
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__223 *hashmap_float32_string_x, key__224 float32, value__225 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__223, key__224, value__225)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__221 *hashmap_float32_string_x, key__222 float32) Option__string {
    var t306 Option__string = hashmap_get__HashMap_7float32_6string(self__221, key__222)
    return t306
}

func println__T_int(value__31 int) struct{} {
    var t308 string
    var inline548 string = _goml_runtime_core_int_to_string(value__31)
    t308 = inline548
    _goml_runtime_core_string_println(t308)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t333 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t333
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__223 *hashmap_CollisionKey_int32_x, key__224 CollisionKey, value__225 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__223, key__224, value__225)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__226 *hashmap_CollisionKey_int32_x, key__227 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__226, key__227)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__221 *hashmap_CollisionKey_int32_x, key__222 CollisionKey) Option__int32 {
    var t340 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__221, key__222)
    return t340
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__228 *hashmap_CollisionKey_int32_x) int {
    var t343 int = hashmap_len__HashMap_12CollisionKey_5int32(self__228)
    return t343
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t354 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t354
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__232 CollisionKey) *ref_CollisionKey_x {
    var t357 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__232)
    return t357
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__223 *hashmap_Ref_12CollisionKey_string_x, key__224 *ref_CollisionKey_x, value__225 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__223, key__224, value__225)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__140 *ref_CollisionKey_x, other__141 *ref_CollisionKey_x) bool {
    var t365 bool = ptr_eq__Ref_12CollisionKey(self__140, other__141)
    return t365
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__142 *ref_CollisionKey_x) uint64 {
    var t368 uint64 = ptr_hash__Ref_12CollisionKey(self__142)
    return t368
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t378 string = _goml_runtime_core_int32_to_string(self__72)
    return t378
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t381 string = _goml_runtime_core_int_to_string(self__69)
    return t381
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__121 float32, other__122 float32) bool {
    var t384 bool = self__121 == other__122
    return t384
}

func main() {
    main0()
}
