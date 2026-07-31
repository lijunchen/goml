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
    var retv200 bool
    var jp202 bool
    if true {
        var t203 float64 = self__0.value
        var t204 float64 = other__1.value
        var t205 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(t203, t204)
        jp202 = t205
    } else {
        jp202 = false
    }
    retv200 = jp202
    return retv200
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var retv207 uint64
    var h__3 uint64 = 14695981039346656037
    var t208 uint64 = h__3 * 1099511628211
    var t209 float64 = self__2.value
    var t210 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(t209)
    var h__4 uint64 = t208 + t210
    retv207 = h__4
    return retv207
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var retv212 bool
    var t213 int32 = self__5.value
    var t214 int32 = other__6.value
    var t215 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t213, t214)
    retv212 = t215
    return retv212
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    var retv217 uint64
    retv217 = 1
    return retv217
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var x152 string = value__8.(Option__string_Some)._0
        var text__9 string = x152
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
        var x153 int32 = value__10.(Option__int32_Some)._0
        var number__11 int32 = x153
        println__T_int32(number__11)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t227 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t228 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t229 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t227, t228)
    var t230 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t229)
    println__T_string(t230)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t231 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t231)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t232 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int(t232)
    var t233 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, zero32__12)
    print_opt_string(t233)
    _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(values32__14, negative_zero32__13)
    var t234 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int(t234)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t235 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(zero64__15)
    var t236 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(negative_zero64__16)
    var t237 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t235, t236)
    var t238 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t237)
    println__T_string(t238)
    var values64__17 *hashmap_float64_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(values64__17, zero64__15, "f64")
    var t239 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(values64__17, negative_zero64__16)
    print_opt_string(t239)
    var derived__18 *hashmap_FloatKey_string_x = _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string()
    var t240 FloatKey = FloatKey{
        value: zero64__15,
    }
    _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(derived__18, t240, "derived")
    var t241 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t242 Option__string = _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(derived__18, t241)
    print_opt_string(t242)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t244 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t244, 10)
    var t245 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t245, 20)
    var t246 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t246, 30)
    var t247 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t247)
    var t248 CollisionKey = CollisionKey{
        value: 1,
    }
    var t249 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t248)
    print_opt_int(t249)
    var t250 CollisionKey = CollisionKey{
        value: 2,
    }
    var t251 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t250)
    print_opt_int(t251)
    var t252 CollisionKey = CollisionKey{
        value: 3,
    }
    var t253 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t252)
    print_opt_int(t253)
    var t254 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t254, 40)
    var t255 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t255)
    var t256 CollisionKey = CollisionKey{
        value: 4,
    }
    var t257 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t256)
    print_opt_int(t257)
    var t258 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t258, 41)
    var t259 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t259)
    var t260 CollisionKey = CollisionKey{
        value: 4,
    }
    var t261 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t260)
    print_opt_int(t261)
    var t262 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t262)
    var t263 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t263)
    var t264 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t264)
    var index__20 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop267:
    for {
        var t268 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
        var t269 bool = t268 < 2000
        if t269 {
            var t270 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t271 int32 = 1000 + t270
            var key__21 CollisionKey = CollisionKey{
                value: t271,
            }
            var t272 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, key__21, t272)
            _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, key__21)
            var t273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t274 int32 = t273 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__20, t274)
            continue
        } else {
            break Loop_loop267
        }
    }
    var t266 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t266)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t276 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t276)
    var alias__24 *ref_CollisionKey_x = key__23
    var t277 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t277)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t278 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, alias__24)
    println__T_bool(t278)
    var t279 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, equal_value__25)
    println__T_bool(t279)
    var t280 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key__23)
    var t281 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(alias__24)
    var t282 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t280, t281)
    println__T_bool(t282)
    var t283 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, alias__24)
    print_opt_string(t283)
    var t284 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, equal_value__25)
    print_opt_string(t284)
    var t285 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(key__23, t285)
    var t286 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, key__23)
    print_opt_string(t286)
    return struct{}{}
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv289 bool
    var t290 bool = self__79 == other__80
    retv289 = t290
    return retv289
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__95 float64) uint64 {
    var retv292 uint64
    var t293 uint64 = _goml_runtime_core_float64_hash(self__95)
    retv292 = t293
    return retv292
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv295 bool
    var t296 bool = self__65 == other__66
    retv295 = t296
    return retv295
}

func println__T_string(value__1 string) struct{} {
    var t298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t298)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t301 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__94 float32) uint64 {
    var retv304 uint64
    var t305 uint64 = _goml_runtime_core_float32_hash(self__94)
    retv304 = t305
    return retv304
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__75 uint64, other__76 uint64) bool {
    var retv307 bool
    var t308 bool = self__75 == other__76
    retv307 = t308
    return retv307
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv310 string
    var t311 string = _goml_runtime_core_bool_to_string(self__37)
    retv310 = t311
    return retv310
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var retv313 *hashmap_float32_string_x
    var t314 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    retv313 = t314
    return retv313
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__198 *hashmap_float32_string_x, key__199 float32, value__200 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__196 *hashmap_float32_string_x, key__197 float32) Option__string {
    var retv318 Option__string
    var t319 Option__string = hashmap_get__HashMap_7float32_6string(self__196, key__197)
    retv318 = t319
    return retv318
}

func println__T_int(value__1 int) struct{} {
    var t321 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t321)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(self__203 *hashmap_float32_string_x) int {
    var retv324 int
    var t325 int = hashmap_len__HashMap_7float32_6string(self__203)
    retv324 = t325
    return retv324
}

func _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(self__201 *hashmap_float32_string_x, key__202 float32) struct{} {
    hashmap_remove__HashMap_7float32_6string(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string() *hashmap_float64_string_x {
    var retv329 *hashmap_float64_string_x
    var t330 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    retv329 = t330
    return retv329
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(self__198 *hashmap_float64_string_x, key__199 float64, value__200 string) struct{} {
    hashmap_set__HashMap_7float64_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(self__196 *hashmap_float64_string_x, key__197 float64) Option__string {
    var retv334 Option__string
    var t335 Option__string = hashmap_get__HashMap_7float64_6string(self__196, key__197)
    retv334 = t335
    return retv334
}

func _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string() *hashmap_FloatKey_string_x {
    var retv337 *hashmap_FloatKey_string_x
    var t338 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    retv337 = t338
    return retv337
}

func _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(self__198 *hashmap_FloatKey_string_x, key__199 FloatKey, value__200 string) struct{} {
    hashmap_set__HashMap_8FloatKey_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(self__196 *hashmap_FloatKey_string_x, key__197 FloatKey) Option__string {
    var retv342 Option__string
    var t343 Option__string = hashmap_get__HashMap_8FloatKey_6string(self__196, key__197)
    retv342 = t343
    return retv342
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var retv345 *hashmap_CollisionKey_int32_x
    var t346 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    retv345 = t346
    return retv345
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__198 *hashmap_CollisionKey_int32_x, key__199 CollisionKey, value__200 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__201 *hashmap_CollisionKey_int32_x, key__202 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__196 *hashmap_CollisionKey_int32_x, key__197 CollisionKey) Option__int32 {
    var retv352 Option__int32
    var t353 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__196, key__197)
    retv352 = t353
    return retv352
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__203 *hashmap_CollisionKey_int32_x) int {
    var retv355 int
    var t356 int = hashmap_len__HashMap_12CollisionKey_5int32(self__203)
    retv355 = t356
    return retv355
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv358 *ref_int32_x
    var t359 *ref_int32_x = ref__Ref_5int32(value__207)
    retv358 = t359
    return retv358
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv361 int32
    var t362 int32 = ref_get__Ref_5int32(self__208)
    retv361 = t362
    return retv361
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var retv366 *hashmap_Ref_12CollisionKey_string_x
    var t367 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    retv366 = t367
    return retv366
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__207 CollisionKey) *ref_CollisionKey_x {
    var retv369 *ref_CollisionKey_x
    var t370 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__207)
    retv369 = t370
    return retv369
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__198 *hashmap_Ref_12CollisionKey_string_x, key__199 *ref_CollisionKey_x, value__200 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__198, key__199, value__200)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t374 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t374)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__96 *ref_CollisionKey_x, other__97 *ref_CollisionKey_x) bool {
    var retv377 bool
    var t378 bool = ptr_eq__Ref_12CollisionKey(self__96, other__97)
    retv377 = t378
    return retv377
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__98 *ref_CollisionKey_x) uint64 {
    var retv380 uint64
    var t381 uint64 = ptr_hash__Ref_12CollisionKey(self__98)
    retv380 = t381
    return retv380
}

func _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(self__196 *hashmap_Ref_12CollisionKey_string_x, key__197 *ref_CollisionKey_x) Option__string {
    var retv383 Option__string
    var t384 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(self__196, key__197)
    retv383 = t384
    return retv383
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(self__209 *ref_CollisionKey_x, value__210 CollisionKey) struct{} {
    ref_set__Ref_12CollisionKey(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv388 string
    retv388 = self__38
    return retv388
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv390 string
    var t391 string = _goml_runtime_core_int32_to_string(self__43)
    retv390 = t391
    return retv390
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv393 string
    var t394 string = _goml_runtime_core_int_to_string(self__40)
    retv393 = t394
    return retv393
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__77 float32, other__78 float32) bool {
    var retv396 bool
    var t397 bool = self__77 == other__78
    retv396 = t397
    return retv396
}

func main() {
    main0()
}
