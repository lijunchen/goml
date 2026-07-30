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
    var retv156 bool
    var jp158 bool
    if true {
        var t159 float64 = self__0.value
        var t160 float64 = other__1.value
        var t161 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(t159, t160)
        jp158 = t161
    } else {
        jp158 = false
    }
    retv156 = jp158
    return retv156
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var retv163 uint64
    var h__3 uint64 = 14695981039346656037
    var t164 uint64 = h__3 * 1099511628211
    var t165 float64 = self__2.value
    var t166 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(t165)
    var h__4 uint64 = t164 + t166
    retv163 = h__4
    return retv163
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var retv168 bool
    var t169 int32 = self__5.value
    var t170 int32 = other__6.value
    var t171 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t169, t170)
    retv168 = t171
    return retv168
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    var retv173 uint64
    retv173 = 1
    return retv173
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var x108 string = value__8.(Option__string_Some)._0
        var text__9 string = x108
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
        var x109 int32 = value__10.(Option__int32_Some)._0
        var number__11 int32 = x109
        println__T_int32(number__11)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t183 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t184 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t185 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t183, t184)
    var t186 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t185)
    println__T_string(t186)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t187 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t187)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t188 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int(t188)
    var t189 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, zero32__12)
    print_opt_string(t189)
    _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(values32__14, negative_zero32__13)
    var t190 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int(t190)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t191 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(zero64__15)
    var t192 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(negative_zero64__16)
    var t193 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t191, t192)
    var t194 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t193)
    println__T_string(t194)
    var values64__17 *hashmap_float64_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(values64__17, zero64__15, "f64")
    var t195 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(values64__17, negative_zero64__16)
    print_opt_string(t195)
    var derived__18 *hashmap_FloatKey_string_x = _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string()
    var t196 FloatKey = FloatKey{
        value: zero64__15,
    }
    _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(derived__18, t196, "derived")
    var t197 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t198 Option__string = _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(derived__18, t197)
    print_opt_string(t198)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t200 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t200, 10)
    var t201 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t201, 20)
    var t202 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t202, 30)
    var t203 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t203)
    var t204 CollisionKey = CollisionKey{
        value: 1,
    }
    var t205 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t204)
    print_opt_int(t205)
    var t206 CollisionKey = CollisionKey{
        value: 2,
    }
    var t207 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t206)
    print_opt_int(t207)
    var t208 CollisionKey = CollisionKey{
        value: 3,
    }
    var t209 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t208)
    print_opt_int(t209)
    var t210 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t210, 40)
    var t211 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t211)
    var t212 CollisionKey = CollisionKey{
        value: 4,
    }
    var t213 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t212)
    print_opt_int(t213)
    var t214 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t214, 41)
    var t215 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t215)
    var t216 CollisionKey = CollisionKey{
        value: 4,
    }
    var t217 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t216)
    print_opt_int(t217)
    var t218 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t218)
    var t219 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t219)
    var t220 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t220)
    var index__20 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop223:
    for {
        var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
        var t225 bool = t224 < 2000
        if t225 {
            var t226 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t227 int32 = 1000 + t226
            var key__21 CollisionKey = CollisionKey{
                value: t227,
            }
            var t228 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, key__21, t228)
            _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, key__21)
            var t229 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t230 int32 = t229 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__20, t230)
            continue
        } else {
            break Loop_loop223
        }
    }
    var t222 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int(t222)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t232 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t232)
    var alias__24 *ref_CollisionKey_x = key__23
    var t233 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t233)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t234 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, alias__24)
    println__T_bool(t234)
    var t235 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, equal_value__25)
    println__T_bool(t235)
    var t236 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key__23)
    var t237 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(alias__24)
    var t238 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t236, t237)
    println__T_bool(t238)
    var t239 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, alias__24)
    print_opt_string(t239)
    var t240 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, equal_value__25)
    print_opt_string(t240)
    var t241 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(key__23, t241)
    var t242 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, key__23)
    print_opt_string(t242)
    return struct{}{}
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__79 float64, other__80 float64) bool {
    var retv245 bool
    var t246 bool = self__79 == other__80
    retv245 = t246
    return retv245
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__95 float64) uint64 {
    var retv248 uint64
    var t249 uint64 = _goml_runtime_core_float64_hash(self__95)
    retv248 = t249
    return retv248
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv251 bool
    var t252 bool = self__65 == other__66
    retv251 = t252
    return retv251
}

func println__T_string(value__1 string) struct{} {
    var t254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t257 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t257)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__94 float32) uint64 {
    var retv260 uint64
    var t261 uint64 = _goml_runtime_core_float32_hash(self__94)
    retv260 = t261
    return retv260
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__75 uint64, other__76 uint64) bool {
    var retv263 bool
    var t264 bool = self__75 == other__76
    retv263 = t264
    return retv263
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv266 string
    var t267 string = _goml_runtime_core_bool_to_string(self__37)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var retv269 *hashmap_float32_string_x
    var t270 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__198 *hashmap_float32_string_x, key__199 float32, value__200 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__196 *hashmap_float32_string_x, key__197 float32) Option__string {
    var retv274 Option__string
    var t275 Option__string = hashmap_get__HashMap_7float32_6string(self__196, key__197)
    retv274 = t275
    return retv274
}

func println__T_int(value__1 int) struct{} {
    var t277 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t277)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(self__203 *hashmap_float32_string_x) int {
    var retv280 int
    var t281 int = hashmap_len__HashMap_7float32_6string(self__203)
    retv280 = t281
    return retv280
}

func _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(self__201 *hashmap_float32_string_x, key__202 float32) struct{} {
    hashmap_remove__HashMap_7float32_6string(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string() *hashmap_float64_string_x {
    var retv285 *hashmap_float64_string_x
    var t286 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    retv285 = t286
    return retv285
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(self__198 *hashmap_float64_string_x, key__199 float64, value__200 string) struct{} {
    hashmap_set__HashMap_7float64_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(self__196 *hashmap_float64_string_x, key__197 float64) Option__string {
    var retv290 Option__string
    var t291 Option__string = hashmap_get__HashMap_7float64_6string(self__196, key__197)
    retv290 = t291
    return retv290
}

func _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string() *hashmap_FloatKey_string_x {
    var retv293 *hashmap_FloatKey_string_x
    var t294 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    retv293 = t294
    return retv293
}

func _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(self__198 *hashmap_FloatKey_string_x, key__199 FloatKey, value__200 string) struct{} {
    hashmap_set__HashMap_8FloatKey_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(self__196 *hashmap_FloatKey_string_x, key__197 FloatKey) Option__string {
    var retv298 Option__string
    var t299 Option__string = hashmap_get__HashMap_8FloatKey_6string(self__196, key__197)
    retv298 = t299
    return retv298
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var retv301 *hashmap_CollisionKey_int32_x
    var t302 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    retv301 = t302
    return retv301
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
    var retv308 Option__int32
    var t309 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__196, key__197)
    retv308 = t309
    return retv308
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__203 *hashmap_CollisionKey_int32_x) int {
    var retv311 int
    var t312 int = hashmap_len__HashMap_12CollisionKey_5int32(self__203)
    retv311 = t312
    return retv311
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv314 *ref_int32_x
    var t315 *ref_int32_x = ref__Ref_5int32(value__207)
    retv314 = t315
    return retv314
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv317 int32
    var t318 int32 = ref_get__Ref_5int32(self__208)
    retv317 = t318
    return retv317
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var retv322 *hashmap_Ref_12CollisionKey_string_x
    var t323 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    retv322 = t323
    return retv322
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__207 CollisionKey) *ref_CollisionKey_x {
    var retv325 *ref_CollisionKey_x
    var t326 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__207)
    retv325 = t326
    return retv325
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__198 *hashmap_Ref_12CollisionKey_string_x, key__199 *ref_CollisionKey_x, value__200 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__198, key__199, value__200)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t330 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t330)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__96 *ref_CollisionKey_x, other__97 *ref_CollisionKey_x) bool {
    var retv333 bool
    var t334 bool = ptr_eq__Ref_12CollisionKey(self__96, other__97)
    retv333 = t334
    return retv333
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__98 *ref_CollisionKey_x) uint64 {
    var retv336 uint64
    var t337 uint64 = ptr_hash__Ref_12CollisionKey(self__98)
    retv336 = t337
    return retv336
}

func _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(self__196 *hashmap_Ref_12CollisionKey_string_x, key__197 *ref_CollisionKey_x) Option__string {
    var retv339 Option__string
    var t340 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(self__196, key__197)
    retv339 = t340
    return retv339
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(self__209 *ref_CollisionKey_x, value__210 CollisionKey) struct{} {
    ref_set__Ref_12CollisionKey(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv344 string
    retv344 = self__38
    return retv344
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv346 string
    var t347 string = _goml_runtime_core_int32_to_string(self__43)
    retv346 = t347
    return retv346
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv349 string
    var t350 string = _goml_runtime_core_int_to_string(self__40)
    retv349 = t350
    return retv349
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__77 float32, other__78 float32) bool {
    var retv352 bool
    var t353 bool = self__77 == other__78
    retv352 = t353
    return retv352
}

func main() {
    main0()
}
