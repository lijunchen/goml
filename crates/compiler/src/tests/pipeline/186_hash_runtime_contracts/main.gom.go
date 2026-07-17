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
    len int32
}

func hashmap_new__HashMap_7float32_6string() *hashmap_float32_string_x {
    return &hashmap_float32_string_x{
        buckets: make(map[uint64][]hashmap_float32_string_x_entry),
        len: 0,
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
    len int32
}

func hashmap_new__HashMap_7float64_6string() *hashmap_float64_string_x {
    return &hashmap_float64_string_x{
        buckets: make(map[uint64][]hashmap_float64_string_x_entry),
        len: 0,
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
    len int32
}

func hashmap_new__HashMap_8FloatKey_6string() *hashmap_FloatKey_string_x {
    return &hashmap_FloatKey_string_x{
        buckets: make(map[uint64][]hashmap_FloatKey_string_x_entry),
        len: 0,
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
    len int32
}

func hashmap_new__HashMap_12CollisionKey_5int32() *hashmap_CollisionKey_int32_x {
    return &hashmap_CollisionKey_int32_x{
        buckets: make(map[uint64][]hashmap_CollisionKey_int32_x_entry),
        len: 0,
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
    len int32
}

func hashmap_new__HashMap_18Ref_12CollisionKey_6string() *hashmap_Ref_12CollisionKey_string_x {
    return &hashmap_Ref_12CollisionKey_string_x{
        buckets: make(map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry),
        len: 0,
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
    var retv70 bool
    var jp72 bool
    if true {
        var t73 float64 = self__0.value
        var t74 float64 = other__1.value
        var t75 bool = _goml_m_trait__impl_i_Eq_i_float64_i_eq(t73, t74)
        jp72 = t75
    } else {
        jp72 = false
    }
    retv70 = jp72
    return retv70
}

func _goml_m_trait__impl_i_Hash_i_FloatKey_i_hash(self__2 FloatKey) uint64 {
    var retv77 uint64
    var h__3 uint64 = 14695981039346656037
    var t78 uint64 = h__3 * 1099511628211
    var t79 float64 = self__2.value
    var t80 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(t79)
    var h__4 uint64 = t78 + t80
    retv77 = h__4
    return retv77
}

func _goml_m_trait__impl_i_Eq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var retv82 bool
    var t83 int32 = self__5.value
    var t84 int32 = other__6.value
    var t85 bool = t83 == t84
    retv82 = t85
    return retv82
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    var retv87 uint64
    retv87 = 1
    return retv87
}

func print_opt_string(value__8 Option__string) struct{} {
    switch value__8.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var x22 string = value__8.(Option__string_Some)._0
        var text__9 string = x22
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
        var x23 int32 = value__10.(Option__int32_Some)._0
        var number__11 int32 = x23
        println__T_int32(number__11)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func float_zero_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t97 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(zero32__12)
    var t98 uint64 = _goml_m_trait__impl_i_Hash_i_float32_i_hash(negative_zero32__13)
    var t99 bool = t97 == t98
    var t100 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t99)
    println__T_string(t100)
    var values32__14 *hashmap_float32_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, zero32__12, "f32")
    var t101 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, negative_zero32__13)
    print_opt_string(t101)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(values32__14, negative_zero32__13, "f32-updated")
    var t102 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t102)
    var t103 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(values32__14, zero32__12)
    print_opt_string(t103)
    _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(values32__14, negative_zero32__13)
    var t104 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(values32__14)
    println__T_int32(t104)
    var zero64__15 float64 = 0
    var negative_zero64__16 float64 = -zero64__15
    var t105 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(zero64__15)
    var t106 uint64 = _goml_m_trait__impl_i_Hash_i_float64_i_hash(negative_zero64__16)
    var t107 bool = t105 == t106
    var t108 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t107)
    println__T_string(t108)
    var values64__17 *hashmap_float64_string_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(values64__17, zero64__15, "f64")
    var t109 Option__string = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(values64__17, negative_zero64__16)
    print_opt_string(t109)
    var derived__18 *hashmap_FloatKey_string_x = _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string()
    var t110 FloatKey = FloatKey{
        value: zero64__15,
    }
    _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(derived__18, t110, "derived")
    var t111 FloatKey = FloatKey{
        value: negative_zero64__16,
    }
    var t112 Option__string = _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(derived__18, t111)
    print_opt_string(t112)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__19 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t114 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t114, 10)
    var t115 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t115, 20)
    var t116 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t116, 30)
    var t117 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t117)
    var t118 CollisionKey = CollisionKey{
        value: 1,
    }
    var t119 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t118)
    print_opt_int(t119)
    var t120 CollisionKey = CollisionKey{
        value: 2,
    }
    var t121 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t120)
    print_opt_int(t121)
    var t122 CollisionKey = CollisionKey{
        value: 3,
    }
    var t123 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t122)
    print_opt_int(t123)
    var t124 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t124, 40)
    var t125 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t125)
    var t126 CollisionKey = CollisionKey{
        value: 4,
    }
    var t127 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t126)
    print_opt_int(t127)
    var t128 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, t128, 41)
    var t129 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t129)
    var t130 CollisionKey = CollisionKey{
        value: 4,
    }
    var t131 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__19, t130)
    print_opt_int(t131)
    var t132 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t132)
    var t133 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, t133)
    var t134 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t134)
    var index__20 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop137:
    for {
        var t138 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
        var t139 bool = t138 < 2000
        if t139 {
            var t140 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t141 int32 = 1000 + t140
            var key__21 CollisionKey = CollisionKey{
                value: t141,
            }
            var t142 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__19, key__21, t142)
            _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__19, key__21)
            var t143 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__20)
            var t144 int32 = t143 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__20, t144)
            continue
        } else {
            break Loop_loop137
        }
    }
    var t136 int32 = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__19)
    println__T_int32(t136)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__22 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t146 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t146)
    var alias__24 *ref_CollisionKey_x = key__23
    var t147 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__25 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t147)
    _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(values__22, key__23, "identity")
    var t148 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, alias__24)
    println__T_bool(t148)
    var t149 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(key__23, equal_value__25)
    println__T_bool(t149)
    var t150 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key__23)
    var t151 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(alias__24)
    var t152 bool = t150 == t151
    println__T_bool(t152)
    var t153 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, alias__24)
    print_opt_string(t153)
    var t154 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, equal_value__25)
    print_opt_string(t154)
    var t155 CollisionKey = CollisionKey{
        value: 99,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(key__23, t155)
    var t156 Option__string = _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(values__22, key__23)
    print_opt_string(t156)
    return struct{}{}
}

func main0() struct{} {
    float_zero_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_float64_i_eq(self__47 float64, other__48 float64) bool {
    var retv159 bool
    var t160 bool = self__47 == other__48
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_Hash_i_float64_i_hash(self__62 float64) uint64 {
    var retv162 uint64
    var t163 uint64 = _goml_runtime_core_float64_hash(self__62)
    retv162 = t163
    return retv162
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_float32_i_hash(self__61 float32) uint64 {
    var retv171 uint64
    var t172 uint64 = _goml_runtime_core_float32_hash(self__61)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv174 string
    var t175 string = _goml_runtime_core_bool_to_string(self__8)
    retv174 = t175
    return retv174
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float32____V__string() *hashmap_float32_string_x {
    var retv177 *hashmap_float32_string_x
    var t178 *hashmap_float32_string_x = hashmap_new__HashMap_7float32_6string()
    retv177 = t178
    return retv177
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float32____V__string(self__129 *hashmap_float32_string_x, key__130 float32, value__131 string) struct{} {
    hashmap_set__HashMap_7float32_6string(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float32____V__string(self__127 *hashmap_float32_string_x, key__128 float32) Option__string {
    var retv182 Option__string
    var t183 Option__string = hashmap_get__HashMap_7float32_6string(self__127, key__128)
    retv182 = t183
    return retv182
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__float32____V__string(self__134 *hashmap_float32_string_x) int32 {
    var retv185 int32
    var t186 int32 = hashmap_len__HashMap_7float32_6string(self__134)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_HashMap_i_H_hb2a35d39f572745ebc511322f27baf02_32____V__string(self__132 *hashmap_float32_string_x, key__133 float32) struct{} {
    hashmap_remove__HashMap_7float32_6string(self__132, key__133)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__float64____V__string() *hashmap_float64_string_x {
    var retv190 *hashmap_float64_string_x
    var t191 *hashmap_float64_string_x = hashmap_new__HashMap_7float64_6string()
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__float64____V__string(self__129 *hashmap_float64_string_x, key__130 float64, value__131 string) struct{} {
    hashmap_set__HashMap_7float64_6string(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__float64____V__string(self__127 *hashmap_float64_string_x, key__128 float64) Option__string {
    var retv195 Option__string
    var t196 Option__string = hashmap_get__HashMap_7float64_6string(self__127, key__128)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_HashMap_i_H_h1c159dae3c6d467863c167e50033a837_ey____V__string() *hashmap_FloatKey_string_x {
    var retv198 *hashmap_FloatKey_string_x
    var t199 *hashmap_FloatKey_string_x = hashmap_new__HashMap_8FloatKey_6string()
    retv198 = t199
    return retv198
}

func _goml_m_inherent_i_HashMap_i_H_hf4091da363de58b5e0b9eb8325b8ab3b_ey____V__string(self__129 *hashmap_FloatKey_string_x, key__130 FloatKey, value__131 string) struct{} {
    hashmap_set__HashMap_8FloatKey_6string(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h76ebf5daded74fd7da365b7b6585281d_ey____V__string(self__127 *hashmap_FloatKey_string_x, key__128 FloatKey) Option__string {
    var retv203 Option__string
    var t204 Option__string = hashmap_get__HashMap_8FloatKey_6string(self__127, key__128)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var retv206 *hashmap_CollisionKey_int32_x
    var t207 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__129 *hashmap_CollisionKey_int32_x, key__130 CollisionKey, value__131 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__132 *hashmap_CollisionKey_int32_x, key__133 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__132, key__133)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__127 *hashmap_CollisionKey_int32_x, key__128 CollisionKey) Option__int32 {
    var retv213 Option__int32
    var t214 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__127, key__128)
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__134 *hashmap_CollisionKey_int32_x) int32 {
    var retv216 int32
    var t217 int32 = hashmap_len__HashMap_12CollisionKey_5int32(self__134)
    retv216 = t217
    return retv216
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv219 *ref_int32_x
    var t220 *ref_int32_x = ref__Ref_5int32(value__137)
    retv219 = t220
    return retv219
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv222 int32
    var t223 int32 = ref_get__Ref_5int32(self__138)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var retv227 *hashmap_Ref_12CollisionKey_string_x
    var t228 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__137 CollisionKey) *ref_CollisionKey_x {
    var retv230 *ref_CollisionKey_x
    var t231 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__137)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_HashMap_i_H_hf172370390478673f5488deef4d68ffa_r_____V__string(self__129 *hashmap_Ref_12CollisionKey_string_x, key__130 *ref_CollisionKey_x, value__131 string) struct{} {
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(self__129, key__130, value__131)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t235 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t235)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_CollisionKey_r__i_eq(self__63 *ref_CollisionKey_x, other__64 *ref_CollisionKey_x) bool {
    var retv238 bool
    var t239 bool = ptr_eq__Ref_12CollisionKey(self__63, other__64)
    retv238 = t239
    return retv238
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__65 *ref_CollisionKey_x) uint64 {
    var retv241 uint64
    var t242 uint64 = ptr_hash__Ref_12CollisionKey(self__65)
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_HashMap_i_H_h0ff2ed6bbeb05c2951b65091ecc43f4d_r_____V__string(self__127 *hashmap_Ref_12CollisionKey_string_x, key__128 *ref_CollisionKey_x) Option__string {
    var retv244 Option__string
    var t245 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(self__127, key__128)
    retv244 = t245
    return retv244
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__CollisionKey(self__139 *ref_CollisionKey_x, value__140 CollisionKey) struct{} {
    ref_set__Ref_12CollisionKey(self__139, value__140)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv249 string
    retv249 = self__9
    return retv249
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv251 string
    var t252 string = _goml_runtime_core_int32_to_string(self__13)
    retv251 = t252
    return retv251
}

func _goml_m_trait__impl_i_Eq_i_float32_i_eq(self__45 float32, other__46 float32) bool {
    var retv254 bool
    var t255 bool = self__45 == other__46
    retv254 = t255
    return retv254
}

func main() {
    main0()
}
