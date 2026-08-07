package main

import (
    _goml_fmt "fmt"
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

func _goml_runtime_core_int32_hash(x int32) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_Point_x struct {
    value Point
}

func ref__Ref_5Point(value Point) *ref_Point_x {
    return &ref_Point_x{
        value: value,
    }
}

func ref_set__Ref_5Point(reference *ref_Point_x, value Point) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_5Point(a *ref_Point_x, b *ref_Point_x) bool {
    return a == b
}

func ptr_hash__Ref_5Point(reference *ref_Point_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type ref_Key_x struct {
    value Key
}

func ref__Ref_3Key(value Key) *ref_Key_x {
    return &ref_Key_x{
        value: value,
    }
}

func ptr_eq__Ref_3Key(a *ref_Key_x, b *ref_Key_x) bool {
    return a == b
}

func ptr_hash__Ref_3Key(reference *ref_Key_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type hashmap_Key_int32_x_entry struct {
    active bool
    key Key
    value int32
}

type hashmap_Key_int32_x struct {
    buckets map[uint64][]hashmap_Key_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_3Key_5int32(m *hashmap_Key_int32_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_3Key_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            var zero hashmap_Key_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_contains__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) bool {
    if m == nil {
        return false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
}

type hashmap_Ref_5Point_int32_x_entry struct {
    active bool
    key *ref_Point_x
    value int32
}

type hashmap_Ref_5Point_int32_x struct {
    buckets map[uint64][]hashmap_Ref_5Point_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_10Ref_5Point_5int32() *hashmap_Ref_5Point_int32_x {
    return &hashmap_Ref_5Point_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_5Point_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_10Ref_5Point_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_5Point_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_5Point_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_Ref_3Key_int32_x_entry struct {
    active bool
    key *ref_Key_x
    value int32
}

type hashmap_Ref_3Key_int32_x struct {
    buckets map[uint64][]hashmap_Ref_3Key_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_8Ref_3Key_5int32() *hashmap_Ref_3Key_int32_x {
    return &hashmap_Ref_3Key_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_3Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_8Ref_3Key_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_3Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_3Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3Key_3Key struct {
    _0 Key
    _1 Key
}

type Point struct {
    x int32
    y int32
}

type Key interface {
    isKey()
}

type A struct {}

func (_ A) isKey() {}

type B struct {
    _0 int32
}

func (_ B) isKey() {}

type P struct {
    _0 Point
}

func (_ P) isKey() {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__6 Key) uint64 {
    switch self__6.(type) {
    case A:
        var t195_source int = 0
        var t195 uint64 = uint64(int(t195_source))
        var t196 uint64 = t195 + 14695981039346656037
        var h__7 uint64 = t196 + 1
        return h__7
    case B:
        var x136 int32 = self__6.(B)._0
        var t197_source int = 0
        var t197 uint64 = uint64(int(t197_source))
        var t198 uint64 = t197 + 14695981039346656037
        var h__9 uint64 = t198 + 2
        var t199_source int = 0
        var t199 uint64 = uint64(int(t199_source))
        var t200 uint64 = t199 + 1099511628211
        var t201 uint64 = h__9 * t200
        var t202 uint64
        var inline338 uint64 = _goml_runtime_core_int32_hash(x136)
        t202 = inline338
        var h__10 uint64 = t201 + t202
        return h__10
    case P:
        var x137 Point = self__6.(P)._0
        var t203_source int = 0
        var t203 uint64 = uint64(int(t203_source))
        var t204 uint64 = t203 + 14695981039346656037
        var h__12 uint64 = t204 + 3
        var t205_source int = 0
        var t205 uint64 = uint64(int(t205_source))
        var t206 uint64 = t205 + 1099511628211
        var t207 uint64 = h__12 * t206
        var t208 uint64
        var inline340_source int = 0
        var inline340 uint64 = uint64(int(inline340_source))
        var inline341 uint64 = inline340 + 14695981039346656037
        var inline342_source int = 0
        var inline342 uint64 = uint64(int(inline342_source))
        var inline343 uint64 = inline342 + 1099511628211
        var inline344 uint64 = inline341 * inline343
        var inline345 int32 = x137.x
        var inline346 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline345)
        var inline347 uint64 = inline344 + inline346
        var inline348_source int = 0
        var inline348 uint64 = uint64(int(inline348_source))
        var inline349 uint64 = inline348 + 1099511628211
        var inline350 uint64 = inline347 * inline349
        var inline351 int32 = x137.y
        var inline352 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline351)
        var inline353 uint64 = inline350 + inline352
        t208 = inline353
        var h__13 uint64 = t207 + t208
        return h__13
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__14 Key, other__15 Key) bool {
    switch other__15.(type) {
    case A:
        switch self__14.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x141 int32 = other__15.(B)._0
        switch self__14.(type) {
        case B:
            var x145 int32 = self__14.(B)._0
            var inline355 bool = x145 == x141
            return inline355
        default:
            return false
        }
    case P:
        var x142 Point = other__15.(P)._0
        switch self__14.(type) {
        case P:
            var x148 Point = self__14.(P)._0
            var inline358 bool
            var inline362 int32 = x148.x
            var inline363 int32 = x142.x
            var inline364 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline362, inline363)
            inline358 = inline364
            if inline358 {
                var inline359 int32 = x148.y
                var inline360 int32 = x142.y
                var inline361 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline359, inline360)
                return inline361
            } else {
                return false
            }
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        var inline366 string = "none"
        var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline366)
        _goml_runtime_core_string_println(inline367)
        return struct{}{}
    case Some:
        var x149 int32 = x__20.(Some)._0
        var inline370 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x149)
        _goml_runtime_core_string_println(inline370)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t230 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t230, 20)
    var t231 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t231)
    var t232 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t232)
    var t233 Key = B{
        _0: 1,
    }
    var t234 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t233)
    println__T_bool(t234)
    var t235 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t235)
    var t236 Key = B{
        _0: 1,
    }
    var t237 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t236)
    println__T_bool(t237)
    var t238 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t238)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t239 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t239)
    var t240 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t240)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t241 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t241.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline409 int32 = t241.(Some)._0
        println__T_int32(inline409)
    default:
        panic("non-exhaustive match")
    }
    var t242 Option__int32
    var inline406 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t242 = inline406
    switch t242.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline402 int32 = t242.(Some)._0
        println__T_int32(inline402)
    default:
        panic("non-exhaustive match")
    }
    var t243 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t243)
    var t244 Option__int32
    var inline397 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t244 = inline397
    switch t244.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline393 int32 = t244.(Some)._0
        println__T_int32(inline393)
    default:
        panic("non-exhaustive match")
    }
    var t245 bool
    var inline390 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t245 = inline390
    var inline387 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t245)
    _goml_runtime_core_string_println(inline387)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline385 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline385
    var t246 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline383 *ref_Key_x = ref__Ref_3Key(t246)
    k1__27 = inline383
    var inline380 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline380)
    var t247 Option__int32
    var inline378 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t247 = inline378
    switch t247.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline374 int32 = t247.(Some)._0
        println__T_int32(inline374)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__132 int32) uint64 {
    var t250 uint64 = _goml_runtime_core_int32_hash(self__132)
    return t250
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t253 bool = self__109 == other__110
    return t253
}

func println__T_string(value__31 string) struct{} {
    var t255 string
    t255 = value__31
    _goml_runtime_core_string_println(t255)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t258 string
    var inline414 string = _goml_runtime_core_int32_to_string(value__31)
    t258 = inline414
    _goml_runtime_core_string_println(t258)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t262 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t262
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__223 *hashmap_Key_int32_x, key__224 Key, value__225 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__223, key__224, value__225)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t266 string
    var inline416 string = _goml_runtime_core_int_to_string(value__31)
    t266 = inline416
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__228 *hashmap_Key_int32_x) int {
    var t270 int = hashmap_len__HashMap_3Key_5int32(self__228)
    return t270
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__221 *hashmap_Key_int32_x, key__222 Key) Option__int32 {
    var t273 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__221, key__222)
    return t273
}

func println__T_bool(value__31 bool) struct{} {
    var t275 string
    var inline418 string = _goml_runtime_core_bool_to_string(value__31)
    t275 = inline418
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__229 *hashmap_Key_int32_x, key__230 Key) bool {
    var t279 bool = hashmap_contains__HashMap_3Key_5int32(self__229, key__230)
    return t279
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__226 *hashmap_Key_int32_x, key__227 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__226, key__227)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t284 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t284
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__232 Point) *ref_Point_x {
    var t287 *ref_Point_x = ref__Ref_5Point(value__232)
    return t287
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__223 *hashmap_Ref_5Point_int32_x, key__224 *ref_Point_x, value__225 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__223, key__224, value__225)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__221 *hashmap_Ref_5Point_int32_x, key__222 *ref_Point_x) Option__int32 {
    var t292 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__221, key__222)
    return t292
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__140 *ref_Point_x, other__141 *ref_Point_x) bool {
    var t297 bool = ptr_eq__Ref_5Point(self__140, other__141)
    return t297
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t313 string = _goml_runtime_core_int32_to_string(self__72)
    return t313
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t319 string = _goml_runtime_core_bool_to_string(self__66)
    return t319
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__142 *ref_Point_x) uint64 {
    var t322 uint64 = ptr_hash__Ref_5Point(self__142)
    return t322
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__140 *ref_Key_x, other__141 *ref_Key_x) bool {
    var t325 bool = ptr_eq__Ref_3Key(self__140, other__141)
    return t325
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__142 *ref_Key_x) uint64 {
    var t328 uint64 = ptr_hash__Ref_3Key(self__142)
    return t328
}

func main() {
    main0()
}
