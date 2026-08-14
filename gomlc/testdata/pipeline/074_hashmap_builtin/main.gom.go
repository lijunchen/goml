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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(entry.key, key) {
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
        var t241_source int = 0
        var t241 uint64 = uint64(int(t241_source))
        var t242 uint64 = t241 + 14695981039346656037
        var h__7 uint64 = t242 + 1
        return h__7
    case B:
        var x182 int32 = self__6.(B)._0
        var t243_source int = 0
        var t243 uint64 = uint64(int(t243_source))
        var t244 uint64 = t243 + 14695981039346656037
        var h__9 uint64 = t244 + 2
        var t245_source int = 0
        var t245 uint64 = uint64(int(t245_source))
        var t246 uint64 = t245 + 1099511628211
        var t247 uint64 = h__9 * t246
        var t248 uint64
        var inline384 uint64 = _goml_runtime_core_int32_hash(x182)
        t248 = inline384
        var h__10 uint64 = t247 + t248
        return h__10
    case P:
        var x183 Point = self__6.(P)._0
        var t249_source int = 0
        var t249 uint64 = uint64(int(t249_source))
        var t250 uint64 = t249 + 14695981039346656037
        var h__12 uint64 = t250 + 3
        var t251_source int = 0
        var t251 uint64 = uint64(int(t251_source))
        var t252 uint64 = t251 + 1099511628211
        var t253 uint64 = h__12 * t252
        var t254 uint64
        var inline386_source int = 0
        var inline386 uint64 = uint64(int(inline386_source))
        var inline387 uint64 = inline386 + 14695981039346656037
        var inline388_source int = 0
        var inline388 uint64 = uint64(int(inline388_source))
        var inline389 uint64 = inline388 + 1099511628211
        var inline390 uint64 = inline387 * inline389
        var inline391 int32 = x183.x
        var inline392 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline391)
        var inline393 uint64 = inline390 + inline392
        var inline394_source int = 0
        var inline394 uint64 = uint64(int(inline394_source))
        var inline395 uint64 = inline394 + 1099511628211
        var inline396 uint64 = inline393 * inline395
        var inline397 int32 = x183.y
        var inline398 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline397)
        var inline399 uint64 = inline396 + inline398
        t254 = inline399
        var h__13 uint64 = t253 + t254
        return h__13
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__14 Key, other__15 Key) bool {
    switch other__15.(type) {
    case A:
        switch self__14.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x187 int32 = other__15.(B)._0
        switch self__14.(type) {
        case B:
            var x191 int32 = self__14.(B)._0
            var inline401 bool = x191 == x187
            return inline401
        default:
            return false
        }
    case P:
        var x188 Point = other__15.(P)._0
        switch self__14.(type) {
        case P:
            var x194 Point = self__14.(P)._0
            var inline404 bool
            var inline408 int32 = x194.x
            var inline409 int32 = x188.x
            var inline410 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline408, inline409)
            inline404 = inline410
            if inline404 {
                var inline405 int32 = x194.y
                var inline406 int32 = x188.y
                var inline407 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline405, inline406)
                return inline407
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
        var inline412 string = "none"
        var inline413 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline412)
        _goml_runtime_core_string_println(inline413)
        return struct{}{}
    case Some:
        var x195 int32 = x__20.(Some)._0
        var inline416 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x195)
        _goml_runtime_core_string_println(inline416)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t276 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t276, 20)
    var t277 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t277)
    var t278 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t278)
    var t279 Key = B{
        _0: 1,
    }
    var t280 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t279)
    println__T_bool(t280)
    var t281 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t281)
    var t282 Key = B{
        _0: 1,
    }
    var t283 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t282)
    println__T_bool(t283)
    var t284 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t284)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t285 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t285)
    var t286 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t286)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t287 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t287.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline455 int32 = t287.(Some)._0
        println__T_int32(inline455)
    default:
        panic("non-exhaustive match")
    }
    var t288 Option__int32
    var inline452 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t288 = inline452
    switch t288.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline448 int32 = t288.(Some)._0
        println__T_int32(inline448)
    default:
        panic("non-exhaustive match")
    }
    var t289 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t289)
    var t290 Option__int32
    var inline443 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t290 = inline443
    switch t290.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline439 int32 = t290.(Some)._0
        println__T_int32(inline439)
    default:
        panic("non-exhaustive match")
    }
    var t291 bool
    var inline436 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t291 = inline436
    var inline433 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t291)
    _goml_runtime_core_string_println(inline433)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline431 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline431
    var t292 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline429 *ref_Key_x = ref__Ref_3Key(t292)
    k1__27 = inline429
    var inline426 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline426)
    var t293 Option__int32
    var inline424 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t293 = inline424
    switch t293.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline420 int32 = t293.(Some)._0
        println__T_int32(inline420)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__130 int32) uint64 {
    var t296 uint64 = _goml_runtime_core_int32_hash(self__130)
    return t296
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__107 int32, other__108 int32) bool {
    var t299 bool = self__107 == other__108
    return t299
}

func println__T_string(value__1 string) struct{} {
    var t301 string
    t301 = value__1
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t304 string
    var inline460 string = _goml_runtime_core_int32_to_string(value__1)
    t304 = inline460
    _goml_runtime_core_string_println(t304)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t308 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t308
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__261 *hashmap_Key_int32_x, key__262 Key, value__263 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__261, key__262, value__263)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t312 string
    var inline462 string = _goml_runtime_core_int_to_string(value__1)
    t312 = inline462
    _goml_runtime_core_string_println(t312)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__266 *hashmap_Key_int32_x) int {
    var t316 int = hashmap_len__HashMap_3Key_5int32(self__266)
    return t316
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__259 *hashmap_Key_int32_x, key__260 Key) Option__int32 {
    var t319 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__259, key__260)
    return t319
}

func println__T_bool(value__1 bool) struct{} {
    var t321 string
    var inline464 string = _goml_runtime_core_bool_to_string(value__1)
    t321 = inline464
    _goml_runtime_core_string_println(t321)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__267 *hashmap_Key_int32_x, key__268 Key) bool {
    var t325 bool = hashmap_contains__HashMap_3Key_5int32(self__267, key__268)
    return t325
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__264 *hashmap_Key_int32_x, key__265 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__264, key__265)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t330 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t330
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__270 Point) *ref_Point_x {
    var t333 *ref_Point_x = ref__Ref_5Point(value__270)
    return t333
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__261 *hashmap_Ref_5Point_int32_x, key__262 *ref_Point_x, value__263 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__261, key__262, value__263)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__259 *hashmap_Ref_5Point_int32_x, key__260 *ref_Point_x) Option__int32 {
    var t338 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__259, key__260)
    return t338
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(self__136 *ref_Point_x, other__137 *ref_Point_x) bool {
    var t343 bool = ptr_eq__Ref_5Point(self__136, other__137)
    return t343
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t359 string = _goml_runtime_core_int32_to_string(self__70)
    return t359
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t365 string = _goml_runtime_core_bool_to_string(self__64)
    return t365
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__138 *ref_Point_x) uint64 {
    var t368 uint64 = ptr_hash__Ref_5Point(self__138)
    return t368
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(self__136 *ref_Key_x, other__137 *ref_Key_x) bool {
    var t371 bool = ptr_eq__Ref_3Key(self__136, other__137)
    return t371
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__138 *ref_Key_x) uint64 {
    var t374 uint64 = ptr_hash__Ref_3Key(self__138)
    return t374
}

func main() {
    main0()
}
