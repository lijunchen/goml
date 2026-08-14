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
        var t246_source int = 0
        var t246 uint64 = uint64(int(t246_source))
        var t247 uint64 = t246 + 14695981039346656037
        var h__7 uint64 = t247 + 1
        return h__7
    case B:
        var x187 int32 = self__6.(B)._0
        var t248_source int = 0
        var t248 uint64 = uint64(int(t248_source))
        var t249 uint64 = t248 + 14695981039346656037
        var h__9 uint64 = t249 + 2
        var t250_source int = 0
        var t250 uint64 = uint64(int(t250_source))
        var t251 uint64 = t250 + 1099511628211
        var t252 uint64 = h__9 * t251
        var t253 uint64
        var inline389 uint64 = _goml_runtime_core_int32_hash(x187)
        t253 = inline389
        var h__10 uint64 = t252 + t253
        return h__10
    case P:
        var x188 Point = self__6.(P)._0
        var t254_source int = 0
        var t254 uint64 = uint64(int(t254_source))
        var t255 uint64 = t254 + 14695981039346656037
        var h__12 uint64 = t255 + 3
        var t256_source int = 0
        var t256 uint64 = uint64(int(t256_source))
        var t257 uint64 = t256 + 1099511628211
        var t258 uint64 = h__12 * t257
        var t259 uint64
        var inline391_source int = 0
        var inline391 uint64 = uint64(int(inline391_source))
        var inline392 uint64 = inline391 + 14695981039346656037
        var inline393_source int = 0
        var inline393 uint64 = uint64(int(inline393_source))
        var inline394 uint64 = inline393 + 1099511628211
        var inline395 uint64 = inline392 * inline394
        var inline396 int32 = x188.x
        var inline397 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline396)
        var inline398 uint64 = inline395 + inline397
        var inline399_source int = 0
        var inline399 uint64 = uint64(int(inline399_source))
        var inline400 uint64 = inline399 + 1099511628211
        var inline401 uint64 = inline398 * inline400
        var inline402 int32 = x188.y
        var inline403 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline402)
        var inline404 uint64 = inline401 + inline403
        t259 = inline404
        var h__13 uint64 = t258 + t259
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
        var x192 int32 = other__15.(B)._0
        switch self__14.(type) {
        case B:
            var x196 int32 = self__14.(B)._0
            var inline406 bool = x196 == x192
            return inline406
        default:
            return false
        }
    case P:
        var x193 Point = other__15.(P)._0
        switch self__14.(type) {
        case P:
            var x199 Point = self__14.(P)._0
            var inline409 bool
            var inline413 int32 = x199.x
            var inline414 int32 = x193.x
            var inline415 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline413, inline414)
            inline409 = inline415
            if inline409 {
                var inline410 int32 = x199.y
                var inline411 int32 = x193.y
                var inline412 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline410, inline411)
                return inline412
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
        var inline417 string = "none"
        var inline418 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline417)
        _goml_runtime_core_string_println(inline418)
        return struct{}{}
    case Some:
        var x200 int32 = x__20.(Some)._0
        var inline421 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x200)
        _goml_runtime_core_string_println(inline421)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t281 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t281, 20)
    var t282 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t282)
    var t283 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t283)
    var t284 Key = B{
        _0: 1,
    }
    var t285 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t284)
    println__T_bool(t285)
    var t286 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t286)
    var t287 Key = B{
        _0: 1,
    }
    var t288 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t287)
    println__T_bool(t288)
    var t289 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t289)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t290 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t290)
    var t291 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t291)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t292 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t292.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline460 int32 = t292.(Some)._0
        println__T_int32(inline460)
    default:
        panic("non-exhaustive match")
    }
    var t293 Option__int32
    var inline457 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t293 = inline457
    switch t293.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline453 int32 = t293.(Some)._0
        println__T_int32(inline453)
    default:
        panic("non-exhaustive match")
    }
    var t294 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t294)
    var t295 Option__int32
    var inline448 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t295 = inline448
    switch t295.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline444 int32 = t295.(Some)._0
        println__T_int32(inline444)
    default:
        panic("non-exhaustive match")
    }
    var t296 bool
    var inline441 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t296 = inline441
    var inline438 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t296)
    _goml_runtime_core_string_println(inline438)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline436 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline436
    var t297 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline434 *ref_Key_x = ref__Ref_3Key(t297)
    k1__27 = inline434
    var inline431 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline431)
    var t298 Option__int32
    var inline429 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t298 = inline429
    switch t298.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline425 int32 = t298.(Some)._0
        println__T_int32(inline425)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__130 int32) uint64 {
    var t301 uint64 = _goml_runtime_core_int32_hash(self__130)
    return t301
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__107 int32, other__108 int32) bool {
    var t304 bool = self__107 == other__108
    return t304
}

func println__T_string(value__1 string) struct{} {
    var t306 string
    t306 = value__1
    _goml_runtime_core_string_println(t306)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t309 string
    var inline465 string = _goml_runtime_core_int32_to_string(value__1)
    t309 = inline465
    _goml_runtime_core_string_println(t309)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t313 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t313
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__264 *hashmap_Key_int32_x, key__265 Key, value__266 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__264, key__265, value__266)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t317 string
    var inline467 string = _goml_runtime_core_int_to_string(value__1)
    t317 = inline467
    _goml_runtime_core_string_println(t317)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__269 *hashmap_Key_int32_x) int {
    var t321 int = hashmap_len__HashMap_3Key_5int32(self__269)
    return t321
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__262 *hashmap_Key_int32_x, key__263 Key) Option__int32 {
    var t324 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__262, key__263)
    return t324
}

func println__T_bool(value__1 bool) struct{} {
    var t326 string
    var inline469 string = _goml_runtime_core_bool_to_string(value__1)
    t326 = inline469
    _goml_runtime_core_string_println(t326)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__270 *hashmap_Key_int32_x, key__271 Key) bool {
    var t330 bool = hashmap_contains__HashMap_3Key_5int32(self__270, key__271)
    return t330
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__267 *hashmap_Key_int32_x, key__268 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__267, key__268)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t335 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t335
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__273 Point) *ref_Point_x {
    var t338 *ref_Point_x = ref__Ref_5Point(value__273)
    return t338
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__264 *hashmap_Ref_5Point_int32_x, key__265 *ref_Point_x, value__266 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__264, key__265, value__266)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__262 *hashmap_Ref_5Point_int32_x, key__263 *ref_Point_x) Option__int32 {
    var t343 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__262, key__263)
    return t343
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(self__136 *ref_Point_x, other__137 *ref_Point_x) bool {
    var t348 bool = ptr_eq__Ref_5Point(self__136, other__137)
    return t348
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t364 string = _goml_runtime_core_int32_to_string(self__70)
    return t364
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t370 string = _goml_runtime_core_bool_to_string(self__64)
    return t370
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__138 *ref_Point_x) uint64 {
    var t373 uint64 = ptr_hash__Ref_5Point(self__138)
    return t373
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(self__136 *ref_Key_x, other__137 *ref_Key_x) bool {
    var t376 bool = ptr_eq__Ref_3Key(self__136, other__137)
    return t376
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__138 *ref_Key_x) uint64 {
    var t379 uint64 = ptr_hash__Ref_3Key(self__138)
    return t379
}

func main() {
    main0()
}
