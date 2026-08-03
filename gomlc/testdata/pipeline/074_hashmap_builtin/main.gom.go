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

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    switch other__7.(type) {
    case A:
        switch self__6.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x180 int32 = other__7.(B)._0
        switch self__6.(type) {
        case B:
            var x184 int32 = self__6.(B)._0
            var inline364 bool = x184 == x180
            return inline364
        default:
            return false
        }
    case P:
        var x181 Point = other__7.(P)._0
        switch self__6.(type) {
        case P:
            var x187 Point = self__6.(P)._0
            var inline367 bool
            var inline371 int32 = x187.x
            var inline372 int32 = x181.x
            var inline373 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline371, inline372)
            inline367 = inline373
            if inline367 {
                var inline368 int32 = x187.y
                var inline369 int32 = x181.y
                var inline370 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline368, inline369)
                return inline370
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

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        return h__13
    case B:
        var x188 int32 = self__12.(B)._0
        var h__15 uint64 = 14695981039346656037 + 2
        var t247 uint64 = h__15 * 1099511628211
        var t248 uint64
        var inline375 uint64 = _goml_runtime_core_int32_hash(x188)
        t248 = inline375
        var h__16 uint64 = t247 + t248
        return h__16
    case P:
        var x189 Point = self__12.(P)._0
        var h__18 uint64 = 14695981039346656037 + 3
        var t249 uint64 = h__18 * 1099511628211
        var t250 uint64
        var inline377 uint64 = 14695981039346656037
        var inline378 uint64 = inline377 * 1099511628211
        var inline379 int32 = x189.x
        var inline380 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline379)
        var inline381 uint64 = inline378 + inline380
        var inline382 uint64 = inline381 * 1099511628211
        var inline383 int32 = x189.y
        var inline384 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline383)
        var inline385 uint64 = inline382 + inline384
        t250 = inline385
        var h__19 uint64 = t249 + t250
        return h__19
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        var inline387 string = "none"
        var inline388 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline387)
        _goml_runtime_core_string_println(inline388)
        return struct{}{}
    case Some:
        var x190 int32 = x__20.(Some)._0
        var inline391 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x190)
        _goml_runtime_core_string_println(inline391)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t256 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t256, 20)
    var t257 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t257)
    var t258 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t258)
    var t259 Key = B{
        _0: 1,
    }
    var t260 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t259)
    println__T_bool(t260)
    var t261 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t261)
    var t262 Key = B{
        _0: 1,
    }
    var t263 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t262)
    println__T_bool(t263)
    var t264 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t264)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t265 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t265)
    var t266 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t266)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t267 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t267.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline430 int32 = t267.(Some)._0
        println__T_int32(inline430)
    default:
        panic("non-exhaustive match")
    }
    var t268 Option__int32
    var inline427 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t268 = inline427
    switch t268.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline423 int32 = t268.(Some)._0
        println__T_int32(inline423)
    default:
        panic("non-exhaustive match")
    }
    var t269 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t269)
    var t270 Option__int32
    var inline418 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t270 = inline418
    switch t270.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline414 int32 = t270.(Some)._0
        println__T_int32(inline414)
    default:
        panic("non-exhaustive match")
    }
    var t271 bool
    var inline411 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t271 = inline411
    var inline408 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t271)
    _goml_runtime_core_string_println(inline408)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline406 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline406
    var t272 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline404 *ref_Key_x = ref__Ref_3Key(t272)
    k1__27 = inline404
    var inline401 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline401)
    var t273 Option__int32
    var inline399 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t273 = inline399
    switch t273.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline395 int32 = t273.(Some)._0
        println__T_int32(inline395)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t276 bool = self__94 == other__95
    return t276
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__117 int32) uint64 {
    var t279 uint64 = _goml_runtime_core_int32_hash(self__117)
    return t279
}

func println__T_string(value__31 string) struct{} {
    var t281 string
    t281 = value__31
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t284 string
    var inline435 string = _goml_runtime_core_int32_to_string(value__31)
    t284 = inline435
    _goml_runtime_core_string_println(t284)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t288 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t288
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__227 *hashmap_Key_int32_x, key__228 Key, value__229 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__227, key__228, value__229)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t292 string
    var inline437 string = _goml_runtime_core_int_to_string(value__31)
    t292 = inline437
    _goml_runtime_core_string_println(t292)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__232 *hashmap_Key_int32_x) int {
    var t296 int = hashmap_len__HashMap_3Key_5int32(self__232)
    return t296
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__225 *hashmap_Key_int32_x, key__226 Key) Option__int32 {
    var t299 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__225, key__226)
    return t299
}

func println__T_bool(value__31 bool) struct{} {
    var t301 string
    var inline439 string = _goml_runtime_core_bool_to_string(value__31)
    t301 = inline439
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__233 *hashmap_Key_int32_x, key__234 Key) bool {
    var t305 bool = hashmap_contains__HashMap_3Key_5int32(self__233, key__234)
    return t305
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__230 *hashmap_Key_int32_x, key__231 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__230, key__231)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t310 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t310
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__236 Point) *ref_Point_x {
    var t313 *ref_Point_x = ref__Ref_5Point(value__236)
    return t313
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__227 *hashmap_Ref_5Point_int32_x, key__228 *ref_Point_x, value__229 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__227, key__228, value__229)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__225 *hashmap_Ref_5Point_int32_x, key__226 *ref_Point_x) Option__int32 {
    var t318 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__225, key__226)
    return t318
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__125 *ref_Point_x, other__126 *ref_Point_x) bool {
    var t323 bool = ptr_eq__Ref_5Point(self__125, other__126)
    return t323
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t339 string = _goml_runtime_core_int32_to_string(self__72)
    return t339
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t345 string = _goml_runtime_core_bool_to_string(self__66)
    return t345
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__127 *ref_Point_x) uint64 {
    var t348 uint64 = ptr_hash__Ref_5Point(self__127)
    return t348
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__125 *ref_Key_x, other__126 *ref_Key_x) bool {
    var t351 bool = ptr_eq__Ref_3Key(self__125, other__126)
    return t351
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__127 *ref_Key_x) uint64 {
    var t354 uint64 = ptr_hash__Ref_3Key(self__127)
    return t354
}

func main() {
    main0()
}
