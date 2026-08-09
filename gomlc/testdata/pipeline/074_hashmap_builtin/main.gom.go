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
        var t231_source int = 0
        var t231 uint64 = uint64(int(t231_source))
        var t232 uint64 = t231 + 14695981039346656037
        var h__7 uint64 = t232 + 1
        return h__7
    case B:
        var x172 int32 = self__6.(B)._0
        var t233_source int = 0
        var t233 uint64 = uint64(int(t233_source))
        var t234 uint64 = t233 + 14695981039346656037
        var h__9 uint64 = t234 + 2
        var t235_source int = 0
        var t235 uint64 = uint64(int(t235_source))
        var t236 uint64 = t235 + 1099511628211
        var t237 uint64 = h__9 * t236
        var t238 uint64
        var inline374 uint64 = _goml_runtime_core_int32_hash(x172)
        t238 = inline374
        var h__10 uint64 = t237 + t238
        return h__10
    case P:
        var x173 Point = self__6.(P)._0
        var t239_source int = 0
        var t239 uint64 = uint64(int(t239_source))
        var t240 uint64 = t239 + 14695981039346656037
        var h__12 uint64 = t240 + 3
        var t241_source int = 0
        var t241 uint64 = uint64(int(t241_source))
        var t242 uint64 = t241 + 1099511628211
        var t243 uint64 = h__12 * t242
        var t244 uint64
        var inline376_source int = 0
        var inline376 uint64 = uint64(int(inline376_source))
        var inline377 uint64 = inline376 + 14695981039346656037
        var inline378_source int = 0
        var inline378 uint64 = uint64(int(inline378_source))
        var inline379 uint64 = inline378 + 1099511628211
        var inline380 uint64 = inline377 * inline379
        var inline381 int32 = x173.x
        var inline382 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline381)
        var inline383 uint64 = inline380 + inline382
        var inline384_source int = 0
        var inline384 uint64 = uint64(int(inline384_source))
        var inline385 uint64 = inline384 + 1099511628211
        var inline386 uint64 = inline383 * inline385
        var inline387 int32 = x173.y
        var inline388 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline387)
        var inline389 uint64 = inline386 + inline388
        t244 = inline389
        var h__13 uint64 = t243 + t244
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
        var x177 int32 = other__15.(B)._0
        switch self__14.(type) {
        case B:
            var x181 int32 = self__14.(B)._0
            var inline391 bool = x181 == x177
            return inline391
        default:
            return false
        }
    case P:
        var x178 Point = other__15.(P)._0
        switch self__14.(type) {
        case P:
            var x184 Point = self__14.(P)._0
            var inline394 bool
            var inline398 int32 = x184.x
            var inline399 int32 = x178.x
            var inline400 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline398, inline399)
            inline394 = inline400
            if inline394 {
                var inline395 int32 = x184.y
                var inline396 int32 = x178.y
                var inline397 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline395, inline396)
                return inline397
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
        var inline402 string = "none"
        var inline403 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline402)
        _goml_runtime_core_string_println(inline403)
        return struct{}{}
    case Some:
        var x185 int32 = x__20.(Some)._0
        var inline406 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x185)
        _goml_runtime_core_string_println(inline406)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t266 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t266, 20)
    var t267 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t267)
    var t268 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t268)
    var t269 Key = B{
        _0: 1,
    }
    var t270 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t269)
    println__T_bool(t270)
    var t271 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t271)
    var t272 Key = B{
        _0: 1,
    }
    var t273 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t272)
    println__T_bool(t273)
    var t274 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t274)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t275 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t275)
    var t276 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t276)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t277 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t277.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline445 int32 = t277.(Some)._0
        println__T_int32(inline445)
    default:
        panic("non-exhaustive match")
    }
    var t278 Option__int32
    var inline442 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t278 = inline442
    switch t278.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline438 int32 = t278.(Some)._0
        println__T_int32(inline438)
    default:
        panic("non-exhaustive match")
    }
    var t279 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t279)
    var t280 Option__int32
    var inline433 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t280 = inline433
    switch t280.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline429 int32 = t280.(Some)._0
        println__T_int32(inline429)
    default:
        panic("non-exhaustive match")
    }
    var t281 bool
    var inline426 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t281 = inline426
    var inline423 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t281)
    _goml_runtime_core_string_println(inline423)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline421 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline421
    var t282 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline419 *ref_Key_x = ref__Ref_3Key(t282)
    k1__27 = inline419
    var inline416 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline416)
    var t283 Option__int32
    var inline414 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t283 = inline414
    switch t283.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline410 int32 = t283.(Some)._0
        println__T_int32(inline410)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__132 int32) uint64 {
    var t286 uint64 = _goml_runtime_core_int32_hash(self__132)
    return t286
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t289 bool = self__109 == other__110
    return t289
}

func println__T_string(value__31 string) struct{} {
    var t291 string
    t291 = value__31
    _goml_runtime_core_string_println(t291)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t294 string
    var inline450 string = _goml_runtime_core_int32_to_string(value__31)
    t294 = inline450
    _goml_runtime_core_string_println(t294)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t298 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t298
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__248 *hashmap_Key_int32_x, key__249 Key, value__250 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__248, key__249, value__250)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t302 string
    var inline452 string = _goml_runtime_core_int_to_string(value__31)
    t302 = inline452
    _goml_runtime_core_string_println(t302)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__253 *hashmap_Key_int32_x) int {
    var t306 int = hashmap_len__HashMap_3Key_5int32(self__253)
    return t306
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__246 *hashmap_Key_int32_x, key__247 Key) Option__int32 {
    var t309 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__246, key__247)
    return t309
}

func println__T_bool(value__31 bool) struct{} {
    var t311 string
    var inline454 string = _goml_runtime_core_bool_to_string(value__31)
    t311 = inline454
    _goml_runtime_core_string_println(t311)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__254 *hashmap_Key_int32_x, key__255 Key) bool {
    var t315 bool = hashmap_contains__HashMap_3Key_5int32(self__254, key__255)
    return t315
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__251 *hashmap_Key_int32_x, key__252 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__251, key__252)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t320 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t320
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__257 Point) *ref_Point_x {
    var t323 *ref_Point_x = ref__Ref_5Point(value__257)
    return t323
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__248 *hashmap_Ref_5Point_int32_x, key__249 *ref_Point_x, value__250 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__246 *hashmap_Ref_5Point_int32_x, key__247 *ref_Point_x) Option__int32 {
    var t328 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__246, key__247)
    return t328
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(self__138 *ref_Point_x, other__139 *ref_Point_x) bool {
    var t333 bool = ptr_eq__Ref_5Point(self__138, other__139)
    return t333
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t349 string = _goml_runtime_core_int32_to_string(self__72)
    return t349
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t355 string = _goml_runtime_core_bool_to_string(self__66)
    return t355
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__140 *ref_Point_x) uint64 {
    var t358 uint64 = ptr_hash__Ref_5Point(self__140)
    return t358
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(self__138 *ref_Key_x, other__139 *ref_Key_x) bool {
    var t361 bool = ptr_eq__Ref_3Key(self__138, other__139)
    return t361
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__140 *ref_Key_x) uint64 {
    var t364 uint64 = ptr_hash__Ref_3Key(self__140)
    return t364
}

func main() {
    main0()
}
