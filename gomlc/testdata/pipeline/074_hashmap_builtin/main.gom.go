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
        var x158 int32 = other__7.(B)._0
        switch self__6.(type) {
        case B:
            var x162 int32 = self__6.(B)._0
            var inline342 bool = x162 == x158
            return inline342
        default:
            return false
        }
    case P:
        var x159 Point = other__7.(P)._0
        switch self__6.(type) {
        case P:
            var x165 Point = self__6.(P)._0
            var inline345 bool
            var inline349 int32 = x165.x
            var inline350 int32 = x159.x
            var inline351 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline349, inline350)
            inline345 = inline351
            if inline345 {
                var inline346 int32 = x165.y
                var inline347 int32 = x159.y
                var inline348 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline346, inline347)
                return inline348
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
        var x166 int32 = self__12.(B)._0
        var h__15 uint64 = 14695981039346656037 + 2
        var t225 uint64 = h__15 * 1099511628211
        var t226 uint64
        var inline353 uint64 = _goml_runtime_core_int32_hash(x166)
        t226 = inline353
        var h__16 uint64 = t225 + t226
        return h__16
    case P:
        var x167 Point = self__12.(P)._0
        var h__18 uint64 = 14695981039346656037 + 3
        var t227 uint64 = h__18 * 1099511628211
        var t228 uint64
        var inline355 uint64 = 14695981039346656037
        var inline356 uint64 = inline355 * 1099511628211
        var inline357 int32 = x167.x
        var inline358 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline357)
        var inline359 uint64 = inline356 + inline358
        var inline360 uint64 = inline359 * 1099511628211
        var inline361 int32 = x167.y
        var inline362 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline361)
        var inline363 uint64 = inline360 + inline362
        t228 = inline363
        var h__19 uint64 = t227 + t228
        return h__19
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        var inline365 string = "none"
        var inline366 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline365)
        _goml_runtime_core_string_println(inline366)
        return struct{}{}
    case Some:
        var x168 int32 = x__20.(Some)._0
        var inline369 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x168)
        _goml_runtime_core_string_println(inline369)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t234 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t234, 20)
    var t235 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t235)
    var t236 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t236)
    var t237 Key = B{
        _0: 1,
    }
    var t238 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t237)
    println__T_bool(t238)
    var t239 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t239)
    var t240 Key = B{
        _0: 1,
    }
    var t241 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t240)
    println__T_bool(t241)
    var t242 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t242)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t243 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t243)
    var t244 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t244)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t245 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t245.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline408 int32 = t245.(Some)._0
        println__T_int32(inline408)
    default:
        panic("non-exhaustive match")
    }
    var t246 Option__int32
    var inline405 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t246 = inline405
    switch t246.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline401 int32 = t246.(Some)._0
        println__T_int32(inline401)
    default:
        panic("non-exhaustive match")
    }
    var t247 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t247)
    var t248 Option__int32
    var inline396 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t248 = inline396
    switch t248.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline392 int32 = t248.(Some)._0
        println__T_int32(inline392)
    default:
        panic("non-exhaustive match")
    }
    var t249 bool
    var inline389 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t249 = inline389
    var inline386 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t249)
    _goml_runtime_core_string_println(inline386)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline384 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline384
    var t250 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline382 *ref_Key_x = ref__Ref_3Key(t250)
    k1__27 = inline382
    var inline379 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline379)
    var t251 Option__int32
    var inline377 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t251 = inline377
    switch t251.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline373 int32 = t251.(Some)._0
        println__T_int32(inline373)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t254 bool = self__65 == other__66
    return t254
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__88 int32) uint64 {
    var t257 uint64 = _goml_runtime_core_int32_hash(self__88)
    return t257
}

func println__T_string(value__1 string) struct{} {
    var t259 string
    t259 = value__1
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t262 string
    var inline413 string = _goml_runtime_core_int32_to_string(value__1)
    t262 = inline413
    _goml_runtime_core_string_println(t262)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t266 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t266
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__198 *hashmap_Key_int32_x, key__199 Key, value__200 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t270 string
    var inline415 string = _goml_runtime_core_int_to_string(value__1)
    t270 = inline415
    _goml_runtime_core_string_println(t270)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__203 *hashmap_Key_int32_x) int {
    var t274 int = hashmap_len__HashMap_3Key_5int32(self__203)
    return t274
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__196 *hashmap_Key_int32_x, key__197 Key) Option__int32 {
    var t277 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__196, key__197)
    return t277
}

func println__T_bool(value__1 bool) struct{} {
    var t279 string
    var inline417 string = _goml_runtime_core_bool_to_string(value__1)
    t279 = inline417
    _goml_runtime_core_string_println(t279)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__204 *hashmap_Key_int32_x, key__205 Key) bool {
    var t283 bool = hashmap_contains__HashMap_3Key_5int32(self__204, key__205)
    return t283
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__201 *hashmap_Key_int32_x, key__202 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t288 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t288
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__207 Point) *ref_Point_x {
    var t291 *ref_Point_x = ref__Ref_5Point(value__207)
    return t291
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__198 *hashmap_Ref_5Point_int32_x, key__199 *ref_Point_x, value__200 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__196 *hashmap_Ref_5Point_int32_x, key__197 *ref_Point_x) Option__int32 {
    var t296 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__196, key__197)
    return t296
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__96 *ref_Point_x, other__97 *ref_Point_x) bool {
    var t301 bool = ptr_eq__Ref_5Point(self__96, other__97)
    return t301
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t317 string = _goml_runtime_core_int32_to_string(self__43)
    return t317
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t323 string = _goml_runtime_core_bool_to_string(self__37)
    return t323
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__98 *ref_Point_x) uint64 {
    var t326 uint64 = ptr_hash__Ref_5Point(self__98)
    return t326
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__96 *ref_Key_x, other__97 *ref_Key_x) bool {
    var t329 bool = ptr_eq__Ref_3Key(self__96, other__97)
    return t329
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__98 *ref_Key_x) uint64 {
    var t332 uint64 = ptr_hash__Ref_3Key(self__98)
    return t332
}

func main() {
    main0()
}
