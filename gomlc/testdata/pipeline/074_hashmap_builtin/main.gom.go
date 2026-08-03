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
        var x139 int32 = other__7.(B)._0
        switch self__6.(type) {
        case B:
            var x143 int32 = self__6.(B)._0
            var inline323 bool = x143 == x139
            return inline323
        default:
            return false
        }
    case P:
        var x140 Point = other__7.(P)._0
        switch self__6.(type) {
        case P:
            var x146 Point = self__6.(P)._0
            var inline326 bool
            var inline330 int32 = x146.x
            var inline331 int32 = x140.x
            var inline332 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline330, inline331)
            inline326 = inline332
            if inline326 {
                var inline327 int32 = x146.y
                var inline328 int32 = x140.y
                var inline329 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline327, inline328)
                return inline329
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
        var x147 int32 = self__12.(B)._0
        var h__15 uint64 = 14695981039346656037 + 2
        var t206 uint64 = h__15 * 1099511628211
        var t207 uint64
        var inline334 uint64 = _goml_runtime_core_int32_hash(x147)
        t207 = inline334
        var h__16 uint64 = t206 + t207
        return h__16
    case P:
        var x148 Point = self__12.(P)._0
        var h__18 uint64 = 14695981039346656037 + 3
        var t208 uint64 = h__18 * 1099511628211
        var t209 uint64
        var inline336 uint64 = 14695981039346656037
        var inline337 uint64 = inline336 * 1099511628211
        var inline338 int32 = x148.x
        var inline339 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline338)
        var inline340 uint64 = inline337 + inline339
        var inline341 uint64 = inline340 * 1099511628211
        var inline342 int32 = x148.y
        var inline343 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline342)
        var inline344 uint64 = inline341 + inline343
        t209 = inline344
        var h__19 uint64 = t208 + t209
        return h__19
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        var inline346 string = "none"
        var inline347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline346)
        _goml_runtime_core_string_println(inline347)
        return struct{}{}
    case Some:
        var x149 int32 = x__20.(Some)._0
        var inline350 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x149)
        _goml_runtime_core_string_println(inline350)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t215 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t215, 20)
    var t216 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t216)
    var t217 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t217)
    var t218 Key = B{
        _0: 1,
    }
    var t219 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t218)
    println__T_bool(t219)
    var t220 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t220)
    var t221 Key = B{
        _0: 1,
    }
    var t222 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t221)
    println__T_bool(t222)
    var t223 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t223)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t224 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t224)
    var t225 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t225)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t226 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t226.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline389 int32 = t226.(Some)._0
        println__T_int32(inline389)
    default:
        panic("non-exhaustive match")
    }
    var t227 Option__int32
    var inline386 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t227 = inline386
    switch t227.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline382 int32 = t227.(Some)._0
        println__T_int32(inline382)
    default:
        panic("non-exhaustive match")
    }
    var t228 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t228)
    var t229 Option__int32
    var inline377 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t229 = inline377
    switch t229.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline373 int32 = t229.(Some)._0
        println__T_int32(inline373)
    default:
        panic("non-exhaustive match")
    }
    var t230 bool
    var inline370 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t230 = inline370
    var inline367 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t230)
    _goml_runtime_core_string_println(inline367)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline365 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline365
    var t231 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x
    var inline363 *ref_Key_x = ref__Ref_3Key(t231)
    k1__27 = inline363
    var inline360 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline360)
    var t232 Option__int32
    var inline358 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t232 = inline358
    switch t232.(type) {
    case None:
        println__T_string("none")
        return struct{}{}
    case Some:
        var inline354 int32 = t232.(Some)._0
        println__T_int32(inline354)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t235 bool = self__94 == other__95
    return t235
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__117 int32) uint64 {
    var t238 uint64 = _goml_runtime_core_int32_hash(self__117)
    return t238
}

func println__T_string(value__31 string) struct{} {
    var t240 string
    t240 = value__31
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t243 string
    var inline394 string = _goml_runtime_core_int32_to_string(value__31)
    t243 = inline394
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t247 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t247
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__206 *hashmap_Key_int32_x, key__207 Key, value__208 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__206, key__207, value__208)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t251 string
    var inline396 string = _goml_runtime_core_int_to_string(value__31)
    t251 = inline396
    _goml_runtime_core_string_println(t251)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__211 *hashmap_Key_int32_x) int {
    var t255 int = hashmap_len__HashMap_3Key_5int32(self__211)
    return t255
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__204 *hashmap_Key_int32_x, key__205 Key) Option__int32 {
    var t258 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__204, key__205)
    return t258
}

func println__T_bool(value__31 bool) struct{} {
    var t260 string
    var inline398 string = _goml_runtime_core_bool_to_string(value__31)
    t260 = inline398
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__212 *hashmap_Key_int32_x, key__213 Key) bool {
    var t264 bool = hashmap_contains__HashMap_3Key_5int32(self__212, key__213)
    return t264
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__209 *hashmap_Key_int32_x, key__210 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__209, key__210)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t269 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t269
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__215 Point) *ref_Point_x {
    var t272 *ref_Point_x = ref__Ref_5Point(value__215)
    return t272
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__206 *hashmap_Ref_5Point_int32_x, key__207 *ref_Point_x, value__208 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__206, key__207, value__208)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__204 *hashmap_Ref_5Point_int32_x, key__205 *ref_Point_x) Option__int32 {
    var t277 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__204, key__205)
    return t277
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__125 *ref_Point_x, other__126 *ref_Point_x) bool {
    var t282 bool = ptr_eq__Ref_5Point(self__125, other__126)
    return t282
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t298 string = _goml_runtime_core_int32_to_string(self__72)
    return t298
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t304 string = _goml_runtime_core_bool_to_string(self__66)
    return t304
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__127 *ref_Point_x) uint64 {
    var t307 uint64 = ptr_hash__Ref_5Point(self__127)
    return t307
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__125 *ref_Key_x, other__126 *ref_Key_x) bool {
    var t310 bool = ptr_eq__Ref_3Key(self__125, other__126)
    return t310
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__127 *ref_Key_x) uint64 {
    var t313 uint64 = ptr_hash__Ref_3Key(self__127)
    return t313
}

func main() {
    main0()
}
