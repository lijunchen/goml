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
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
        _tag: 0,
    }
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
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
        _tag: 0,
    }
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
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
        _tag: 0,
    }
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

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
    _v2_0 Point
}

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__6 Key) uint64 {
    switch self__6._tag {
    case 0:
        var t470_source int = 0
        var t470 uint64 = uint64(int(t470_source))
        var t471 uint64 = t470 + 14695981039346656037
        var h__7 uint64 = t471 + 1
        return h__7
    case 1:
        var x411 int32 = self__6._v1_0
        var t472_source int = 0
        var t472 uint64 = uint64(int(t472_source))
        var t473 uint64 = t472 + 14695981039346656037
        var h__9 uint64 = t473 + 2
        var t474_source int = 0
        var t474 uint64 = uint64(int(t474_source))
        var t475 uint64 = t474 + 1099511628211
        var t476 uint64 = h__9 * t475
        var t477 uint64
        var inline613 uint64 = _goml_runtime_core_int32_hash(x411)
        t477 = inline613
        var h__10 uint64 = t476 + t477
        return h__10
    case 2:
        var x412 Point = self__6._v2_0
        var t478_source int = 0
        var t478 uint64 = uint64(int(t478_source))
        var t479 uint64 = t478 + 14695981039346656037
        var h__12 uint64 = t479 + 3
        var t480_source int = 0
        var t480 uint64 = uint64(int(t480_source))
        var t481 uint64 = t480 + 1099511628211
        var t482 uint64 = h__12 * t481
        var t483 uint64
        var inline615_source int = 0
        var inline615 uint64 = uint64(int(inline615_source))
        var inline616 uint64 = inline615 + 14695981039346656037
        var inline617_source int = 0
        var inline617 uint64 = uint64(int(inline617_source))
        var inline618 uint64 = inline617 + 1099511628211
        var inline619 uint64 = inline616 * inline618
        var inline620 int32 = x412.x
        var inline621 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline620)
        var inline622 uint64 = inline619 + inline621
        var inline623_source int = 0
        var inline623 uint64 = uint64(int(inline623_source))
        var inline624 uint64 = inline623 + 1099511628211
        var inline625 uint64 = inline622 * inline624
        var inline626 int32 = x412.y
        var inline627 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(inline626)
        var inline628 uint64 = inline625 + inline627
        t483 = inline628
        var h__13 uint64 = t482 + t483
        return h__13
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__14 Key, other__15 Key) bool {
    switch other__15._tag {
    case 0:
        switch self__14._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x416 int32 = other__15._v1_0
        switch self__14._tag {
        case 1:
            var x420 int32 = self__14._v1_0
            var inline630 bool = x420 == x416
            return inline630
        default:
            return false
        }
    case 2:
        var x417 Point = other__15._v2_0
        switch self__14._tag {
        case 2:
            var x423 Point = self__14._v2_0
            var inline633 bool
            var inline637 int32 = x423.x
            var inline638 int32 = x417.x
            var inline639 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline637, inline638)
            inline633 = inline639
            if inline633 {
                var inline634 int32 = x423.y
                var inline635 int32 = x417.y
                var inline636 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline634, inline635)
                return inline636
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
    switch x__20._tag {
    case 0:
        var inline641 string = "none"
        var inline642 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline641)
        _goml_runtime_core_string_println(inline642)
        return struct{}{}
    case 1:
        var x424 int32 = x__20._v1_0
        var inline645 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x424)
        _goml_runtime_core_string_println(inline645)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, Key{
        _tag: 0,
    }, 10)
    var t505 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t505, 20)
    var t506 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t506)
    var t507 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, Key{
        _tag: 0,
    })
    print_opt_int(t507)
    var t508 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t509 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t508)
    println__T_bool(t509)
    var t510 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t510)
    var t511 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t512 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t511)
    println__T_bool(t512)
    var t513 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t513)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t514 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t514)
    var t515 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t515)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t516 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    switch t516._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline684 int32 = t516._v1_0
        println__T_int32(inline684)
    default:
        panic("non-exhaustive match")
    }
    var t517 Option__int32
    var inline681 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t517 = inline681
    switch t517._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline677 int32 = t517._v1_0
        println__T_int32(inline677)
    default:
        panic("non-exhaustive match")
    }
    var t518 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t518)
    var t519 Option__int32
    var inline672 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t519 = inline672
    switch t519._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline668 int32 = t519._v1_0
        println__T_int32(inline668)
    default:
        panic("non-exhaustive match")
    }
    var t520 bool
    var inline665 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t520 = inline665
    var inline662 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t520)
    _goml_runtime_core_string_println(inline662)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline660 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline660
    var t521 Key = Key{
        _tag: 1,
        _v1_0: 7,
    }
    var k1__27 *ref_Key_x
    var inline658 *ref_Key_x = ref__Ref_3Key(t521)
    k1__27 = inline658
    var inline655 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline655)
    var t522 Option__int32
    var inline653 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t522 = inline653
    switch t522._tag {
    case 0:
        println__T_string("none")
        return struct{}{}
    case 1:
        var inline649 int32 = t522._v1_0
        println__T_int32(inline649)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__214 int32) uint64 {
    var t525 uint64 = _goml_runtime_core_int32_hash(self__214)
    return t525
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__191 int32, other__192 int32) bool {
    var t528 bool = self__191 == other__192
    return t528
}

func println__T_string(value__1 string) struct{} {
    var t530 string
    t530 = value__1
    _goml_runtime_core_string_println(t530)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t533 string
    var inline689 string = _goml_runtime_core_int32_to_string(value__1)
    t533 = inline689
    _goml_runtime_core_string_println(t533)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var t537 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t537
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__422 *hashmap_Key_int32_x, key__423 Key, value__424 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t541 string
    var inline691 string = _goml_runtime_core_int_to_string(value__1)
    t541 = inline691
    _goml_runtime_core_string_println(t541)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__427 *hashmap_Key_int32_x) int {
    var t545 int = hashmap_len__HashMap_3Key_5int32(self__427)
    return t545
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__420 *hashmap_Key_int32_x, key__421 Key) Option__int32 {
    var t548 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__420, key__421)
    return t548
}

func println__T_bool(value__1 bool) struct{} {
    var t550 string
    var inline693 string = _goml_runtime_core_bool_to_string(value__1)
    t550 = inline693
    _goml_runtime_core_string_println(t550)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__428 *hashmap_Key_int32_x, key__429 Key) bool {
    var t554 bool = hashmap_contains__HashMap_3Key_5int32(self__428, key__429)
    return t554
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__425 *hashmap_Key_int32_x, key__426 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__425, key__426)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var t559 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t559
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__431 Point) *ref_Point_x {
    var t562 *ref_Point_x = ref__Ref_5Point(value__431)
    return t562
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__422 *hashmap_Ref_5Point_int32_x, key__423 *ref_Point_x, value__424 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__420 *hashmap_Ref_5Point_int32_x, key__421 *ref_Point_x) Option__int32 {
    var t567 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__420, key__421)
    return t567
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(self__220 *ref_Point_x, other__221 *ref_Point_x) bool {
    var t572 bool = ptr_eq__Ref_5Point(self__220, other__221)
    return t572
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t588 string = _goml_runtime_core_int32_to_string(self__154)
    return t588
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t594 string = _goml_runtime_core_bool_to_string(self__148)
    return t594
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__222 *ref_Point_x) uint64 {
    var t597 uint64 = ptr_hash__Ref_5Point(self__222)
    return t597
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(self__220 *ref_Key_x, other__221 *ref_Key_x) bool {
    var t600 bool = ptr_eq__Ref_3Key(self__220, other__221)
    return t600
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__222 *ref_Key_x) uint64 {
    var t603 uint64 = ptr_hash__Ref_3Key(self__222)
    return t603
}

func main() {
    main0()
}
