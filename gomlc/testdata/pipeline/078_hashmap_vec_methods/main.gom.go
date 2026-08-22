package main

import (
    _goml_fmt "fmt"
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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
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

func hashmap_get__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_3Key_5int32(m, key)
    if ok {
        return Option__i32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__i32{
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

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__0 Key) uint64 {
    switch self__0._tag {
    case 0:
        var t438_source int = 0
        var t438 uint64 = uint64(int(t438_source))
        var t439 uint64 = t438 + 14695981039346656037
        var h__1 uint64 = t439 + 1
        return h__1
    case 1:
        var x411 int32 = self__0._v1_0
        var t440_source int = 0
        var t440 uint64 = uint64(int(t440_source))
        var t441 uint64 = t440 + 14695981039346656037
        var h__3 uint64 = t441 + 2
        var t442_source int = 0
        var t442 uint64 = uint64(int(t442_source))
        var t443 uint64 = t442 + 1099511628211
        var t444 uint64 = h__3 * t443
        var t445 uint64
        var inline532 uint64 = _goml_runtime_core_int32_hash(x411)
        t445 = inline532
        var h__4 uint64 = t444 + t445
        return h__4
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__5 Key, other__6 Key) bool {
    switch other__6._tag {
    case 0:
        switch self__5._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x415 int32 = other__6._v1_0
        switch self__5._tag {
        case 1:
            var x417 int32 = self__5._v1_0
            var inline534 bool = x417 == x415
            return inline534
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 30)
    var t462 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(v__11, 0)
    println__T_isize(t462)
    var t463 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(v__11, 1)
    println__T_isize(t463)
    var t464 int
    var inline588 int = 2
    var inline589 int = vec_get__Vec_3int(v__11, inline588)
    t464 = inline589
    var inline585 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t464)
    _goml_runtime_core_string_println(inline585)
    var t465 int
    var inline583 int = vec_len__Vec_3int(v__11)
    t465 = inline583
    var inline580 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t465)
    _goml_runtime_core_string_println(inline580)
    var m__12 *hashmap_Key_int32_x
    var inline578 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline578
    var inline575 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    }, inline575)
    var t466 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var inline572 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t466, inline572)
    var t467 int
    var inline570 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t467 = inline570
    var inline567 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t467)
    _goml_runtime_core_string_println(inline567)
    var t468 Option__i32
    var inline565 Option__i32 = hashmap_get__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    })
    t468 = inline565
    switch t468._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline561 int32 = t468._v1_0
        println__T_i32(inline561)
    default:
        panic("non-exhaustive match")
    }
    var t469 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t470 bool
    var inline558 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t469)
    t470 = inline558
    var inline555 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t470)
    _goml_runtime_core_string_println(inline555)
    var t471 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t471)
    var t472 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t473 bool
    var inline551 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t472)
    t473 = inline551
    var inline548 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t473)
    _goml_runtime_core_string_println(inline548)
    var t474 int
    var inline546 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t474 = inline546
    var inline543 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t474)
    _goml_runtime_core_string_println(inline543)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t482 string
    t482 = value__1
    _goml_runtime_core_string_println(t482)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t485 string
    var inline592 string = _goml_runtime_core_int32_to_string(value__1)
    t485 = inline592
    _goml_runtime_core_string_println(t485)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t489 *_goml_vec_int = vec_new__Vec_3int()
    return t489
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t493 string
    var inline594 string = _goml_runtime_core_int_to_string(value__1)
    t493 = inline594
    _goml_runtime_core_string_println(t493)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(self__268 *_goml_vec_int, index__269 int) int {
    var t497 int = vec_get__Vec_3int(self__268, index__269)
    return t497
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t527 string = _goml_runtime_core_int_to_string(self__151)
    return t527
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t530 string = _goml_runtime_core_bool_to_string(self__148)
    return t530
}

func main() {
    main0()
}
