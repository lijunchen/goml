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

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
}

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__0 Key) uint64 {
    switch self__0._tag {
    case 0:
        var t435_source int = 0
        var t435 uint64 = uint64(int(t435_source))
        var t436 uint64 = t435 + 14695981039346656037
        var h__1 uint64 = t436 + 1
        return h__1
    case 1:
        var x408 int32 = self__0._v1_0
        var t437_source int = 0
        var t437 uint64 = uint64(int(t437_source))
        var t438 uint64 = t437 + 14695981039346656037
        var h__3 uint64 = t438 + 2
        var t439_source int = 0
        var t439 uint64 = uint64(int(t439_source))
        var t440 uint64 = t439 + 1099511628211
        var t441 uint64 = h__3 * t440
        var t442 uint64
        var inline529 uint64 = _goml_runtime_core_int32_hash(x408)
        t442 = inline529
        var h__4 uint64 = t441 + t442
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
        var x412 int32 = other__6._v1_0
        switch self__5._tag {
        case 1:
            var x414 int32 = self__5._v1_0
            var inline531 bool = x414 == x412
            return inline531
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 30)
    var t459 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t459)
    var t460 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t460)
    var t461 int
    var inline585 int = 2
    var inline586 int = vec_get__Vec_3int(v__11, inline585)
    t461 = inline586
    var inline582 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t461)
    _goml_runtime_core_string_println(inline582)
    var t462 int
    var inline580 int = vec_len__Vec_3int(v__11)
    t462 = inline580
    var inline577 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t462)
    _goml_runtime_core_string_println(inline577)
    var m__12 *hashmap_Key_int32_x
    var inline575 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline575
    var inline572 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    }, inline572)
    var t463 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var inline569 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t463, inline569)
    var t464 int
    var inline567 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t464 = inline567
    var inline564 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t464)
    _goml_runtime_core_string_println(inline564)
    var t465 Option__int32
    var inline562 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    })
    t465 = inline562
    switch t465._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline558 int32 = t465._v1_0
        println__T_int32(inline558)
    default:
        panic("non-exhaustive match")
    }
    var t466 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t467 bool
    var inline555 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t466)
    t467 = inline555
    var inline552 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t467)
    _goml_runtime_core_string_println(inline552)
    var t468 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t468)
    var t469 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t470 bool
    var inline548 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t469)
    t470 = inline548
    var inline545 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t470)
    _goml_runtime_core_string_println(inline545)
    var t471 int
    var inline543 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t471 = inline543
    var inline540 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t471)
    _goml_runtime_core_string_println(inline540)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t479 string
    t479 = value__1
    _goml_runtime_core_string_println(t479)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t482 string
    var inline589 string = _goml_runtime_core_int32_to_string(value__1)
    t482 = inline589
    _goml_runtime_core_string_println(t482)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t486 *_goml_vec_int = vec_new__Vec_3int()
    return t486
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t490 string
    var inline591 string = _goml_runtime_core_int_to_string(value__1)
    t490 = inline591
    _goml_runtime_core_string_println(t490)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__268 *_goml_vec_int, index__269 int) int {
    var t494 int = vec_get__Vec_3int(self__268, index__269)
    return t494
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t524 string = _goml_runtime_core_int_to_string(self__151)
    return t524
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t527 string = _goml_runtime_core_bool_to_string(self__148)
    return t527
}

func main() {
    main0()
}
