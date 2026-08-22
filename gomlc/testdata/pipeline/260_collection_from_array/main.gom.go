package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_8Ref_3int(arr [2]*ref_int_x, index int) *ref_int_x {
    return arr[index]
}

func array_set__Array_2_8Ref_3int(arr [2]*ref_int_x, index int, value *ref_int_x) [2]*ref_int_x {
    arr[index] = value
    return arr
}

type _goml_vec_int struct {
    items []int
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_Ref_3int struct {
    items []*ref_int_x
}

func vec_get__Vec_8Ref_3int(vec *_goml_vec_Ref_3int, index int) *ref_int_x {
    return vec.items[index]
}

type _goml_vec_Tuple2_3int_6string struct {
    items []Tuple2_3int_6string
}

func vec_get__Vec_19Tuple2_3int_6string(vec *_goml_vec_Tuple2_3int_6string, index int) Tuple2_3int_6string {
    return vec.items[index]
}

type _goml_vec_Vec_3int struct {
    items []*_goml_vec_int
}

func vec_get__Vec_8Vec_3int(vec *_goml_vec_Vec_3int, index int) *_goml_vec_int {
    return vec.items[index]
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_LoggedKey_int_x_entry struct {
    active bool
    key LoggedKey
    value int
}

type hashmap_LoggedKey_int_x struct {
    buckets map[uint64][]hashmap_LoggedKey_int_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_9LoggedKey_3int() *hashmap_LoggedKey_int_x {
    return &hashmap_LoggedKey_int_x{
        buckets: make(map[uint64][]hashmap_LoggedKey_int_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_9LoggedKey_3int(m *hashmap_LoggedKey_int_x, key LoggedKey) (int, bool) {
    if m == nil {
        var zero int
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(key)
    var bucket []hashmap_LoggedKey_int_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_LoggedKey_int_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int
    return zero, false
}

func hashmap_get__HashMap_9LoggedKey_3int(m *hashmap_LoggedKey_int_x, key LoggedKey) Option__isize {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_9LoggedKey_3int(m, key)
    if ok {
        return Option__isize{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__isize{
        _tag: 0,
    }
}

func hashmap_set__HashMap_9LoggedKey_3int(m *hashmap_LoggedKey_int_x, key LoggedKey, value int) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(key)
    var bucket []hashmap_LoggedKey_int_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_LoggedKey_int_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_LoggedKey_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_LoggedKey_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_string_int_x_entry struct {
    active bool
    key string
    value int
}

type hashmap_string_int_x struct {
    indices map[string]int
    entries []hashmap_string_int_x_entry
    len int
}

func hashmap_new__HashMap_6string_3int() *hashmap_string_int_x {
    return &hashmap_string_int_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_len__HashMap_6string_3int(m *hashmap_string_int_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_set__HashMap_6string_3int(m *hashmap_string_int_x, key string, value int) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_string_Vec_3int_x_entry struct {
    active bool
    key string
    value *_goml_vec_int
}

type hashmap_string_Vec_3int_x struct {
    indices map[string]int
    entries []hashmap_string_Vec_3int_x_entry
    len int
}

func hashmap_new__HashMap_6string_8Vec_3int() *hashmap_string_Vec_3int_x {
    return &hashmap_string_Vec_3int_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_6string_8Vec_3int(m *hashmap_string_Vec_3int_x, key string) (*_goml_vec_int, bool) {
    if m == nil {
        var zero *_goml_vec_int
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero *_goml_vec_int
        return zero, false
    }
    var entry hashmap_string_Vec_3int_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero *_goml_vec_int
    return zero, false
}

func hashmap_get__HashMap_6string_8Vec_3int(m *hashmap_string_Vec_3int_x, key string) _goml_m_Option____Vec_l_isize_r_ {
    var value *_goml_vec_int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_8Vec_3int(m, key)
    if ok {
        return _goml_m_Option____Vec_l_isize_r_{
            _tag: 1,
            _v1_0: value,
        }
    }
    return _goml_m_Option____Vec_l_isize_r_{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_8Vec_3int(m *hashmap_string_Vec_3int_x, key string, value *_goml_vec_int) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_Vec_3int_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_Vec_3int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_Vec_3int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_9LoggedKey_3int struct {
    _0 LoggedKey
    _1 int
}

type Tuple2_6string_3int struct {
    _0 string
    _1 int
}

type Tuple2_3int_6string struct {
    _0 int
    _1 string
}

type Tuple2_6string_8Vec_3int struct {
    _0 string
    _1 *_goml_vec_int
}

type LoggedKey struct {
    id int
    log *ref_string_x
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type _goml_m_Option____Vec_l_isize_r_ struct {
    _tag int32
    _v1_0 *_goml_vec_int
}

func _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(self__0 LoggedKey, other__1 LoggedKey) bool {
    var t436 *ref_string_x = self__0.log
    var t437 *ref_string_x = self__0.log
    var t438 string
    var inline594 string = ref_get__Ref_6string(t437)
    t438 = inline594
    var t439 string = t438 + "E"
    ref_set__Ref_6string(t436, t439)
    var t440 int = self__0.id
    var t441 int = other__1.id
    var t442 bool = t440 == t441
    return t442
}

func _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(self__2 LoggedKey) uint64 {
    var t445 *ref_string_x = self__2.log
    var t446 *ref_string_x = self__2.log
    var t447 string
    var inline598 string = ref_get__Ref_6string(t446)
    t447 = inline598
    var t448 string = t447 + "H"
    ref_set__Ref_6string(t445, t448)
    var t449 int = self__2.id
    var t450 uint64 = uint64(int(t449))
    return t450
}

func logged_key(log__3 *ref_string_x, label__4 string, id__5 int) LoggedKey {
    var t453 string
    var inline602 string = ref_get__Ref_6string(log__3)
    t453 = inline602
    var t454 string = t453 + label__4
    ref_set__Ref_6string(log__3, t454)
    var t455 LoggedKey = LoggedKey{
        id: id__5,
        log: log__3,
    }
    return t455
}

func main0() struct{} {
    var make_vec__9 func([3]int) *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }
    var t461 [3]int = [3]int{1, 2, 3}
    var values__10 *_goml_vec_int = make_vec__9(t461)
    var t462 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(values__10)
    var t463 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t462)
    var t464 string = "" + t463
    var t465 string = t464 + ":"
    var t466 int = vec_get__Vec_3int(values__10, 0)
    var t467 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t466)
    var t468 string = t465 + t467
    var t469 string = t468 + ":"
    var t470 int = vec_get__Vec_3int(values__10, 2)
    var t471 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t470)
    var t472 string = t469 + t471
    println__T_string(t472)
    var t473 [0]int = [0]int{}
    var empty__11 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t473)
    var t474 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(empty__11)
    println__T_isize(t474)
    var t475 [0]int = [0]int{}
    var inferred_empty__12 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t475)
    var t476 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inferred_empty__12)
    println__T_isize(t476)
    var t477 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(1)
    var t478 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(2)
    var source__13 [2]*ref_int_x = [2]*ref_int_x{t477, t478}
    var copied__14 *_goml_vec_Ref_3int = func(values [2]*ref_int_x) *_goml_vec_Ref_3int {
        var storage struct {
            vector _goml_vec_Ref_3int
            values [2]*ref_int_x
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(source__13)
    var t479 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t479, 5)
    var place_root419 [2]*ref_int_x = source__13
    var index420 int = 0
    array_get__Array_2_8Ref_3int(place_root419, index420)
    var value422 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(9)
    var t480 [2]*ref_int_x = array_set__Array_2_8Ref_3int(place_root419, index420, value422)
    source__13 = t480
    var t482 *ref_int_x = vec_get__Vec_8Ref_3int(copied__14, 0)
    var t483 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t482)
    var t484 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t483)
    var t485 string = "" + t484
    var t486 string = t485 + ":"
    var t487 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    var t488 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t487)
    var t489 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t488)
    var t490 string = t486 + t489
    println__T_string(t490)
    var log__15 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var t491 LoggedKey = logged_key(log__15, "A", 1)
    var t492 int
    var inline656 string = "a"
    var inline657 int = 10
    var inline658 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline659 string = inline658 + inline656
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline659)
    t492 = inline657
    var t493 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t491,
        _1: t492,
    }
    var t494 LoggedKey
    var inline649 string = "B"
    var inline650 int = 1
    var inline651 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline652 string = inline651 + inline649
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline652)
    var inline654 LoggedKey = LoggedKey{
        id: inline650,
        log: log__15,
    }
    t494 = inline654
    var t495 int
    var inline643 string = "b"
    var inline644 int = 20
    var inline645 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline646 string = inline645 + inline643
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline646)
    t495 = inline644
    var t496 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t494,
        _1: t495,
    }
    var t497 [2]Tuple2_9LoggedKey_3int = [2]Tuple2_9LoggedKey_3int{t493, t496}
    var table__16 *hashmap_LoggedKey_int_x = func(values [2]Tuple2_9LoggedKey_3int) *hashmap_LoggedKey_int_x {
        var result *hashmap_LoggedKey_int_x = hashmap_new__HashMap_9LoggedKey_3int()
        for _, entry := range values {
            hashmap_set__HashMap_9LoggedKey_3int(result, entry._0, entry._1)
        }
        return result
    }(t497)
    var t498 string
    var inline641 string = ref_get__Ref_6string(log__15)
    t498 = inline641
    var inline638 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t498)
    _goml_runtime_core_string_println(inline638)
    var t499 LoggedKey = LoggedKey{
        id: 1,
        log: log__15,
    }
    var mtmp426 Option__isize
    var inline636 Option__isize = hashmap_get__HashMap_9LoggedKey_3int(table__16, t499)
    mtmp426 = inline636
    var jp501 int
    switch mtmp426._tag {
    case 0:
        jp501 = 0
    case 1:
        var x427 int = mtmp426._v1_0
        jp501 = x427
    default:
        panic("non-exhaustive match")
    }
    var inline633 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp501)
    _goml_runtime_core_string_println(inline633)
    var make_map__18 func([2]Tuple2_6string_3int) *hashmap_string_int_x = func(values [2]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }
    var t502 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "a",
        _1: 1,
    }
    var t503 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "b",
        _1: 2,
    }
    var t504 [2]Tuple2_6string_3int = [2]Tuple2_6string_3int{t502, t503}
    var words__19 *hashmap_string_int_x = make_map__18(t504)
    var t505 int
    var inline631 int = hashmap_len__HashMap_6string_3int(words__19)
    t505 = inline631
    var inline628 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t505)
    _goml_runtime_core_string_println(inline628)
    var t506 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var no_words__20 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t506)
    var t507 int
    var inline626 int = hashmap_len__HashMap_6string_3int(no_words__20)
    t507 = inline626
    var inline623 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t507)
    _goml_runtime_core_string_println(inline623)
    var t508 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var inferred_no_words__21 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t508)
    var t509 int
    var inline621 int = hashmap_len__HashMap_6string_3int(inferred_no_words__21)
    t509 = inline621
    var inline618 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t509)
    _goml_runtime_core_string_println(inline618)
    var t510 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "a",
    }
    var t511 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 2,
        _1: "b",
    }
    var t512 [2]Tuple2_3int_6string = [2]Tuple2_3int_6string{t510, t511}
    var pairs__22 *_goml_vec_Tuple2_3int_6string = func(values [2]Tuple2_3int_6string) *_goml_vec_Tuple2_3int_6string {
        var storage struct {
            vector _goml_vec_Tuple2_3int_6string
            values [2]Tuple2_3int_6string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t512)
    var t513 [2]int = [2]int{1, 2}
    var t514 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t513)
    var t515 [2]int = [2]int{3, 4}
    var t516 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t515)
    var t517 [2]*_goml_vec_int = [2]*_goml_vec_int{t514, t516}
    var nested__23 *_goml_vec_Vec_3int = func(values [2]*_goml_vec_int) *_goml_vec_Vec_3int {
        var storage struct {
            vector _goml_vec_Vec_3int
            values [2]*_goml_vec_int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t517)
    var t518 [2]int = [2]int{5, 6}
    var t519 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t518)
    var t520 Tuple2_6string_8Vec_3int = Tuple2_6string_8Vec_3int{
        _0: "values",
        _1: t519,
    }
    var t521 [1]Tuple2_6string_8Vec_3int = [1]Tuple2_6string_8Vec_3int{t520}
    var nested_map__24 *hashmap_string_Vec_3int_x = func(values [1]Tuple2_6string_8Vec_3int) *hashmap_string_Vec_3int_x {
        var result *hashmap_string_Vec_3int_x = hashmap_new__HashMap_6string_8Vec_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_8Vec_3int(result, entry._0, entry._1)
        }
        return result
    }(t521)
    var mtmp432 _goml_m_Option____Vec_l_isize_r_
    var inline615 string = "values"
    var inline616 _goml_m_Option____Vec_l_isize_r_ = hashmap_get__HashMap_6string_8Vec_3int(nested_map__24, inline615)
    mtmp432 = inline616
    var jp523 int
    switch mtmp432._tag {
    case 0:
        jp523 = 0
    case 1:
        var x433 *_goml_vec_int = mtmp432._v1_0
        var t536 int = vec_get__Vec_3int(x433, 0)
        jp523 = t536
    default:
        panic("non-exhaustive match")
    }
    var t524 Tuple2_3int_6string = vec_get__Vec_19Tuple2_3int_6string(pairs__22, 1)
    var t525 string = t524._1
    var t526 string = "" + t525
    var t527 string = t526 + ":"
    var t528 *_goml_vec_int = vec_get__Vec_8Vec_3int(nested__23, 1)
    var t529 int = vec_get__Vec_3int(t528, 0)
    var t530 string
    var inline613 string = _goml_runtime_core_int_to_string(t529)
    t530 = inline613
    var t531 string = t527 + t530
    var t532 string = t531 + ":"
    var t533 string
    var inline611 string = _goml_runtime_core_int_to_string(jp523)
    t533 = inline611
    var t534 string = t532 + t533
    var inline608 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t534)
    _goml_runtime_core_string_println(inline608)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t539 string = ref_get__Ref_6string(self__432)
    return t539
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t543 string
    t543 = value__1
    _goml_runtime_core_string_println(t543)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__273 *_goml_vec_int) int {
    var t547 int = vec_len__Vec_3int(self__273)
    return t547
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t550 string = _goml_runtime_core_int_to_string(self__32)
    return t550
}

func println__T_isize(value__1 int) struct{} {
    var t552 string
    var inline663 string = _goml_runtime_core_int_to_string(value__1)
    t552 = inline663
    _goml_runtime_core_string_println(t552)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__431 int) *ref_int_x {
    var t556 *ref_int_x = ref__Ref_3int(value__431)
    return t556
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__432 *ref_int_x) int {
    var t561 int = ref_get__Ref_3int(self__432)
    return t561
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t564 *ref_string_x = ref__Ref_6string(value__431)
    return t564
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t578 string = _goml_runtime_core_int_to_string(self__151)
    return t578
}

func main() {
    main0()
}
