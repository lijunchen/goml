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

func hashmap_get__HashMap_9LoggedKey_3int(m *hashmap_LoggedKey_int_x, key LoggedKey) Option__int {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_9LoggedKey_3int(m, key)
    if ok {
        return Option__int{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int{
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

func hashmap_get__HashMap_6string_8Vec_3int(m *hashmap_string_Vec_3int_x, key string) _goml_m_Option____Vec_l_int_r_ {
    var value *_goml_vec_int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_8Vec_3int(m, key)
    if ok {
        return _goml_m_Option____Vec_l_int_r_{
            _tag: 1,
            _v1_0: value,
        }
    }
    return _goml_m_Option____Vec_l_int_r_{
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

type Option__int struct {
    _tag int32
    _v1_0 int
}

type _goml_m_Option____Vec_l_int_r_ struct {
    _tag int32
    _v1_0 *_goml_vec_int
}

func _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(self__0 LoggedKey, other__1 LoggedKey) bool {
    var t433 *ref_string_x = self__0.log
    var t434 *ref_string_x = self__0.log
    var t435 string
    var inline591 string = ref_get__Ref_6string(t434)
    t435 = inline591
    var t436 string = t435 + "E"
    ref_set__Ref_6string(t433, t436)
    var t437 int = self__0.id
    var t438 int = other__1.id
    var t439 bool = t437 == t438
    return t439
}

func _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(self__2 LoggedKey) uint64 {
    var t442 *ref_string_x = self__2.log
    var t443 *ref_string_x = self__2.log
    var t444 string
    var inline595 string = ref_get__Ref_6string(t443)
    t444 = inline595
    var t445 string = t444 + "H"
    ref_set__Ref_6string(t442, t445)
    var t446 int = self__2.id
    var t447 uint64 = uint64(int(t446))
    return t447
}

func logged_key(log__3 *ref_string_x, label__4 string, id__5 int) LoggedKey {
    var t450 string
    var inline599 string = ref_get__Ref_6string(log__3)
    t450 = inline599
    var t451 string = t450 + label__4
    ref_set__Ref_6string(log__3, t451)
    var t452 LoggedKey = LoggedKey{
        id: id__5,
        log: log__3,
    }
    return t452
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
    var t458 [3]int = [3]int{1, 2, 3}
    var values__10 *_goml_vec_int = make_vec__9(t458)
    var t459 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(values__10)
    var t460 string = _goml_m_inherent_i_int_i_int_i_to__string(t459)
    var t461 string = "" + t460
    var t462 string = t461 + ":"
    var t463 int = vec_get__Vec_3int(values__10, 0)
    var t464 string = _goml_m_inherent_i_int_i_int_i_to__string(t463)
    var t465 string = t462 + t464
    var t466 string = t465 + ":"
    var t467 int = vec_get__Vec_3int(values__10, 2)
    var t468 string = _goml_m_inherent_i_int_i_int_i_to__string(t467)
    var t469 string = t466 + t468
    println__T_string(t469)
    var t470 [0]int = [0]int{}
    var empty__11 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t470)
    var t471 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(empty__11)
    println__T_int(t471)
    var t472 [0]int = [0]int{}
    var inferred_empty__12 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t472)
    var t473 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(inferred_empty__12)
    println__T_int(t473)
    var t474 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(1)
    var t475 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(2)
    var source__13 [2]*ref_int_x = [2]*ref_int_x{t474, t475}
    var copied__14 *_goml_vec_Ref_3int = func(values [2]*ref_int_x) *_goml_vec_Ref_3int {
        var storage struct {
            vector _goml_vec_Ref_3int
            values [2]*ref_int_x
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(source__13)
    var t476 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t476, 5)
    var place_root416 [2]*ref_int_x = source__13
    var index417 int = 0
    array_get__Array_2_8Ref_3int(place_root416, index417)
    var value419 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(9)
    var t477 [2]*ref_int_x = array_set__Array_2_8Ref_3int(place_root416, index417, value419)
    source__13 = t477
    var t479 *ref_int_x = vec_get__Vec_8Ref_3int(copied__14, 0)
    var t480 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t479)
    var t481 string = _goml_m_inherent_i_int_i_int_i_to__string(t480)
    var t482 string = "" + t481
    var t483 string = t482 + ":"
    var t484 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    var t485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t484)
    var t486 string = _goml_m_inherent_i_int_i_int_i_to__string(t485)
    var t487 string = t483 + t486
    println__T_string(t487)
    var log__15 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var t488 LoggedKey = logged_key(log__15, "A", 1)
    var t489 int
    var inline653 string = "a"
    var inline654 int = 10
    var inline655 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline656 string = inline655 + inline653
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline656)
    t489 = inline654
    var t490 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t488,
        _1: t489,
    }
    var t491 LoggedKey
    var inline646 string = "B"
    var inline647 int = 1
    var inline648 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline649 string = inline648 + inline646
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline649)
    var inline651 LoggedKey = LoggedKey{
        id: inline647,
        log: log__15,
    }
    t491 = inline651
    var t492 int
    var inline640 string = "b"
    var inline641 int = 20
    var inline642 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline643 string = inline642 + inline640
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline643)
    t492 = inline641
    var t493 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t491,
        _1: t492,
    }
    var t494 [2]Tuple2_9LoggedKey_3int = [2]Tuple2_9LoggedKey_3int{t490, t493}
    var table__16 *hashmap_LoggedKey_int_x = func(values [2]Tuple2_9LoggedKey_3int) *hashmap_LoggedKey_int_x {
        var result *hashmap_LoggedKey_int_x = hashmap_new__HashMap_9LoggedKey_3int()
        for _, entry := range values {
            hashmap_set__HashMap_9LoggedKey_3int(result, entry._0, entry._1)
        }
        return result
    }(t494)
    var t495 string
    var inline638 string = ref_get__Ref_6string(log__15)
    t495 = inline638
    var inline635 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t495)
    _goml_runtime_core_string_println(inline635)
    var t496 LoggedKey = LoggedKey{
        id: 1,
        log: log__15,
    }
    var mtmp423 Option__int
    var inline633 Option__int = hashmap_get__HashMap_9LoggedKey_3int(table__16, t496)
    mtmp423 = inline633
    var jp498 int
    switch mtmp423._tag {
    case 0:
        jp498 = 0
    case 1:
        var x424 int = mtmp423._v1_0
        jp498 = x424
    default:
        panic("non-exhaustive match")
    }
    var inline630 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp498)
    _goml_runtime_core_string_println(inline630)
    var make_map__18 func([2]Tuple2_6string_3int) *hashmap_string_int_x = func(values [2]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }
    var t499 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "a",
        _1: 1,
    }
    var t500 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "b",
        _1: 2,
    }
    var t501 [2]Tuple2_6string_3int = [2]Tuple2_6string_3int{t499, t500}
    var words__19 *hashmap_string_int_x = make_map__18(t501)
    var t502 int
    var inline628 int = hashmap_len__HashMap_6string_3int(words__19)
    t502 = inline628
    var inline625 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t502)
    _goml_runtime_core_string_println(inline625)
    var t503 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var no_words__20 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t503)
    var t504 int
    var inline623 int = hashmap_len__HashMap_6string_3int(no_words__20)
    t504 = inline623
    var inline620 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t504)
    _goml_runtime_core_string_println(inline620)
    var t505 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var inferred_no_words__21 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t505)
    var t506 int
    var inline618 int = hashmap_len__HashMap_6string_3int(inferred_no_words__21)
    t506 = inline618
    var inline615 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t506)
    _goml_runtime_core_string_println(inline615)
    var t507 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "a",
    }
    var t508 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 2,
        _1: "b",
    }
    var t509 [2]Tuple2_3int_6string = [2]Tuple2_3int_6string{t507, t508}
    var pairs__22 *_goml_vec_Tuple2_3int_6string = func(values [2]Tuple2_3int_6string) *_goml_vec_Tuple2_3int_6string {
        var storage struct {
            vector _goml_vec_Tuple2_3int_6string
            values [2]Tuple2_3int_6string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t509)
    var t510 [2]int = [2]int{1, 2}
    var t511 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t510)
    var t512 [2]int = [2]int{3, 4}
    var t513 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t512)
    var t514 [2]*_goml_vec_int = [2]*_goml_vec_int{t511, t513}
    var nested__23 *_goml_vec_Vec_3int = func(values [2]*_goml_vec_int) *_goml_vec_Vec_3int {
        var storage struct {
            vector _goml_vec_Vec_3int
            values [2]*_goml_vec_int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t514)
    var t515 [2]int = [2]int{5, 6}
    var t516 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t515)
    var t517 Tuple2_6string_8Vec_3int = Tuple2_6string_8Vec_3int{
        _0: "values",
        _1: t516,
    }
    var t518 [1]Tuple2_6string_8Vec_3int = [1]Tuple2_6string_8Vec_3int{t517}
    var nested_map__24 *hashmap_string_Vec_3int_x = func(values [1]Tuple2_6string_8Vec_3int) *hashmap_string_Vec_3int_x {
        var result *hashmap_string_Vec_3int_x = hashmap_new__HashMap_6string_8Vec_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_8Vec_3int(result, entry._0, entry._1)
        }
        return result
    }(t518)
    var mtmp429 _goml_m_Option____Vec_l_int_r_
    var inline612 string = "values"
    var inline613 _goml_m_Option____Vec_l_int_r_ = hashmap_get__HashMap_6string_8Vec_3int(nested_map__24, inline612)
    mtmp429 = inline613
    var jp520 int
    switch mtmp429._tag {
    case 0:
        jp520 = 0
    case 1:
        var x430 *_goml_vec_int = mtmp429._v1_0
        var t533 int = vec_get__Vec_3int(x430, 0)
        jp520 = t533
    default:
        panic("non-exhaustive match")
    }
    var t521 Tuple2_3int_6string = vec_get__Vec_19Tuple2_3int_6string(pairs__22, 1)
    var t522 string = t521._1
    var t523 string = "" + t522
    var t524 string = t523 + ":"
    var t525 *_goml_vec_int = vec_get__Vec_8Vec_3int(nested__23, 1)
    var t526 int = vec_get__Vec_3int(t525, 0)
    var t527 string
    var inline610 string = _goml_runtime_core_int_to_string(t526)
    t527 = inline610
    var t528 string = t524 + t527
    var t529 string = t528 + ":"
    var t530 string
    var inline608 string = _goml_runtime_core_int_to_string(jp520)
    t530 = inline608
    var t531 string = t529 + t530
    var inline605 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t531)
    _goml_runtime_core_string_println(inline605)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t536 string = ref_get__Ref_6string(self__432)
    return t536
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t540 string
    t540 = value__1
    _goml_runtime_core_string_println(t540)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__273 *_goml_vec_int) int {
    var t544 int = vec_len__Vec_3int(self__273)
    return t544
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t547 string = _goml_runtime_core_int_to_string(self__32)
    return t547
}

func println__T_int(value__1 int) struct{} {
    var t549 string
    var inline660 string = _goml_runtime_core_int_to_string(value__1)
    t549 = inline660
    _goml_runtime_core_string_println(t549)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__431 int) *ref_int_x {
    var t553 *ref_int_x = ref__Ref_3int(value__431)
    return t553
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__433 *ref_int_x, value__434 int) struct{} {
    ref_set__Ref_3int(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__432 *ref_int_x) int {
    var t558 int = ref_get__Ref_3int(self__432)
    return t558
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t561 *ref_string_x = ref__Ref_6string(value__431)
    return t561
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t575 string = _goml_runtime_core_int_to_string(self__151)
    return t575
}

func main() {
    main0()
}
