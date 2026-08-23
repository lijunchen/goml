package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
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

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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

func _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(self__0 LoggedKey, other__0 LoggedKey) bool {
    var t0 *ref_string_x = self__0.log
    var t1 *ref_string_x = self__0.log
    var t2 string
    var inline1 string = ref_get__Ref_6string(t1)
    t2 = inline1
    var t3 string = t2 + "E"
    ref_set__Ref_6string(t0, t3)
    var t4 int = self__0.id
    var t5 int = other__0.id
    var t6 bool = t4 == t5
    return t6
}

func _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(self__0 LoggedKey) uint64 {
    var t0 *ref_string_x = self__0.log
    var t1 *ref_string_x = self__0.log
    var t2 string
    var inline1 string = ref_get__Ref_6string(t1)
    t2 = inline1
    var t3 string = t2 + "H"
    ref_set__Ref_6string(t0, t3)
    var t4 int = self__0.id
    var t5 uint64 = uint64(int(t4))
    return t5
}

func logged_key(log__0 *ref_string_x, label__0 string, id__0 int) LoggedKey {
    var t0 string
    var inline1 string = ref_get__Ref_6string(log__0)
    t0 = inline1
    var t1 string = t0 + label__0
    ref_set__Ref_6string(log__0, t1)
    var t2 LoggedKey = LoggedKey{
        id: id__0,
        log: log__0,
    }
    return t2
}

func logged_value(log__0 *ref_string_x, label__0 string, value__0 int) int {
    var t0 string
    var inline1 string = ref_get__Ref_6string(log__0)
    t0 = inline1
    var t1 string = t0 + label__0
    ref_set__Ref_6string(log__0, t1)
    return value__0
}

func main0() struct{} {
    var make_vec__0 func([3]int) *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }
    var t0 [3]int = [3]int{1, 2, 3}
    var values__0 *_goml_vec_int = make_vec__0(t0)
    var t1 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(values__0)
    var t2 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1)
    var t3 string = "" + t2
    var t4 string = t3 + ":"
    var t5 int = vec_get__Vec_3int(values__0, 0)
    var t6 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t5)
    var t7 string = t4 + t6
    var t8 string = t7 + ":"
    var t9 int = vec_get__Vec_3int(values__0, 2)
    var t10 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t9)
    var t11 string = t8 + t10
    println__T_string(t11)
    var empty__0 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t12 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(empty__0)
    println__T_isize(t12)
    var inferred_empty__0 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t13 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inferred_empty__0)
    println__T_isize(t13)
    var t14 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(1)
    var t15 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(2)
    var source__0 [2]*ref_int_x = [2]*ref_int_x{t14, t15}
    var copied__0 *_goml_vec_Ref_3int = func(values [2]*ref_int_x) *_goml_vec_Ref_3int {
        var storage struct {
            vector _goml_vec_Ref_3int
            values [2]*ref_int_x
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(source__0)
    var t16 *ref_int_x = array_get__Array_2_8Ref_3int(source__0, 0)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t16, 5)
    var place_root0 [2]*ref_int_x = source__0
    var index0 int = 0
    array_get__Array_2_8Ref_3int(place_root0, index0)
    var value0 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(9)
    var t17 [2]*ref_int_x = array_set__Array_2_8Ref_3int(place_root0, index0, value0)
    source__0 = t17
    var t19 *ref_int_x = vec_get__Vec_8Ref_3int(copied__0, 0)
    var t20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t19)
    var t21 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t20)
    var t22 string = "" + t21
    var t23 string = t22 + ":"
    var t24 *ref_int_x = array_get__Array_2_8Ref_3int(source__0, 0)
    var t25 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t24)
    var t26 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t25)
    var t27 string = t23 + t26
    println__T_string(t27)
    var log__0 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var t28 LoggedKey = logged_key(log__0, "A", 1)
    var t29 int = logged_value(log__0, "a", 10)
    var t30 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t28,
        _1: t29,
    }
    var t31 LoggedKey
    var inline28 string = "B"
    var inline29 int = 1
    var inline30 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__0)
    var inline31 string = inline30 + inline28
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__0, inline31)
    var inline33 LoggedKey = LoggedKey{
        id: inline29,
        log: log__0,
    }
    t31 = inline33
    var t32 int
    var inline23 string = "b"
    var inline24 int = 20
    var inline25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__0)
    var inline26 string = inline25 + inline23
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__0, inline26)
    t32 = inline24
    var t33 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t31,
        _1: t32,
    }
    var table__0 *hashmap_LoggedKey_int_x = &hashmap_LoggedKey_int_x{
        buckets: make(map[uint64][]hashmap_LoggedKey_int_x_entry, 2),
        hashes: make([]uint64, 0, 2),
        len: 0,
    }
    hashmap_set__HashMap_9LoggedKey_3int(table__0, t30._0, t30._1)
    hashmap_set__HashMap_9LoggedKey_3int(table__0, t33._0, t33._1)
    var t35 string
    var inline22 string = ref_get__Ref_6string(log__0)
    t35 = inline22
    var inline20 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t35)
    _goml_runtime_core_string_println(inline20)
    var t36 LoggedKey = LoggedKey{
        id: 1,
        log: log__0,
    }
    var mtmp0 Option__isize
    var inline19 Option__isize = hashmap_get__HashMap_9LoggedKey_3int(table__0, t36)
    mtmp0 = inline19
    var jp0 int
    switch mtmp0._tag {
    case 0:
        jp0 = 0
    case 1:
        var x1 int = mtmp0._v1_0
        jp0 = x1
    default:
        panic("non-exhaustive match")
    }
    var inline17 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp0)
    _goml_runtime_core_string_println(inline17)
    var make_map__0 func([2]Tuple2_6string_3int) *hashmap_string_int_x = func(values [2]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }
    var t37 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "a",
        _1: 1,
    }
    var t38 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "b",
        _1: 2,
    }
    var t39 [2]Tuple2_6string_3int = [2]Tuple2_6string_3int{t37, t38}
    var words__0 *hashmap_string_int_x = make_map__0(t39)
    var t40 int
    var inline16 int = hashmap_len__HashMap_6string_3int(words__0)
    t40 = inline16
    var inline14 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t40)
    _goml_runtime_core_string_println(inline14)
    var no_words__0 *hashmap_string_int_x
    var inline13 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    no_words__0 = inline13
    var t41 int
    var inline12 int = hashmap_len__HashMap_6string_3int(no_words__0)
    t41 = inline12
    var inline10 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t41)
    _goml_runtime_core_string_println(inline10)
    var inferred_no_words__0 *hashmap_string_int_x
    var inline9 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    inferred_no_words__0 = inline9
    var t42 int
    var inline8 int = hashmap_len__HashMap_6string_3int(inferred_no_words__0)
    t42 = inline8
    var inline6 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t42)
    _goml_runtime_core_string_println(inline6)
    var t43 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "a",
    }
    var t44 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 2,
        _1: "b",
    }
    var t45 [2]Tuple2_3int_6string = [2]Tuple2_3int_6string{t43, t44}
    var pairs__0 *_goml_vec_Tuple2_3int_6string = func(values [2]Tuple2_3int_6string) *_goml_vec_Tuple2_3int_6string {
        var storage struct {
            vector _goml_vec_Tuple2_3int_6string
            values [2]Tuple2_3int_6string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t45)
    var t46 [2]int = [2]int{1, 2}
    var t47 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t46)
    var t48 [2]int = [2]int{3, 4}
    var t49 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t48)
    var t50 [2]*_goml_vec_int = [2]*_goml_vec_int{t47, t49}
    var nested__0 *_goml_vec_Vec_3int = func(values [2]*_goml_vec_int) *_goml_vec_Vec_3int {
        var storage struct {
            vector _goml_vec_Vec_3int
            values [2]*_goml_vec_int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t50)
    var t51 [2]int = [2]int{5, 6}
    var t52 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t51)
    var t53 Tuple2_6string_8Vec_3int = Tuple2_6string_8Vec_3int{
        _0: "values",
        _1: t52,
    }
    var nested_map__0 *hashmap_string_Vec_3int_x = &hashmap_string_Vec_3int_x{
        indices: make(map[string]int, 1),
        entries: make([]hashmap_string_Vec_3int_x_entry, 0, 1),
        len: 0,
    }
    hashmap_set__HashMap_6string_8Vec_3int(nested_map__0, t53._0, t53._1)
    var mtmp1 _goml_m_Option____Vec_l_isize_r_
    var inline4 string = "values"
    var inline5 _goml_m_Option____Vec_l_isize_r_ = hashmap_get__HashMap_6string_8Vec_3int(nested_map__0, inline4)
    mtmp1 = inline5
    var jp1 int
    switch mtmp1._tag {
    case 0:
        jp1 = 0
    case 1:
        var x0 *_goml_vec_int = mtmp1._v1_0
        var t66 int = vec_get__Vec_3int(x0, 0)
        jp1 = t66
    default:
        panic("non-exhaustive match")
    }
    var t55 Tuple2_3int_6string = vec_get__Vec_19Tuple2_3int_6string(pairs__0, 1)
    var t56 string = t55._1
    var t57 string = "" + t56
    var t58 string = t57 + ":"
    var t59 *_goml_vec_int = vec_get__Vec_8Vec_3int(nested__0, 1)
    var t60 int = vec_get__Vec_3int(t59, 0)
    var t61 string
    var inline3 string = __goml_builtin_int_to_string(t60)
    t61 = inline3
    var t62 string = t58 + t61
    var t63 string = t62 + ":"
    var t64 string
    var inline2 string = __goml_builtin_int_to_string(jp1)
    t64 = inline2
    var t65 string = t63 + t64
    var inline0 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t65)
    _goml_runtime_core_string_println(inline0)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__0 *ref_string_x) string {
    var t0 string = ref_get__Ref_6string(self__0)
    return t0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__0 *ref_string_x, value__0 string) struct{} {
    ref_set__Ref_6string(self__0, value__0)
    return struct{}{}
}

func println__T_string(value__0 string) struct{} {
    var t0 string
    t0 = value__0
    _goml_runtime_core_string_println(t0)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__0 *_goml_vec_int) int {
    var t0 int = vec_len__Vec_3int(self__0)
    return t0
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__0 int) string {
    var inline0 int64 = int64(int(self__0))
    var inline1 string = signed_decimal_string(inline0)
    return inline1
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t0 *_goml_vec_int = vec_new__Vec_3int()
    return t0
}

func println__T_isize(value__0 int) struct{} {
    var t0 string
    var inline0 string = __goml_builtin_int_to_string(value__0)
    t0 = inline0
    _goml_runtime_core_string_println(t0)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__0 int) *ref_int_x {
    var t0 *ref_int_x = ref__Ref_3int(value__0)
    return t0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__0 *ref_int_x, value__0 int) struct{} {
    ref_set__Ref_3int(self__0, value__0)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__0 *ref_int_x) int {
    var t0 int = ref_get__Ref_3int(self__0)
    return t0
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__0 string) *ref_string_x {
    var t0 *ref_string_x = ref__Ref_6string(value__0)
    return t0
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__0 string) string {
    return self__0
}

func __goml_builtin_int_to_string(value__0 int) string {
    var t0 int64 = int64(int(value__0))
    var inline0 bool = t0 < 0
    if inline0 {
        var inline1 uint64 = uint64(int64(t0))
        var inline2 uint64 = 0 - inline1
        var inline3 string = decimal_string(inline2)
        var inline4 string = "-" + inline3
        return inline4
    } else {
        var inline5 uint64 = uint64(int64(t0))
        var inline6 string = decimal_string(inline5)
        return inline6
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__0 int) string {
    var inline0 int64 = int64(int(self__0))
    var inline1 string = signed_decimal_string(inline0)
    return inline1
}

func signed_decimal_string(value__0 int64) string {
    var t0 bool = value__0 < 0
    if t0 {
        var t1 uint64 = uint64(int64(value__0))
        var t2 uint64 = 0 - t1
        var t3 string = decimal_string(t2)
        var t4 string = "-" + t3
        return t4
    } else {
        var t5 uint64 = uint64(int64(value__0))
        var t6 string = decimal_string(t5)
        return t6
    }
}

func decimal_string(value__0 uint64) string {
    var t0 bool = value__0 == 0
    if t0 {
        return "0"
    } else {
        var reversed__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__0 uint64 = value__0
        Loop_loop0:
        for {
            var t10 bool = remaining__0 > 0
            if t10 {
                var t11_rhs uint64 = 10
                var t11 uint64 = remaining__0 % t11_rhs
                var t12 uint8 = uint8(uint64(t11))
                var t13 uint8 = t12 + 48
                vec_push__Vec_5uint8(reversed__0, t13)
                var compound_old1 uint64 = remaining__0
                var compound_value1 uint64 = 10
                var t14 uint64 = compound_old1 / compound_value1
                remaining__0 = t14
                continue
            } else {
                break Loop_loop0
            }
        }
        var t1 int
        var inline3 int = vec_len__Vec_5uint8(reversed__0)
        t1 = inline3
        var bytes__0 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1)
        var offset__0 int = 0
        Loop_loop1:
        for {
            var t2 int
            var inline2 int = vec_len__Vec_5uint8(reversed__0)
            t2 = inline2
            var t3 bool = offset__0 < t2
            if t3 {
                var t4 int
                var inline1 int = vec_len__Vec_5uint8(reversed__0)
                t4 = inline1
                var t5 int = t4 - offset__0
                var t6 int = t5 - 1
                var t7 uint8 = vec_get__Vec_5uint8(reversed__0, t6)
                vec_push__Vec_5uint8(bytes__0, t7)
                var compound_old0 int = offset__0
                var compound_value0 int = 1
                var t8 int = compound_old0 + compound_value0
                offset__0 = t8
                continue
            } else {
                break Loop_loop1
            }
        }
        var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__0)
        var x0 string = mtmp0._1
        return x0
    }
}

func main() {
    main0()
}
