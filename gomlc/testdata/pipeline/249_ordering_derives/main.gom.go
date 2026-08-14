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

func _goml_runtime_core_int_hash(x int) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int = 0
    for {
        if i >= int(len(s)) {
            break
        }
        h = h * 1099511628211 + uint64(s[i])
        i = i + 1
    }
    return h
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
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

type hashmap_Vec_3int_string_x_entry struct {
    active bool
    key *_goml_vec_int
    value string
}

type hashmap_Vec_3int_string_x struct {
    buckets map[uint64][]hashmap_Vec_3int_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_8Vec_3int_6string() *hashmap_Vec_3int_string_x {
    return &hashmap_Vec_3int_string_x{
        buckets: make(map[uint64][]hashmap_Vec_3int_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_8Vec_3int_6string(m *hashmap_Vec_3int_string_x, key *_goml_vec_int) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(key)
    var bucket []hashmap_Vec_3int_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Vec_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_8Vec_3int_6string(m *hashmap_Vec_3int_string_x, key *_goml_vec_int) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_8Vec_3int_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_8Vec_3int_6string(m *hashmap_Vec_3int_string_x, key *_goml_vec_int, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(key)
    var bucket []hashmap_Vec_3int_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Vec_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Vec_3int_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Vec_3int_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_Tuple2_3int_6string_string_x_entry struct {
    active bool
    key Tuple2_3int_6string
    value string
}

type hashmap_Tuple2_3int_6string_string_x struct {
    buckets map[uint64][]hashmap_Tuple2_3int_6string_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_19Tuple2_3int_6string_6string() *hashmap_Tuple2_3int_6string_string_x {
    return &hashmap_Tuple2_3int_6string_string_x{
        buckets: make(map[uint64][]hashmap_Tuple2_3int_6string_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_19Tuple2_3int_6string_6string(m *hashmap_Tuple2_3int_6string_string_x, key Tuple2_3int_6string) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i__o_int_c_string_q__i_hash(key)
    var bucket []hashmap_Tuple2_3int_6string_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Tuple2_3int_6string_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_19Tuple2_3int_6string_6string(m *hashmap_Tuple2_3int_6string_string_x, key Tuple2_3int_6string) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_19Tuple2_3int_6string_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_19Tuple2_3int_6string_6string(m *hashmap_Tuple2_3int_6string_string_x, key Tuple2_3int_6string, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i__o_int_c_string_q__i_hash(key)
    var bucket []hashmap_Tuple2_3int_6string_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Tuple2_3int_6string_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Tuple2_3int_6string_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Tuple2_3int_6string_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_Array_2_3int_string_x_entry struct {
    active bool
    key [2]int
    value string
}

type hashmap_Array_2_3int_string_x struct {
    buckets map[uint64][]hashmap_Array_2_3int_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_12Array_2_3int_6string() *hashmap_Array_2_3int_string_x {
    return &hashmap_Array_2_3int_string_x{
        buckets: make(map[uint64][]hashmap_Array_2_3int_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_12Array_2_3int_6string(m *hashmap_Array_2_3int_string_x, key [2]int) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i__l_int_x3b_2_r__i_hash(key)
    var bucket []hashmap_Array_2_3int_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Array_2_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_12Array_2_3int_6string(m *hashmap_Array_2_3int_string_x, key [2]int) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_12Array_2_3int_6string(m, key)
    if ok {
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
}

func hashmap_set__HashMap_12Array_2_3int_6string(m *hashmap_Array_2_3int_string_x, key [2]int, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i__l_int_x3b_2_r__i_hash(key)
    var bucket []hashmap_Array_2_3int_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Array_2_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Array_2_3int_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Array_2_3int_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3int_6string struct {
    _0 int
    _1 string
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Tuple2_7float64_7float64 struct {
    _0 float64
    _1 float64
}

type Version struct {
    major int
    minor int
}

type MaybeNumber struct {
    value float64
}

type NoTraits struct {}

type GenericPair__int struct {
    first int
    second int
    nested *_goml_vec_int
}

type _goml_m_std_p_cmp_p_Ordering int32

const (
    Less _goml_m_std_p_cmp_p_Ordering = 0
    Equal _goml_m_std_p_cmp_p_Ordering = 1
    Greater _goml_m_std_p_cmp_p_Ordering = 2
)

type Level interface {
    isLevel()
}

type Low struct {}

func (_ Low) isLevel() {}

type Medium struct {
    _0 int
    _1 int
}

func (_ Medium) isLevel() {}

type High struct {
    _0 int
    _1 int
}

func (_ High) isLevel() {}

type PartialLevel interface {
    isPartialLevel()
}

type Value struct {
    _0 float64
}

func (_ Value) isPartialLevel() {}

type Empty struct {}

func (_ Empty) isPartialLevel() {}

type _goml_m_Option____std_p_cmp_p_Ordering interface {
    is_goml_m_Option____std_p_cmp_p_Ordering()
}

type _goml_m_Option____std_p_cmp_p_Ordering_None struct {}

func (_ _goml_m_Option____std_p_cmp_p_Ordering_None) is_goml_m_Option____std_p_cmp_p_Ordering() {}

type _goml_m_Option____std_p_cmp_p_Ordering_Some struct {
    _0 _goml_m_std_p_cmp_p_Ordering
}

func (_ _goml_m_Option____std_p_cmp_p_Ordering_Some) is_goml_m_Option____std_p_cmp_p_Ordering() {}

type Phantom__NoTraits int32

const (
    First Phantom__NoTraits = 0
    Second Phantom__NoTraits = 1
)

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Result__int__string interface {
    isResult__int__string()
}

type Ok struct {
    _0 int
}

func (_ Ok) isResult__int__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int__string() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(self__13 _goml_m_std_p_cmp_p_Ordering, other__14 _goml_m_std_p_cmp_p_Ordering) bool {
    switch self__13 {
    case Less:
        switch other__14 {
        case Less:
            return true
        default:
            return false
        }
    case Equal:
        switch other__14 {
        case Equal:
            return true
        default:
            return false
        }
    case Greater:
        switch other__14 {
        case Greater:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__47 int, other__48 int) _goml_m_std_p_cmp_p_Ordering {
    var t549 bool = self__47 < other__48
    if t549 {
        return Less
    } else {
        var t552 bool = self__47 > other__48
        if t552 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) _goml_m_Option____std_p_cmp_p_Ordering {
    var t901 int = self__2.major
    var t902 int = other__3.major
    var commute_field3407 _goml_m_std_p_cmp_p_Ordering
    var inline2488 bool = t901 < t902
    var inline2490 _goml_m_std_p_cmp_p_Ordering
    if inline2488 {
        inline2490 = Less
    } else {
        var inline2492 bool = t901 > t902
        if inline2492 {
            inline2490 = Greater
        } else {
            inline2490 = Equal
        }
    }
    commute_field3407 = inline2490
    switch commute_field3407 {
    case Equal:
        var t907 int = self__2.minor
        var t908 int = other__3.minor
        var commute_field3404 _goml_m_std_p_cmp_p_Ordering
        var inline2482 bool = t907 < t908
        var inline2484 _goml_m_std_p_cmp_p_Ordering
        if inline2482 {
            inline2484 = Less
        } else {
            var inline2486 bool = t907 > t908
            if inline2486 {
                inline2484 = Greater
            } else {
                inline2484 = Equal
            }
        }
        commute_field3404 = inline2484
        switch commute_field3404 {
        case Equal:
            var t913 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t913
        default:
            var t914 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3404,
            }
            return t914
        }
    default:
        var t915 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3407,
        }
        return t915
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2494 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2494.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2495 _goml_m_std_p_cmp_p_Ordering = inline2494.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2497 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2495, Less)
        return inline2497
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) _goml_m_std_p_cmp_p_Ordering {
    var t930 int = self__6.major
    var t931 int = other__7.major
    var _goml_m__i_derive1__ordering____8 _goml_m_std_p_cmp_p_Ordering
    var inline2521 bool = t930 < t931
    if inline2521 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2522 bool = t930 > t931
        if inline2522 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t934 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t934 = false
    case Equal:
        t934 = true
    case Greater:
        t934 = false
    default:
        panic("non-exhaustive match")
    }
    if t934 {
        var t935 int = self__6.minor
        var t936 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 _goml_m_std_p_cmp_p_Ordering
        var inline2517 bool = t935 < t936
        if inline2517 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2518 bool = t935 > t936
            if inline2518 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t939 bool
        switch _goml_m__i_derive0__ordering____9 {
        case Less:
            t939 = false
        case Equal:
            t939 = true
        case Greater:
            t939 = false
        default:
            panic("non-exhaustive match")
        }
        if t939 {
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp975 int
    switch self__23.(type) {
    case Low:
        jp975 = 0
    case Medium:
        jp975 = 1
    case High:
        jp975 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp977 int
    switch other__24.(type) {
    case Low:
        jp977 = 0
    case Medium:
        jp977 = 1
    case High:
        jp977 = 2
    default:
        panic("non-exhaustive match")
    }
    var t980 bool = jp975 < jp977
    if t980 {
        var t981 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t981
    } else {
        var t984 bool = jp975 > jp977
        if t984 {
            var t985 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t985
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t990 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t990
                default:
                    var t991 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t991
                }
            case Medium:
                var x224 int = other__24.(Medium)._0
                var x225 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x232 int = self__23.(Medium)._0
                    var x233 int = self__23.(Medium)._1
                    var commute_field3413 _goml_m_std_p_cmp_p_Ordering
                    var inline2542 bool = x232 < x224
                    var inline2544 _goml_m_std_p_cmp_p_Ordering
                    if inline2542 {
                        inline2544 = Less
                    } else {
                        var inline2546 bool = x232 > x224
                        if inline2546 {
                            inline2544 = Greater
                        } else {
                            inline2544 = Equal
                        }
                    }
                    commute_field3413 = inline2544
                    switch commute_field3413 {
                    case Equal:
                        var commute_field3410 _goml_m_std_p_cmp_p_Ordering
                        var inline2536 bool = x233 < x225
                        var inline2538 _goml_m_std_p_cmp_p_Ordering
                        if inline2536 {
                            inline2538 = Less
                        } else {
                            var inline2540 bool = x233 > x225
                            if inline2540 {
                                inline2538 = Greater
                            } else {
                                inline2538 = Equal
                            }
                        }
                        commute_field3410 = inline2538
                        switch commute_field3410 {
                        case Equal:
                            var t1002 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1002
                        default:
                            var t1003 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3410,
                            }
                            return t1003
                        }
                    default:
                        var t1004 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3413,
                        }
                        return t1004
                    }
                default:
                    var t1005 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1005
                }
            case High:
                var x226 int = other__24.(High)._0
                var x227 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x242 int = self__23.(High)._0
                    var x243 int = self__23.(High)._1
                    var commute_field3419 _goml_m_std_p_cmp_p_Ordering
                    var inline2554 bool = x242 < x226
                    var inline2556 _goml_m_std_p_cmp_p_Ordering
                    if inline2554 {
                        inline2556 = Less
                    } else {
                        var inline2558 bool = x242 > x226
                        if inline2558 {
                            inline2556 = Greater
                        } else {
                            inline2556 = Equal
                        }
                    }
                    commute_field3419 = inline2556
                    switch commute_field3419 {
                    case Equal:
                        var commute_field3416 _goml_m_std_p_cmp_p_Ordering
                        var inline2548 bool = x243 < x227
                        var inline2550 _goml_m_std_p_cmp_p_Ordering
                        if inline2548 {
                            inline2550 = Less
                        } else {
                            var inline2552 bool = x243 > x227
                            if inline2552 {
                                inline2550 = Greater
                            } else {
                                inline2550 = Equal
                            }
                        }
                        commute_field3416 = inline2550
                        switch commute_field3416 {
                        case Equal:
                            var t1016 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1016
                        default:
                            var t1017 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3416,
                            }
                            return t1017
                        }
                    default:
                        var t1018 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3419,
                        }
                        return t1018
                    }
                default:
                    var t1019 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1019
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline2560 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2560.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2561 _goml_m_std_p_cmp_p_Ordering = inline2560.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2563 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2561, Less)
        return inline2563
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) _goml_m_std_p_cmp_p_Ordering {
    var jp1035 int
    switch self__39.(type) {
    case Low:
        jp1035 = 0
    case Medium:
        jp1035 = 1
    case High:
        jp1035 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1037 int
    switch other__40.(type) {
    case Low:
        jp1037 = 0
    case Medium:
        jp1037 = 1
    case High:
        jp1037 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1040 bool = jp1035 < jp1037
    if t1040 {
        return Less
    } else {
        var t1043 bool = jp1035 > jp1037
        if t1043 {
            return Greater
        } else {
            switch other__40.(type) {
            case Low:
                switch self__39.(type) {
                case Low:
                    return Equal
                default:
                    return Equal
                }
            case Medium:
                var x259 int = other__40.(Medium)._0
                var x260 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x267 int = self__39.(Medium)._0
                    var x268 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 _goml_m_std_p_cmp_p_Ordering
                    var inline2587 bool = x267 < x259
                    if inline2587 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2588 bool = x267 > x259
                        if inline2588 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1052 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1052 = false
                    case Equal:
                        t1052 = true
                    case Greater:
                        t1052 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1052 {
                        var _goml_m__i_derive4__ordering____48 _goml_m_std_p_cmp_p_Ordering
                        var inline2583 bool = x268 < x260
                        if inline2583 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2584 bool = x268 > x260
                            if inline2584 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1055 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1055 = false
                        case Equal:
                            t1055 = true
                        case Greater:
                            t1055 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1055 {
                            return Equal
                        } else {
                            return _goml_m__i_derive4__ordering____48
                        }
                    } else {
                        return _goml_m__i_derive7__ordering____47
                    }
                default:
                    return Equal
                }
            case High:
                var x261 int = other__40.(High)._0
                var x262 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x273 int = self__39.(High)._0
                    var x274 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x273, x261)
                    var t1060 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1060 {
                        var _goml_m__i_derive10__ordering____54 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x274, x262)
                        var t1063 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1063 = false
                        case Equal:
                            t1063 = true
                        case Greater:
                            t1063 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1063 {
                            return Equal
                        } else {
                            return _goml_m__i_derive10__ordering____54
                        }
                    } else {
                        return _goml_m__i_derive13__ordering____53
                    }
                default:
                    return Equal
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(self__60 MaybeNumber, other__61 MaybeNumber) bool {
    var t1086 float64 = self__60.value
    var t1087 float64 = other__61.value
    var inline2599 bool = t1086 == t1087
    return inline2599
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1091 float64 = self__62.value
    var t1092 float64 = other__63.value
    var commute_field3422 _goml_m_std_p_cmp_p_Ordering
    var inline2601 bool = t1091 < t1092
    if inline2601 {
        commute_field3422 = Less
        switch commute_field3422 {
        case Equal:
            var t1097 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1097
        default:
            var t1098 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3422,
            }
            return t1098
        }
    } else {
        var inline2603 bool = t1091 > t1092
        if inline2603 {
            commute_field3422 = Greater
            switch commute_field3422 {
            case Equal:
                var t1097 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                return t1097
            default:
                var t1098 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3422,
                }
                return t1098
            }
        } else {
            var inline2605 bool = t1091 == t1092
            if inline2605 {
                commute_field3422 = Equal
                switch commute_field3422 {
                case Equal:
                    var t1097 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1097
                default:
                    var t1098 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: commute_field3422,
                    }
                    return t1098
                }
            } else {
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1125 int
    switch self__95.(type) {
    case Value:
        jp1125 = 0
    case Empty:
        jp1125 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1127 int
    switch other__96.(type) {
    case Value:
        jp1127 = 0
    case Empty:
        jp1127 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1130 bool = jp1125 < jp1127
    if t1130 {
        var t1131 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1131
    } else {
        var t1134 bool = jp1125 > jp1127
        if t1134 {
            var t1135 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1135
        } else {
            switch other__96.(type) {
            case Value:
                var x311 float64 = other__96.(Value)._0
                switch self__95.(type) {
                case Value:
                    var x312 float64 = self__95.(Value)._0
                    var commute_field3425 _goml_m_std_p_cmp_p_Ordering
                    var inline2632 bool = x312 < x311
                    if inline2632 {
                        commute_field3425 = Less
                        switch commute_field3425 {
                        case Equal:
                            var t1144 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1144
                        default:
                            var t1145 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3425,
                            }
                            return t1145
                        }
                    } else {
                        var inline2634 bool = x312 > x311
                        if inline2634 {
                            commute_field3425 = Greater
                            switch commute_field3425 {
                            case Equal:
                                var t1144 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1144
                            default:
                                var t1145 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: commute_field3425,
                                }
                                return t1145
                            }
                        } else {
                            var inline2636 bool = x312 == x311
                            if inline2636 {
                                commute_field3425 = Equal
                                switch commute_field3425 {
                                case Equal:
                                    var t1144 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1144
                                default:
                                    var t1145 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: commute_field3425,
                                    }
                                    return t1145
                                }
                            } else {
                                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                            }
                        }
                    }
                default:
                    var t1146 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1146
                }
            case Empty:
                switch self__95.(type) {
                case Empty:
                    var t1149 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1149
                default:
                    var t1150 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1150
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func ordering_name(value__102 _goml_m_std_p_cmp_p_Ordering) string {
    switch value__102 {
    case Less:
        return "less"
    case Equal:
        return "equal"
    case Greater:
        return "greater"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var first__103 Version = Version{
        major: 1,
        minor: 9,
    }
    var second__104 Version = Version{
        major: 2,
        minor: 0,
    }
    var t1168 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1168)
    println__T_string(t1169)
    var t1170 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1171 string = ordering_name(t1170)
    println__T_string(t1171)
    var t1172 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1173 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1172)
    var t1174 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1173)
    println__T_string(t1174)
    var t1175 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1176 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1177 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1175, t1176)
    var t1178 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1177)
    println__T_string(t1178)
    var t1179 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1180 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1181 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1179, t1180)
    var t1182 string = ordering_name(t1181)
    println__T_string(t1182)
    var zero__105 float64 = 0
    var t1183 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1183,
    }
    var t1184 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1184)
    println__T_string(t1185)
    var t1186 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1187 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1186)
    var t1188 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1187)
    println__T_string(t1188)
    var vec_literal__1621 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1621, 3)
    var generic_first__107 GenericPair__int = GenericPair__int{
        first: 1,
        second: 2,
        nested: vec_literal__1621,
    }
    var vec_literal__1701 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1701, 0)
    var generic_second__108 GenericPair__int = GenericPair__int{
        first: 1,
        second: 3,
        nested: vec_literal__1701,
    }
    var t1189 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1190 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1189)
    println__T_string(t1190)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1191 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1192 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1191)
    println__T_string(t1192)
    var t1193 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = Value{
        _0: t1193,
    }
    var t1194 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1195 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1194)
    var t1196 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1195)
    println__T_string(t1196)
    var vec_literal__2131 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 2)
    var vec_literal__2178 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 3)
    var t1197 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(vec_literal__2131, vec_literal__2178)
    var t1198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1197)
    println__T_string(t1198)
    var t1199 Option__int = Option__int_Some{
        _0: 2,
    }
    var t1200 Option__int = Option__int_Some{
        _0: 3,
    }
    var t1201 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1199, t1200)
    var t1202 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1201)
    println__T_string(t1202)
    var ok__114 Result__int__string = Ok{
        _0: 1,
    }
    var error__115 Result__int__string = Err{
        _0: "error",
    }
    var t1203 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1203)
    println__T_string(t1204)
    var t1205 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2131, 0, 2)
    var t1206 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2178, 0, 2)
    var t1207 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1205, t1206)
    var t1208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1207)
    println__T_string(t1208)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, vec_literal__2131, "vector")
    var vec_literal__2661 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 2)
    var t1209 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, vec_literal__2661)
    var t1210 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1209, "missing")
    println__T_string(t1210)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default()
    var t1249 int = default_tuple__117._0
    var t1250 int = 0
    var t1251 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1249, t1250)
    var jp1212 bool
    if t1251 {
        var t1252 string = default_tuple__117._1
        var t1253 string = ""
        var inline2661 bool = t1252 == t1253
        jp1212 = inline2661
    } else {
        jp1212 = false
    }
    var t1213 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1212)
    println__T_string(t1213)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs344 [3]int = [3]int{0, 0, 0}
    var t1238 int = array_get__Array_3_3int(default_array__118, 0)
    var t1239 int = array_get__Array_3_3int(_eq_rhs344, 0)
    var t1240 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1238, t1239)
    var jp1215 bool
    if t1240 {
        var t1243 int = array_get__Array_3_3int(default_array__118, 1)
        var t1244 int = array_get__Array_3_3int(_eq_rhs344, 1)
        var t1245 bool
        var inline2665 bool = t1243 == t1244
        t1245 = inline2665
        if t1245 {
            var t1246 int = array_get__Array_3_3int(default_array__118, 2)
            var t1247 int = array_get__Array_3_3int(_eq_rhs344, 2)
            var inline2663 bool = t1246 == t1247
            jp1215 = inline2663
        } else {
            jp1215 = false
        }
    } else {
        jp1215 = false
    }
    var t1216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1215)
    println__T_string(t1216)
    var t1217 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1218 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1219 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(t1217, t1218)
    var t1220 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1219)
    var inline2706 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1220)
    _goml_runtime_core_string_println(inline2706)
    var t1221 [2]int = [2]int{1, 2}
    var t1222 [2]int = [2]int{1, 3}
    var t1223 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1221, t1222)
    var t1224 string = ordering_name(t1223)
    var inline2703 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1224)
    _goml_runtime_core_string_println(inline2703)
    var t1225 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_7float64_7float64 = Tuple2_7float64_7float64{
        _0: 0,
        _1: t1225,
    }
    var t1226 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1227 bool
    var inline2700 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t1226)
    var inline2701 bool = !inline2700
    t1227 = inline2701
    var t1228 string
    var inline2698 string = _goml_runtime_core_bool_to_string(t1227)
    t1228 = inline2698
    var inline2695 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1228)
    _goml_runtime_core_string_println(inline2695)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2693 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2693
    var t1229 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2690 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1229, inline2690)
    var t1230 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1231 Option__string
    var inline2688 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1230)
    t1231 = inline2688
    var t1232 string
    var inline2684 string = "missing"
    switch t1231.(type) {
    case Option__string_None:
        t1232 = inline2684
    case Option__string_Some:
        var inline2685 string = t1231.(Option__string_Some)._0
        t1232 = inline2685
    default:
        panic("non-exhaustive match")
    }
    var inline2681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1232)
    _goml_runtime_core_string_println(inline2681)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2679 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2679
    var t1233 [2]int = [2]int{1, 2}
    var inline2676 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1233, inline2676)
    var t1234 [2]int = [2]int{1, 2}
    var t1235 Option__string
    var inline2674 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1234)
    t1235 = inline2674
    var t1236 string
    var inline2670 string = "missing"
    switch t1235.(type) {
    case Option__string_None:
        t1236 = inline2670
    case Option__string_Some:
        var inline2671 string = t1235.(Option__string_Some)._0
        t1236 = inline2671
    default:
        panic("non-exhaustive match")
    }
    var inline2667 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1236)
    _goml_runtime_core_string_println(inline2667)
    return struct{}{}
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(self__15 int, other__16 int) bool {
    var commute_field3488 _goml_m_std_p_cmp_p_Ordering
    var inline2830 bool = self__15 < other__16
    var inline2832 _goml_m_std_p_cmp_p_Ordering
    if inline2830 {
        inline2832 = Less
    } else {
        var inline2834 bool = self__15 > other__16
        if inline2834 {
            inline2832 = Greater
        } else {
            inline2832 = Equal
        }
    }
    commute_field3488 = inline2832
    switch commute_field3488 {
    case Less:
        return true
    case Equal:
        return false
    case Greater:
        return false
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__101 int, other__102 int) bool {
    var t1631 bool = self__101 == other__102
    return t1631
}

func println__T_string(value__1 string) struct{} {
    var t1727 string
    t1727 = value__1
    _goml_runtime_core_string_println(t1727)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1731 string = _goml_runtime_core_bool_to_string(self__64)
    return t1731
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(self__299 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    var t1734 bool
    switch self__299.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        t1734 = false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        t1734 = true
    default:
        panic("non-exhaustive match")
    }
    var t1735 bool = !t1734
    return t1735
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1738 *_goml_vec_int = vec_new__Vec_3int()
    return t1738
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3225 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3225.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3226 _goml_m_std_p_cmp_p_Ordering = inline3225.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3228 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3226, Less)
        return inline3228
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3230 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3230.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3231 _goml_m_std_p_cmp_p_Ordering = inline3230.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3233 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3231, Less)
        return inline3233
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3235 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3235.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3236 _goml_m_std_p_cmp_p_Ordering = inline3235.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3238 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3236, Less)
        return inline3238
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3240 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3240.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3241 _goml_m_std_p_cmp_p_Ordering = inline3240.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3243 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3241, Less)
        return inline3243
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3245 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
    switch inline3245.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3246 _goml_m_std_p_cmp_p_Ordering = inline3245.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3248 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3246, Less)
        return inline3248
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__227 *_goml_vec_int, start__228 int, end__229 int) []int {
    var t1758 []int = self__227.items[start__228:end__229]
    return t1758
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3250 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3250.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3251 _goml_m_std_p_cmp_p_Ordering = inline3250.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3253 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3251, Less)
        return inline3253
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1764 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1764
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__264 *hashmap_Vec_3int_string_x, key__265 *_goml_vec_int, value__266 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__264, key__265, value__266)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__262 *hashmap_Vec_3int_string_x, key__263 *_goml_vec_int) Option__string {
    var t1769 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__262, key__263)
    return t1769
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__300 Option__string, fallback__301 string) string {
    switch self__300.(type) {
    case Option__string_None:
        return fallback__301
    case Option__string_Some:
        var x166 string = self__300.(Option__string_Some)._0
        return x166
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default() Tuple2_3int_6string {
    var t1776 int
    t1776 = 0
    var t1777 string
    t1777 = ""
    var t1778 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1776,
        _1: t1777,
    }
    return t1778
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1784 int
    t1784 = 0
    var t1785 int
    t1785 = 0
    var t1786 int
    t1786 = 0
    var t1787 [3]int = [3]int{t1784, t1785, t1786}
    return t1787
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1792 int = self._0
    var t1793 int = other._0
    var t1794 bool
    var inline3266 bool = t1792 == t1793
    t1794 = inline3266
    if t1794 {
        var t1797 int = self._1
        var t1798 int = other._1
        var t1799 bool
        var inline3262 bool = t1797 == t1798
        t1799 = inline3262
        if t1799 {
            return false
        } else {
            var t1800 int = self._1
            var t1801 int = other._1
            var inline3260 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1800, t1801)
            return inline3260
        }
    } else {
        var t1803 int = self._0
        var t1804 int = other._0
        var inline3264 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1803, t1804)
        return inline3264
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) _goml_m_std_p_cmp_p_Ordering {
    var t1808 int = array_get__Array_2_3int(self, 0)
    var t1809 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 _goml_m_std_p_cmp_p_Ordering
    var inline3273 bool = t1808 < t1809
    if inline3273 {
        _structural_ordering_0 = Less
    } else {
        var inline3274 bool = t1808 > t1809
        if inline3274 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1812 bool
    switch _structural_ordering_0 {
    case Less:
        t1812 = false
    case Equal:
        t1812 = true
    case Greater:
        t1812 = false
    default:
        panic("non-exhaustive match")
    }
    if t1812 {
        var t1813 int = array_get__Array_2_3int(self, 1)
        var t1814 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 _goml_m_std_p_cmp_p_Ordering
        var inline3269 bool = t1813 < t1814
        if inline3269 {
            _structural_ordering_1 = Less
        } else {
            var inline3270 bool = t1813 > t1814
            if inline3270 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1817 bool
        switch _structural_ordering_1 {
        case Less:
            t1817 = false
        case Equal:
            t1817 = true
        case Greater:
            t1817 = false
        default:
            panic("non-exhaustive match")
        }
        if t1817 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(self Tuple2_7float64_7float64, other Tuple2_7float64_7float64) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1820 float64 = self._0
    var t1821 float64 = other._0
    var _structural_partial_ordering_0 _goml_m_Option____std_p_cmp_p_Ordering
    var commute_field3647 _goml_m_std_p_cmp_p_Ordering
    var inline3285 bool = t1820 < t1821
    if inline3285 {
        var inline3286 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        _structural_partial_ordering_0 = inline3286
        commute_field3647 = Less
        var t1826 bool
        switch commute_field3647 {
        case Less:
            t1826 = false
        case Equal:
            t1826 = true
        case Greater:
            t1826 = false
        default:
            panic("non-exhaustive match")
        }
        if t1826 {
            var t1827 float64 = self._1
            var t1828 float64 = other._1
            var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
            var commute_field3644 _goml_m_std_p_cmp_p_Ordering
            var inline3277 bool = t1827 < t1828
            if inline3277 {
                var inline3278 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Less,
                }
                _structural_partial_ordering_1 = inline3278
                commute_field3644 = Less
                var t1833 bool
                switch commute_field3644 {
                case Less:
                    t1833 = false
                case Equal:
                    t1833 = true
                case Greater:
                    t1833 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1833 {
                    var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1834
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3279 bool = t1827 > t1828
                if inline3279 {
                    var inline3280 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Greater,
                    }
                    _structural_partial_ordering_1 = inline3280
                    commute_field3644 = Greater
                    var t1833 bool
                    switch commute_field3644 {
                    case Less:
                        t1833 = false
                    case Equal:
                        t1833 = true
                    case Greater:
                        t1833 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1833 {
                        var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1834
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3281 bool = t1827 == t1828
                    if inline3281 {
                        var inline3282 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        _structural_partial_ordering_1 = inline3282
                        commute_field3644 = Equal
                        var t1833 bool
                        switch commute_field3644 {
                        case Less:
                            t1833 = false
                        case Equal:
                            t1833 = true
                        case Greater:
                            t1833 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1833 {
                            var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1834
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                    }
                }
            }
        } else {
            return _structural_partial_ordering_0
        }
    } else {
        var inline3287 bool = t1820 > t1821
        if inline3287 {
            var inline3288 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            _structural_partial_ordering_0 = inline3288
            commute_field3647 = Greater
            var t1826 bool
            switch commute_field3647 {
            case Less:
                t1826 = false
            case Equal:
                t1826 = true
            case Greater:
                t1826 = false
            default:
                panic("non-exhaustive match")
            }
            if t1826 {
                var t1827 float64 = self._1
                var t1828 float64 = other._1
                var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                var commute_field3644 _goml_m_std_p_cmp_p_Ordering
                var inline3277 bool = t1827 < t1828
                if inline3277 {
                    var inline3278 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Less,
                    }
                    _structural_partial_ordering_1 = inline3278
                    commute_field3644 = Less
                    var t1833 bool
                    switch commute_field3644 {
                    case Less:
                        t1833 = false
                    case Equal:
                        t1833 = true
                    case Greater:
                        t1833 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1833 {
                        var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1834
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3279 bool = t1827 > t1828
                    if inline3279 {
                        var inline3280 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Greater,
                        }
                        _structural_partial_ordering_1 = inline3280
                        commute_field3644 = Greater
                        var t1833 bool
                        switch commute_field3644 {
                        case Less:
                            t1833 = false
                        case Equal:
                            t1833 = true
                        case Greater:
                            t1833 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1833 {
                            var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1834
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3281 bool = t1827 == t1828
                        if inline3281 {
                            var inline3282 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            _structural_partial_ordering_1 = inline3282
                            commute_field3644 = Equal
                            var t1833 bool
                            switch commute_field3644 {
                            case Less:
                                t1833 = false
                            case Equal:
                                t1833 = true
                            case Greater:
                                t1833 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1833 {
                                var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1834
                            } else {
                                return _structural_partial_ordering_1
                            }
                        } else {
                            return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                        }
                    }
                }
            } else {
                return _structural_partial_ordering_0
            }
        } else {
            var inline3289 bool = t1820 == t1821
            if inline3289 {
                var inline3290 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                _structural_partial_ordering_0 = inline3290
                commute_field3647 = Equal
                var t1826 bool
                switch commute_field3647 {
                case Less:
                    t1826 = false
                case Equal:
                    t1826 = true
                case Greater:
                    t1826 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1826 {
                    var t1827 float64 = self._1
                    var t1828 float64 = other._1
                    var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                    var commute_field3644 _goml_m_std_p_cmp_p_Ordering
                    var inline3277 bool = t1827 < t1828
                    if inline3277 {
                        var inline3278 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Less,
                        }
                        _structural_partial_ordering_1 = inline3278
                        commute_field3644 = Less
                        var t1833 bool
                        switch commute_field3644 {
                        case Less:
                            t1833 = false
                        case Equal:
                            t1833 = true
                        case Greater:
                            t1833 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1833 {
                            var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1834
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3279 bool = t1827 > t1828
                        if inline3279 {
                            var inline3280 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Greater,
                            }
                            _structural_partial_ordering_1 = inline3280
                            commute_field3644 = Greater
                            var t1833 bool
                            switch commute_field3644 {
                            case Less:
                                t1833 = false
                            case Equal:
                                t1833 = true
                            case Greater:
                                t1833 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1833 {
                                var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1834
                            } else {
                                return _structural_partial_ordering_1
                            }
                        } else {
                            var inline3281 bool = t1827 == t1828
                            if inline3281 {
                                var inline3282 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                _structural_partial_ordering_1 = inline3282
                                commute_field3644 = Equal
                                var t1833 bool
                                switch commute_field3644 {
                                case Less:
                                    t1833 = false
                                case Equal:
                                    t1833 = true
                                case Greater:
                                    t1833 = false
                                default:
                                    panic("non-exhaustive match")
                                }
                                if t1833 {
                                    var t1834 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1834
                                } else {
                                    return _structural_partial_ordering_1
                                }
                            } else {
                                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                            }
                        }
                    }
                } else {
                    return _structural_partial_ordering_0
                }
            } else {
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__298 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__298.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(self__67 GenericPair__int, other__68 GenericPair__int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1896 int = self__67.first
    var t1897 int = other__68.first
    var commute_field3653 _goml_m_std_p_cmp_p_Ordering
    var inline3304 bool = t1896 < t1897
    var inline3306 _goml_m_std_p_cmp_p_Ordering
    if inline3304 {
        inline3306 = Less
    } else {
        var inline3308 bool = t1896 > t1897
        if inline3308 {
            inline3306 = Greater
        } else {
            inline3306 = Equal
        }
    }
    commute_field3653 = inline3306
    switch commute_field3653 {
    case Equal:
        var t1902 int = self__67.second
        var t1903 int = other__68.second
        var commute_field3650 _goml_m_std_p_cmp_p_Ordering
        var inline3298 bool = t1902 < t1903
        var inline3300 _goml_m_std_p_cmp_p_Ordering
        if inline3298 {
            inline3300 = Less
        } else {
            var inline3302 bool = t1902 > t1903
            if inline3302 {
                inline3300 = Greater
            } else {
                inline3300 = Equal
            }
        }
        commute_field3650 = inline3300
        switch commute_field3650 {
        case Equal:
            var t1908 *_goml_vec_int = self__67.nested
            var t1909 *_goml_vec_int = other__68.nested
            var mtmp285 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t1908, t1909)
            switch mtmp285.(type) {
            case _goml_m_Option____std_p_cmp_p_Ordering_None:
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            case _goml_m_Option____std_p_cmp_p_Ordering_Some:
                var x286 _goml_m_std_p_cmp_p_Ordering = mtmp285.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
                switch x286 {
                case Equal:
                    var t1914 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1914
                default:
                    var t1915 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: x286,
                    }
                    return t1915
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1916 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3650,
            }
            return t1916
        }
    default:
        var t1917 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3653,
        }
        return t1917
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1921 int
    switch self__83 {
    case First:
        jp1921 = 0
    case Second:
        jp1921 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1923 int
    switch other__84 {
    case First:
        jp1923 = 0
    case Second:
        jp1923 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1926 bool = jp1921 < jp1923
    if t1926 {
        var t1927 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1927
    } else {
        var t1930 bool = jp1921 > jp1923
        if t1930 {
            var t1931 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1931
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t1936 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1936
                default:
                    var t1937 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1937
                }
            case Second:
                switch self__83 {
                case Second:
                    var t1940 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1940
                default:
                    var t1941 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1941
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__91 *_goml_vec_int, other__92 *_goml_vec_int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1959 int
    var inline3332 int = vec_len__Vec_3int(self__91)
    t1959 = inline3332
    var t1960 int
    var inline3330 int = vec_len__Vec_3int(other__92)
    t1960 = inline3330
    var t1961 bool = t1959 < t1960
    var jp1945 int
    if t1961 {
        var inline3310 int = vec_len__Vec_3int(self__91)
        jp1945 = inline3310
    } else {
        var inline3312 int = vec_len__Vec_3int(other__92)
        jp1945 = inline3312
    }
    var index__94 int = 0
    Loop_loop1950:
    for {
        var t1951 bool = index__94 < jp1945
        if t1951 {
            var t1952 int = vec_get__Vec_3int(self__91, index__94)
            var t1953 int = vec_get__Vec_3int(other__92, index__94)
            var commute_field3656 _goml_m_std_p_cmp_p_Ordering
            var inline3314 bool = t1952 < t1953
            var inline3316 _goml_m_std_p_cmp_p_Ordering
            if inline3314 {
                inline3316 = Less
            } else {
                var inline3318 bool = t1952 > t1953
                if inline3318 {
                    inline3316 = Greater
                } else {
                    inline3316 = Equal
                }
            }
            commute_field3656 = inline3316
            switch commute_field3656 {
            case Equal:
                var compound_old10 int = index__94
                var compound_value11 int = 1
                var t1956 int = compound_old10 + compound_value11
                index__94 = t1956
                continue
            default:
                var t1958 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3656,
                }
                return t1958
            }
        } else {
            break Loop_loop1950
        }
    }
    var t1947 int
    var inline3328 int = vec_len__Vec_3int(self__91)
    t1947 = inline3328
    var t1948 int
    var inline3326 int = vec_len__Vec_3int(other__92)
    t1948 = inline3326
    var inline3320 bool = t1947 < t1948
    var inline3322 _goml_m_std_p_cmp_p_Ordering
    if inline3320 {
        inline3322 = Less
    } else {
        var inline3324 bool = t1947 > t1948
        if inline3324 {
            inline3322 = Greater
        } else {
            inline3322 = Equal
        }
    }
    var inline3323 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3322,
    }
    return inline3323
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__111 Option__int, other__112 Option__int) _goml_m_Option____std_p_cmp_p_Ordering {
    switch other__112.(type) {
    case Option__int_None:
        switch self__111.(type) {
        case Option__int_None:
            var t1970 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1970
        case Option__int_Some:
            var t1971 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1971
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__112.(Option__int_Some)._0
        switch self__111.(type) {
        case Option__int_None:
            var t1974 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1974
        case Option__int_Some:
            var x35 int = self__111.(Option__int_Some)._0
            var inline3334 bool = x35 < x33
            var inline3336 _goml_m_std_p_cmp_p_Ordering
            if inline3334 {
                inline3336 = Less
            } else {
                var inline3338 bool = x35 > x33
                if inline3338 {
                    inline3336 = Greater
                } else {
                    inline3336 = Equal
                }
            }
            var inline3337 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3336,
            }
            return inline3337
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(self__119 Result__int__string, other__120 Result__int__string) _goml_m_Option____std_p_cmp_p_Ordering {
    switch other__120.(type) {
    case Ok:
        var x45 int = other__120.(Ok)._0
        switch self__119.(type) {
        case Ok:
            var x47 int = self__119.(Ok)._0
            var inline3340 bool = x47 < x45
            var inline3342 _goml_m_std_p_cmp_p_Ordering
            if inline3340 {
                inline3342 = Less
            } else {
                var inline3344 bool = x47 > x45
                if inline3344 {
                    inline3342 = Greater
                } else {
                    inline3342 = Equal
                }
            }
            var inline3343 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3342,
            }
            return inline3343
        case Err:
            var t1983 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1983
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__120.(Err)._0
        switch self__119.(type) {
        case Ok:
            var t1986 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1986
        case Err:
            var x50 string = self__119.(Err)._0
            var inline3346 bool = x50 < x46
            var inline3348 _goml_m_std_p_cmp_p_Ordering
            if inline3346 {
                inline3348 = Less
            } else {
                var inline3350 bool = x50 > x46
                if inline3350 {
                    inline3348 = Greater
                } else {
                    inline3348 = Equal
                }
            }
            var inline3349 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3348,
            }
            return inline3349
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__101 []int, other__102 []int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t2005 int
    var inline3374 int = len(self__101)
    t2005 = inline3374
    var t2006 int
    var inline3372 int = len(other__102)
    t2006 = inline3372
    var t2007 bool = t2005 < t2006
    var jp1991 int
    if t2007 {
        var inline3352 int = len(self__101)
        jp1991 = inline3352
    } else {
        var inline3354 int = len(other__102)
        jp1991 = inline3354
    }
    var index__104 int = 0
    Loop_loop1996:
    for {
        var t1997 bool = index__104 < jp1991
        if t1997 {
            var t1998 int = self__101[index__104]
            var t1999 int = other__102[index__104]
            var commute_field3659 _goml_m_std_p_cmp_p_Ordering
            var inline3356 bool = t1998 < t1999
            var inline3358 _goml_m_std_p_cmp_p_Ordering
            if inline3356 {
                inline3358 = Less
            } else {
                var inline3360 bool = t1998 > t1999
                if inline3360 {
                    inline3358 = Greater
                } else {
                    inline3358 = Equal
                }
            }
            commute_field3659 = inline3358
            switch commute_field3659 {
            case Equal:
                var compound_old21 int = index__104
                var compound_value22 int = 1
                var t2002 int = compound_old21 + compound_value22
                index__104 = t2002
                continue
            default:
                var t2004 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3659,
                }
                return t2004
            }
        } else {
            break Loop_loop1996
        }
    }
    var t1993 int
    var inline3370 int = len(self__101)
    t1993 = inline3370
    var t1994 int
    var inline3368 int = len(other__102)
    t1994 = inline3368
    var inline3362 bool = t1993 < t1994
    var inline3364 _goml_m_std_p_cmp_p_Ordering
    if inline3362 {
        inline3364 = Less
    } else {
        var inline3366 bool = t1993 > t1994
        if inline3366 {
            inline3364 = Greater
        } else {
            inline3364 = Equal
        }
    }
    var inline3365 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3364,
    }
    return inline3365
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__139 *_goml_vec_int, other__140 *_goml_vec_int) bool {
    var t2020 int
    var inline3382 int = vec_len__Vec_3int(self__139)
    t2020 = inline3382
    var t2021 int
    var inline3380 int = vec_len__Vec_3int(other__140)
    t2021 = inline3380
    var t2022 bool = t2020 != t2021
    if t2022 {
        return false
    } else {
        var index__141 int = 0
        Loop_loop2024:
        for {
            var t2025 int
            var inline3378 int = vec_len__Vec_3int(self__139)
            t2025 = inline3378
            var t2026 bool = index__141 < t2025
            if t2026 {
                var t2028 int = vec_get__Vec_3int(self__139, index__141)
                var t2029 int = vec_get__Vec_3int(other__140, index__141)
                var t2030 bool
                var inline3376 bool = t2028 == t2029
                t2030 = inline3376
                if t2030 {
                    var compound_old43 int = index__141
                    var compound_value44 int = 1
                    var t2031 int = compound_old43 + compound_value44
                    index__141 = t2031
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2024
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__142 *_goml_vec_int) uint64 {
    var value__143 uint64 = 14695981039346656037
    var index__144 int = 0
    Loop_loop2036:
    for {
        var t2037 int
        var inline3386 int = vec_len__Vec_3int(self__142)
        t2037 = inline3386
        var t2038 bool = index__144 < t2037
        if t2038 {
            var t2039 uint64 = value__143 * 1099511628211
            var t2040 int = vec_get__Vec_3int(self__142, index__144)
            var t2041 uint64
            var inline3384 uint64 = _goml_runtime_core_int_hash(t2040)
            t2041 = inline3384
            var t2042 uint64 = t2039 + t2041
            value__143 = t2042
            var compound_old48 int = index__144
            var compound_value49 int = 1
            var t2043 int = compound_old48 + compound_value49
            index__144 = t2043
            continue
        } else {
            break Loop_loop2036
        }
    }
    return value__143
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2049 int = self._0
    var t2050 int = other._0
    var t2051 bool
    var inline3390 bool = t2049 == t2050
    t2051 = inline3390
    if t2051 {
        var t2054 string = self._1
        var t2055 string = other._1
        var t2056 bool
        var inline3388 bool = t2054 == t2055
        t2056 = inline3388
        if t2056 {
            return true
        } else {
            return false
        }
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i__o_int_c_string_q__i_hash(self Tuple2_3int_6string) uint64 {
    var _structural_hash_0 uint64 = 14695981039346656037
    var t2059 uint64 = _structural_hash_0 * 1099511628211
    var t2060 int = self._0
    var t2061 uint64
    var inline3394 uint64 = _goml_runtime_core_int_hash(t2060)
    t2061 = inline3394
    var _structural_hash_1 uint64 = t2059 + t2061
    var t2062 uint64 = _structural_hash_1 * 1099511628211
    var t2063 string = self._1
    var t2064 uint64
    var inline3392 uint64 = _goml_runtime_core_string_hash(t2063)
    t2064 = inline3392
    var _structural_hash_2 uint64 = t2062 + t2064
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2069 int = array_get__Array_2_3int(self, 0)
    var t2070 int = array_get__Array_2_3int(other, 0)
    var t2071 bool
    var inline3398 bool = t2069 == t2070
    t2071 = inline3398
    if t2071 {
        var t2074 int = array_get__Array_2_3int(self, 1)
        var t2075 int = array_get__Array_2_3int(other, 1)
        var t2076 bool
        var inline3396 bool = t2074 == t2075
        t2076 = inline3396
        if t2076 {
            return true
        } else {
            return false
        }
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i__l_int_x3b_2_r__i_hash(self [2]int) uint64 {
    var _structural_hash_0 uint64 = 14695981039346656037
    var t2079 uint64 = _structural_hash_0 * 1099511628211
    var t2080 int = array_get__Array_2_3int(self, 0)
    var t2081 uint64
    var inline3402 uint64 = _goml_runtime_core_int_hash(t2080)
    t2081 = inline3402
    var _structural_hash_1 uint64 = t2079 + t2081
    var t2082 uint64 = _structural_hash_1 * 1099511628211
    var t2083 int = array_get__Array_2_3int(self, 1)
    var t2084 uint64
    var inline3400 uint64 = _goml_runtime_core_int_hash(t2083)
    t2084 = inline3400
    var _structural_hash_2 uint64 = t2082 + t2084
    return _structural_hash_2
}

func main() {
    main0()
}
