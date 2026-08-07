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

type Tuple2_5Level_5Level struct {
    _0 Level
    _1 Level
}

type Tuple2_12PartialLevel_12PartialLevel struct {
    _0 PartialLevel
    _1 PartialLevel
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

type Tuple2_17Phantom__NoTraits_17Phantom__NoTraits struct {
    _0 Phantom__NoTraits
    _1 Phantom__NoTraits
}

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_19Result__int__string_19Result__int__string struct {
    _0 Result__int__string
    _1 Result__int__string
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
    var t534 bool = self__47 < other__48
    if t534 {
        return Less
    } else {
        var t537 bool = self__47 > other__48
        if t537 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) _goml_m_Option____std_p_cmp_p_Ordering {
    var t886 int = self__2.major
    var t887 int = other__3.major
    var commute_field3410 _goml_m_std_p_cmp_p_Ordering
    var inline2488 bool = t886 < t887
    var inline2490 _goml_m_std_p_cmp_p_Ordering
    if inline2488 {
        inline2490 = Less
    } else {
        var inline2492 bool = t886 > t887
        if inline2492 {
            inline2490 = Greater
        } else {
            inline2490 = Equal
        }
    }
    commute_field3410 = inline2490
    switch commute_field3410 {
    case Equal:
        var t892 int = self__2.minor
        var t893 int = other__3.minor
        var commute_field3407 _goml_m_std_p_cmp_p_Ordering
        var inline2482 bool = t892 < t893
        var inline2484 _goml_m_std_p_cmp_p_Ordering
        if inline2482 {
            inline2484 = Less
        } else {
            var inline2486 bool = t892 > t893
            if inline2486 {
                inline2484 = Greater
            } else {
                inline2484 = Equal
            }
        }
        commute_field3407 = inline2484
        switch commute_field3407 {
        case Equal:
            var t898 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t898
        default:
            var t899 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3407,
            }
            return t899
        }
    default:
        var t900 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3410,
        }
        return t900
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
    var t915 int = self__6.major
    var t916 int = other__7.major
    var _goml_m__i_derive1__ordering____8 _goml_m_std_p_cmp_p_Ordering
    var inline2521 bool = t915 < t916
    if inline2521 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2522 bool = t915 > t916
        if inline2522 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t919 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t919 = false
    case Equal:
        t919 = true
    case Greater:
        t919 = false
    default:
        panic("non-exhaustive match")
    }
    if t919 {
        var t920 int = self__6.minor
        var t921 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 _goml_m_std_p_cmp_p_Ordering
        var inline2517 bool = t920 < t921
        if inline2517 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2518 bool = t920 > t921
            if inline2518 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t924 bool
        switch _goml_m__i_derive0__ordering____9 {
        case Less:
            t924 = false
        case Equal:
            t924 = true
        case Greater:
            t924 = false
        default:
            panic("non-exhaustive match")
        }
        if t924 {
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp960 int
    switch self__23.(type) {
    case Low:
        jp960 = 0
    case Medium:
        jp960 = 1
    case High:
        jp960 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp962 int
    switch other__24.(type) {
    case Low:
        jp962 = 0
    case Medium:
        jp962 = 1
    case High:
        jp962 = 2
    default:
        panic("non-exhaustive match")
    }
    var t965 bool = jp960 < jp962
    if t965 {
        var t966 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t966
    } else {
        var t969 bool = jp960 > jp962
        if t969 {
            var t970 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t970
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t975 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t975
                default:
                    var t976 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t976
                }
            case Medium:
                var x209 int = other__24.(Medium)._0
                var x210 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x217 int = self__23.(Medium)._0
                    var x218 int = self__23.(Medium)._1
                    var commute_field3416 _goml_m_std_p_cmp_p_Ordering
                    var inline2542 bool = x217 < x209
                    var inline2544 _goml_m_std_p_cmp_p_Ordering
                    if inline2542 {
                        inline2544 = Less
                    } else {
                        var inline2546 bool = x217 > x209
                        if inline2546 {
                            inline2544 = Greater
                        } else {
                            inline2544 = Equal
                        }
                    }
                    commute_field3416 = inline2544
                    switch commute_field3416 {
                    case Equal:
                        var commute_field3413 _goml_m_std_p_cmp_p_Ordering
                        var inline2536 bool = x218 < x210
                        var inline2538 _goml_m_std_p_cmp_p_Ordering
                        if inline2536 {
                            inline2538 = Less
                        } else {
                            var inline2540 bool = x218 > x210
                            if inline2540 {
                                inline2538 = Greater
                            } else {
                                inline2538 = Equal
                            }
                        }
                        commute_field3413 = inline2538
                        switch commute_field3413 {
                        case Equal:
                            var t987 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t987
                        default:
                            var t988 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3413,
                            }
                            return t988
                        }
                    default:
                        var t989 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3416,
                        }
                        return t989
                    }
                default:
                    var t990 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t990
                }
            case High:
                var x211 int = other__24.(High)._0
                var x212 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x227 int = self__23.(High)._0
                    var x228 int = self__23.(High)._1
                    var commute_field3422 _goml_m_std_p_cmp_p_Ordering
                    var inline2554 bool = x227 < x211
                    var inline2556 _goml_m_std_p_cmp_p_Ordering
                    if inline2554 {
                        inline2556 = Less
                    } else {
                        var inline2558 bool = x227 > x211
                        if inline2558 {
                            inline2556 = Greater
                        } else {
                            inline2556 = Equal
                        }
                    }
                    commute_field3422 = inline2556
                    switch commute_field3422 {
                    case Equal:
                        var commute_field3419 _goml_m_std_p_cmp_p_Ordering
                        var inline2548 bool = x228 < x212
                        var inline2550 _goml_m_std_p_cmp_p_Ordering
                        if inline2548 {
                            inline2550 = Less
                        } else {
                            var inline2552 bool = x228 > x212
                            if inline2552 {
                                inline2550 = Greater
                            } else {
                                inline2550 = Equal
                            }
                        }
                        commute_field3419 = inline2550
                        switch commute_field3419 {
                        case Equal:
                            var t1001 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1001
                        default:
                            var t1002 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3419,
                            }
                            return t1002
                        }
                    default:
                        var t1003 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3422,
                        }
                        return t1003
                    }
                default:
                    var t1004 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1004
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
    var jp1020 int
    switch self__39.(type) {
    case Low:
        jp1020 = 0
    case Medium:
        jp1020 = 1
    case High:
        jp1020 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1022 int
    switch other__40.(type) {
    case Low:
        jp1022 = 0
    case Medium:
        jp1022 = 1
    case High:
        jp1022 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1025 bool = jp1020 < jp1022
    if t1025 {
        return Less
    } else {
        var t1028 bool = jp1020 > jp1022
        if t1028 {
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
                var x244 int = other__40.(Medium)._0
                var x245 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x252 int = self__39.(Medium)._0
                    var x253 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 _goml_m_std_p_cmp_p_Ordering
                    var inline2587 bool = x252 < x244
                    if inline2587 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2588 bool = x252 > x244
                        if inline2588 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1037 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1037 = false
                    case Equal:
                        t1037 = true
                    case Greater:
                        t1037 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1037 {
                        var _goml_m__i_derive4__ordering____48 _goml_m_std_p_cmp_p_Ordering
                        var inline2583 bool = x253 < x245
                        if inline2583 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2584 bool = x253 > x245
                            if inline2584 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1040 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1040 = false
                        case Equal:
                            t1040 = true
                        case Greater:
                            t1040 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1040 {
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
                var x246 int = other__40.(High)._0
                var x247 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x258 int = self__39.(High)._0
                    var x259 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x258, x246)
                    var t1045 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1045 {
                        var _goml_m__i_derive10__ordering____54 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x259, x247)
                        var t1048 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1048 = false
                        case Equal:
                            t1048 = true
                        case Greater:
                            t1048 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1048 {
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
    var t1071 float64 = self__60.value
    var t1072 float64 = other__61.value
    var inline2599 bool = t1071 == t1072
    return inline2599
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1076 float64 = self__62.value
    var t1077 float64 = other__63.value
    var commute_field3425 _goml_m_std_p_cmp_p_Ordering
    var inline2601 bool = t1076 < t1077
    if inline2601 {
        commute_field3425 = Less
        switch commute_field3425 {
        case Equal:
            var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1082
        default:
            var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3425,
            }
            return t1083
        }
    } else {
        var inline2603 bool = t1076 > t1077
        if inline2603 {
            commute_field3425 = Greater
            switch commute_field3425 {
            case Equal:
                var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                return t1082
            default:
                var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3425,
                }
                return t1083
            }
        } else {
            var inline2605 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(t1076, t1077)
            if inline2605 {
                commute_field3425 = Equal
                switch commute_field3425 {
                case Equal:
                    var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1082
                default:
                    var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: commute_field3425,
                    }
                    return t1083
                }
            } else {
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1110 int
    switch self__95.(type) {
    case Value:
        jp1110 = 0
    case Empty:
        jp1110 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1112 int
    switch other__96.(type) {
    case Value:
        jp1112 = 0
    case Empty:
        jp1112 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1115 bool = jp1110 < jp1112
    if t1115 {
        var t1116 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1116
    } else {
        var t1119 bool = jp1110 > jp1112
        if t1119 {
            var t1120 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1120
        } else {
            switch other__96.(type) {
            case Value:
                var x296 float64 = other__96.(Value)._0
                switch self__95.(type) {
                case Value:
                    var x297 float64 = self__95.(Value)._0
                    var commute_field3428 _goml_m_std_p_cmp_p_Ordering
                    var inline2632 bool = x297 < x296
                    if inline2632 {
                        commute_field3428 = Less
                        switch commute_field3428 {
                        case Equal:
                            var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1129
                        default:
                            var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3428,
                            }
                            return t1130
                        }
                    } else {
                        var inline2634 bool = x297 > x296
                        if inline2634 {
                            commute_field3428 = Greater
                            switch commute_field3428 {
                            case Equal:
                                var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1129
                            default:
                                var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: commute_field3428,
                                }
                                return t1130
                            }
                        } else {
                            var inline2636 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(x297, x296)
                            if inline2636 {
                                commute_field3428 = Equal
                                switch commute_field3428 {
                                case Equal:
                                    var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1129
                                default:
                                    var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: commute_field3428,
                                    }
                                    return t1130
                                }
                            } else {
                                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                            }
                        }
                    }
                default:
                    var t1131 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1131
                }
            case Empty:
                switch self__95.(type) {
                case Empty:
                    var t1134 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1134
                default:
                    var t1135 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1135
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
    var t1153 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1154 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1153)
    println__T_string(t1154)
    var t1155 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1156 string = ordering_name(t1155)
    println__T_string(t1156)
    var t1157 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1158 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1157)
    var t1159 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1158)
    println__T_string(t1159)
    var t1160 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1161 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1162 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1160, t1161)
    var t1163 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1162)
    println__T_string(t1163)
    var t1164 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1165 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1166 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1164, t1165)
    var t1167 string = ordering_name(t1166)
    println__T_string(t1167)
    var zero__105 float64 = 0
    var t1168 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1168,
    }
    var t1169 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1170 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1169)
    println__T_string(t1170)
    var t1171 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1172 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1171)
    var t1173 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1172)
    println__T_string(t1173)
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
    var t1174 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1175 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1174)
    println__T_string(t1175)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1176 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1177 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1176)
    println__T_string(t1177)
    var t1178 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = Value{
        _0: t1178,
    }
    var t1179 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1180 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1179)
    var t1181 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1180)
    println__T_string(t1181)
    var vec_literal__2131 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 2)
    var vec_literal__2178 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 3)
    var t1182 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(vec_literal__2131, vec_literal__2178)
    var t1183 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1182)
    println__T_string(t1183)
    var t1184 Option__int = Option__int_Some{
        _0: 2,
    }
    var t1185 Option__int = Option__int_Some{
        _0: 3,
    }
    var t1186 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1184, t1185)
    var t1187 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1186)
    println__T_string(t1187)
    var ok__114 Result__int__string = Ok{
        _0: 1,
    }
    var error__115 Result__int__string = Err{
        _0: "error",
    }
    var t1188 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1189 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1188)
    println__T_string(t1189)
    var t1190 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2131, 0, 2)
    var t1191 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2178, 0, 2)
    var t1192 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1190, t1191)
    var t1193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1192)
    println__T_string(t1193)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, vec_literal__2131, "vector")
    var vec_literal__2661 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 2)
    var t1194 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, vec_literal__2661)
    var t1195 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1194, "missing")
    println__T_string(t1195)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default()
    var t1234 int = default_tuple__117._0
    var t1235 int = 0
    var t1236 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1234, t1235)
    var jp1197 bool
    if t1236 {
        var t1237 string = default_tuple__117._1
        var t1238 string = ""
        var inline2661 bool = t1237 == t1238
        jp1197 = inline2661
    } else {
        jp1197 = false
    }
    var t1198 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1197)
    println__T_string(t1198)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs329 [3]int = [3]int{0, 0, 0}
    var t1223 int = array_get__Array_3_3int(default_array__118, 0)
    var t1224 int = array_get__Array_3_3int(_eq_rhs329, 0)
    var t1225 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1223, t1224)
    var jp1200 bool
    if t1225 {
        var t1228 int = array_get__Array_3_3int(default_array__118, 1)
        var t1229 int = array_get__Array_3_3int(_eq_rhs329, 1)
        var t1230 bool
        var inline2665 bool = t1228 == t1229
        t1230 = inline2665
        if t1230 {
            var t1231 int = array_get__Array_3_3int(default_array__118, 2)
            var t1232 int = array_get__Array_3_3int(_eq_rhs329, 2)
            var inline2663 bool = t1231 == t1232
            jp1200 = inline2663
        } else {
            jp1200 = false
        }
    } else {
        jp1200 = false
    }
    var t1201 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1200)
    println__T_string(t1201)
    var t1202 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1203 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1204 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(t1202, t1203)
    var t1205 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1204)
    var inline2706 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1205)
    _goml_runtime_core_string_println(inline2706)
    var t1206 [2]int = [2]int{1, 2}
    var t1207 [2]int = [2]int{1, 3}
    var t1208 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1206, t1207)
    var t1209 string = ordering_name(t1208)
    var inline2703 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1209)
    _goml_runtime_core_string_println(inline2703)
    var t1210 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_7float64_7float64 = Tuple2_7float64_7float64{
        _0: 0,
        _1: t1210,
    }
    var t1211 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1212 bool
    var inline2700 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t1211)
    var inline2701 bool = !inline2700
    t1212 = inline2701
    var t1213 string
    var inline2698 string = _goml_runtime_core_bool_to_string(t1212)
    t1213 = inline2698
    var inline2695 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1213)
    _goml_runtime_core_string_println(inline2695)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2693 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2693
    var t1214 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2690 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1214, inline2690)
    var t1215 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1216 Option__string
    var inline2688 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1215)
    t1216 = inline2688
    var t1217 string
    var inline2684 string = "missing"
    switch t1216.(type) {
    case Option__string_None:
        t1217 = inline2684
    case Option__string_Some:
        var inline2685 string = t1216.(Option__string_Some)._0
        t1217 = inline2685
    default:
        panic("non-exhaustive match")
    }
    var inline2681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1217)
    _goml_runtime_core_string_println(inline2681)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2679 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2679
    var t1218 [2]int = [2]int{1, 2}
    var inline2676 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1218, inline2676)
    var t1219 [2]int = [2]int{1, 2}
    var t1220 Option__string
    var inline2674 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1219)
    t1220 = inline2674
    var t1221 string
    var inline2670 string = "missing"
    switch t1220.(type) {
    case Option__string_None:
        t1221 = inline2670
    case Option__string_Some:
        var inline2671 string = t1220.(Option__string_Some)._0
        t1221 = inline2671
    default:
        panic("non-exhaustive match")
    }
    var inline2667 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1221)
    _goml_runtime_core_string_println(inline2667)
    return struct{}{}
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(self__15 int, other__16 int) bool {
    var commute_field3491 _goml_m_std_p_cmp_p_Ordering
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
    commute_field3491 = inline2832
    switch commute_field3491 {
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

func _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(self__123 float64, other__124 float64) bool {
    var t1600 bool = self__123 == other__124
    return t1600
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t1625 bool = self__103 == other__104
    return t1625
}

func println__T_string(value__31 string) struct{} {
    var t1718 string
    t1718 = value__31
    _goml_runtime_core_string_println(t1718)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1722 string = _goml_runtime_core_bool_to_string(self__66)
    return t1722
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(self__289 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    var t1725 bool
    switch self__289.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        t1725 = false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        t1725 = true
    default:
        panic("non-exhaustive match")
    }
    var t1726 bool = !t1725
    return t1726
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1729 *_goml_vec_int = vec_new__Vec_3int()
    return t1729
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__176 *_goml_vec_int, elem__177 int) struct{} {
    vec_push__Vec_3int(self__176, elem__177)
    return struct{}{}
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3226 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3226.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3227 _goml_m_std_p_cmp_p_Ordering = inline3226.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3229 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3227, Less)
        return inline3229
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3231 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3231.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3232 _goml_m_std_p_cmp_p_Ordering = inline3231.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3234 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3232, Less)
        return inline3234
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3236 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3236.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3237 _goml_m_std_p_cmp_p_Ordering = inline3236.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3239 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3237, Less)
        return inline3239
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3241 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3241.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3242 _goml_m_std_p_cmp_p_Ordering = inline3241.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3244 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3242, Less)
        return inline3244
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3246 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
    switch inline3246.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3247 _goml_m_std_p_cmp_p_Ordering = inline3246.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3249 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3247, Less)
        return inline3249
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__225 *_goml_vec_int, start__226 int, end__227 int) []int {
    var t1749 []int = self__225.items[start__226:end__227]
    return t1749
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3251 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3251.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3252 _goml_m_std_p_cmp_p_Ordering = inline3251.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3254 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3252, Less)
        return inline3254
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1755 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1755
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__248 *hashmap_Vec_3int_string_x, key__249 *_goml_vec_int, value__250 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__246 *hashmap_Vec_3int_string_x, key__247 *_goml_vec_int) Option__string {
    var t1760 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__246, key__247)
    return t1760
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__290 Option__string, fallback__291 string) string {
    switch self__290.(type) {
    case Option__string_None:
        return fallback__291
    case Option__string_Some:
        var x152 string = self__290.(Option__string_Some)._0
        return x152
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default() Tuple2_3int_6string {
    var t1767 int
    t1767 = 0
    var t1768 string
    t1768 = ""
    var t1769 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1767,
        _1: t1768,
    }
    return t1769
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1775 int
    t1775 = 0
    var t1776 int
    t1776 = 0
    var t1777 int
    t1777 = 0
    var t1778 [3]int = [3]int{t1775, t1776, t1777}
    return t1778
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1783 int = self._0
    var t1784 int = other._0
    var t1785 bool
    var inline3267 bool = t1783 == t1784
    t1785 = inline3267
    if t1785 {
        var t1788 int = self._1
        var t1789 int = other._1
        var t1790 bool
        var inline3263 bool = t1788 == t1789
        t1790 = inline3263
        if t1790 {
            return false
        } else {
            var t1791 int = self._1
            var t1792 int = other._1
            var inline3261 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1791, t1792)
            return inline3261
        }
    } else {
        var t1794 int = self._0
        var t1795 int = other._0
        var inline3265 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1794, t1795)
        return inline3265
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) _goml_m_std_p_cmp_p_Ordering {
    var t1799 int = array_get__Array_2_3int(self, 0)
    var t1800 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 _goml_m_std_p_cmp_p_Ordering
    var inline3274 bool = t1799 < t1800
    if inline3274 {
        _structural_ordering_0 = Less
    } else {
        var inline3275 bool = t1799 > t1800
        if inline3275 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1803 bool
    switch _structural_ordering_0 {
    case Less:
        t1803 = false
    case Equal:
        t1803 = true
    case Greater:
        t1803 = false
    default:
        panic("non-exhaustive match")
    }
    if t1803 {
        var t1804 int = array_get__Array_2_3int(self, 1)
        var t1805 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 _goml_m_std_p_cmp_p_Ordering
        var inline3270 bool = t1804 < t1805
        if inline3270 {
            _structural_ordering_1 = Less
        } else {
            var inline3271 bool = t1804 > t1805
            if inline3271 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1808 bool
        switch _structural_ordering_1 {
        case Less:
            t1808 = false
        case Equal:
            t1808 = true
        case Greater:
            t1808 = false
        default:
            panic("non-exhaustive match")
        }
        if t1808 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(self Tuple2_7float64_7float64, other Tuple2_7float64_7float64) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1811 float64 = self._0
    var t1812 float64 = other._0
    var _structural_partial_ordering_0 _goml_m_Option____std_p_cmp_p_Ordering
    var commute_field3650 _goml_m_std_p_cmp_p_Ordering
    var inline3286 bool = t1811 < t1812
    if inline3286 {
        var inline3287 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        _structural_partial_ordering_0 = inline3287
        commute_field3650 = Less
        var t1817 bool
        switch commute_field3650 {
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
            var t1818 float64 = self._1
            var t1819 float64 = other._1
            var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
            var commute_field3647 _goml_m_std_p_cmp_p_Ordering
            var inline3278 bool = t1818 < t1819
            if inline3278 {
                var inline3279 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Less,
                }
                _structural_partial_ordering_1 = inline3279
                commute_field3647 = Less
                var t1824 bool
                switch commute_field3647 {
                case Less:
                    t1824 = false
                case Equal:
                    t1824 = true
                case Greater:
                    t1824 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1824 {
                    var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1825
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3280 bool = t1818 > t1819
                if inline3280 {
                    var inline3281 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Greater,
                    }
                    _structural_partial_ordering_1 = inline3281
                    commute_field3647 = Greater
                    var t1824 bool
                    switch commute_field3647 {
                    case Less:
                        t1824 = false
                    case Equal:
                        t1824 = true
                    case Greater:
                        t1824 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1824 {
                        var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1825
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3282 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(t1818, t1819)
                    if inline3282 {
                        var inline3283 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        _structural_partial_ordering_1 = inline3283
                        commute_field3647 = Equal
                        var t1824 bool
                        switch commute_field3647 {
                        case Less:
                            t1824 = false
                        case Equal:
                            t1824 = true
                        case Greater:
                            t1824 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1824 {
                            var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1825
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
        var inline3288 bool = t1811 > t1812
        if inline3288 {
            var inline3289 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            _structural_partial_ordering_0 = inline3289
            commute_field3650 = Greater
            var t1817 bool
            switch commute_field3650 {
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
                var t1818 float64 = self._1
                var t1819 float64 = other._1
                var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                var commute_field3647 _goml_m_std_p_cmp_p_Ordering
                var inline3278 bool = t1818 < t1819
                if inline3278 {
                    var inline3279 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Less,
                    }
                    _structural_partial_ordering_1 = inline3279
                    commute_field3647 = Less
                    var t1824 bool
                    switch commute_field3647 {
                    case Less:
                        t1824 = false
                    case Equal:
                        t1824 = true
                    case Greater:
                        t1824 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1824 {
                        var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1825
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3280 bool = t1818 > t1819
                    if inline3280 {
                        var inline3281 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Greater,
                        }
                        _structural_partial_ordering_1 = inline3281
                        commute_field3647 = Greater
                        var t1824 bool
                        switch commute_field3647 {
                        case Less:
                            t1824 = false
                        case Equal:
                            t1824 = true
                        case Greater:
                            t1824 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1824 {
                            var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1825
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3282 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(t1818, t1819)
                        if inline3282 {
                            var inline3283 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            _structural_partial_ordering_1 = inline3283
                            commute_field3647 = Equal
                            var t1824 bool
                            switch commute_field3647 {
                            case Less:
                                t1824 = false
                            case Equal:
                                t1824 = true
                            case Greater:
                                t1824 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1824 {
                                var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1825
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
            var inline3290 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(t1811, t1812)
            if inline3290 {
                var inline3291 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                _structural_partial_ordering_0 = inline3291
                commute_field3650 = Equal
                var t1817 bool
                switch commute_field3650 {
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
                    var t1818 float64 = self._1
                    var t1819 float64 = other._1
                    var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                    var commute_field3647 _goml_m_std_p_cmp_p_Ordering
                    var inline3278 bool = t1818 < t1819
                    if inline3278 {
                        var inline3279 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Less,
                        }
                        _structural_partial_ordering_1 = inline3279
                        commute_field3647 = Less
                        var t1824 bool
                        switch commute_field3647 {
                        case Less:
                            t1824 = false
                        case Equal:
                            t1824 = true
                        case Greater:
                            t1824 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1824 {
                            var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1825
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3280 bool = t1818 > t1819
                        if inline3280 {
                            var inline3281 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Greater,
                            }
                            _structural_partial_ordering_1 = inline3281
                            commute_field3647 = Greater
                            var t1824 bool
                            switch commute_field3647 {
                            case Less:
                                t1824 = false
                            case Equal:
                                t1824 = true
                            case Greater:
                                t1824 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1824 {
                                var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1825
                            } else {
                                return _structural_partial_ordering_1
                            }
                        } else {
                            var inline3282 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(t1818, t1819)
                            if inline3282 {
                                var inline3283 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                _structural_partial_ordering_1 = inline3283
                                commute_field3647 = Equal
                                var t1824 bool
                                switch commute_field3647 {
                                case Less:
                                    t1824 = false
                                case Equal:
                                    t1824 = true
                                case Greater:
                                    t1824 = false
                                default:
                                    panic("non-exhaustive match")
                                }
                                if t1824 {
                                    var t1825 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1825
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

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__288 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__288.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(self__67 GenericPair__int, other__68 GenericPair__int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1887 int = self__67.first
    var t1888 int = other__68.first
    var commute_field3656 _goml_m_std_p_cmp_p_Ordering
    var inline3305 bool = t1887 < t1888
    var inline3307 _goml_m_std_p_cmp_p_Ordering
    if inline3305 {
        inline3307 = Less
    } else {
        var inline3309 bool = t1887 > t1888
        if inline3309 {
            inline3307 = Greater
        } else {
            inline3307 = Equal
        }
    }
    commute_field3656 = inline3307
    switch commute_field3656 {
    case Equal:
        var t1893 int = self__67.second
        var t1894 int = other__68.second
        var commute_field3653 _goml_m_std_p_cmp_p_Ordering
        var inline3299 bool = t1893 < t1894
        var inline3301 _goml_m_std_p_cmp_p_Ordering
        if inline3299 {
            inline3301 = Less
        } else {
            var inline3303 bool = t1893 > t1894
            if inline3303 {
                inline3301 = Greater
            } else {
                inline3301 = Equal
            }
        }
        commute_field3653 = inline3301
        switch commute_field3653 {
        case Equal:
            var t1899 *_goml_vec_int = self__67.nested
            var t1900 *_goml_vec_int = other__68.nested
            var mtmp270 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t1899, t1900)
            switch mtmp270.(type) {
            case _goml_m_Option____std_p_cmp_p_Ordering_None:
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            case _goml_m_Option____std_p_cmp_p_Ordering_Some:
                var x271 _goml_m_std_p_cmp_p_Ordering = mtmp270.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
                switch x271 {
                case Equal:
                    var t1905 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1905
                default:
                    var t1906 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: x271,
                    }
                    return t1906
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1907 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3653,
            }
            return t1907
        }
    default:
        var t1908 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3656,
        }
        return t1908
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1912 int
    switch self__83 {
    case First:
        jp1912 = 0
    case Second:
        jp1912 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1914 int
    switch other__84 {
    case First:
        jp1914 = 0
    case Second:
        jp1914 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1917 bool = jp1912 < jp1914
    if t1917 {
        var t1918 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1918
    } else {
        var t1921 bool = jp1912 > jp1914
        if t1921 {
            var t1922 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1922
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t1927 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1927
                default:
                    var t1928 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1928
                }
            case Second:
                switch self__83 {
                case Second:
                    var t1931 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1931
                default:
                    var t1932 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1932
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__91 *_goml_vec_int, other__92 *_goml_vec_int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1950 int
    var inline3333 int = vec_len__Vec_3int(self__91)
    t1950 = inline3333
    var t1951 int
    var inline3331 int = vec_len__Vec_3int(other__92)
    t1951 = inline3331
    var t1952 bool = t1950 < t1951
    var jp1936 int
    if t1952 {
        var inline3311 int = vec_len__Vec_3int(self__91)
        jp1936 = inline3311
    } else {
        var inline3313 int = vec_len__Vec_3int(other__92)
        jp1936 = inline3313
    }
    var index__94 int = 0
    Loop_loop1941:
    for {
        var t1942 bool = index__94 < jp1936
        if t1942 {
            var t1943 int = vec_get__Vec_3int(self__91, index__94)
            var t1944 int = vec_get__Vec_3int(other__92, index__94)
            var commute_field3659 _goml_m_std_p_cmp_p_Ordering
            var inline3315 bool = t1943 < t1944
            var inline3317 _goml_m_std_p_cmp_p_Ordering
            if inline3315 {
                inline3317 = Less
            } else {
                var inline3319 bool = t1943 > t1944
                if inline3319 {
                    inline3317 = Greater
                } else {
                    inline3317 = Equal
                }
            }
            commute_field3659 = inline3317
            switch commute_field3659 {
            case Equal:
                var compound_old10 int = index__94
                var compound_value11 int = 1
                var t1947 int = compound_old10 + compound_value11
                index__94 = t1947
                continue
            default:
                var t1949 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3659,
                }
                return t1949
            }
        } else {
            break Loop_loop1941
        }
    }
    var t1938 int
    var inline3329 int = vec_len__Vec_3int(self__91)
    t1938 = inline3329
    var t1939 int
    var inline3327 int = vec_len__Vec_3int(other__92)
    t1939 = inline3327
    var inline3321 bool = t1938 < t1939
    var inline3323 _goml_m_std_p_cmp_p_Ordering
    if inline3321 {
        inline3323 = Less
    } else {
        var inline3325 bool = t1938 > t1939
        if inline3325 {
            inline3323 = Greater
        } else {
            inline3323 = Equal
        }
    }
    var inline3324 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3323,
    }
    return inline3324
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__111 Option__int, other__112 Option__int) _goml_m_Option____std_p_cmp_p_Ordering {
    switch other__112.(type) {
    case Option__int_None:
        switch self__111.(type) {
        case Option__int_None:
            var t1961 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1961
        case Option__int_Some:
            var t1962 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1962
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__112.(Option__int_Some)._0
        switch self__111.(type) {
        case Option__int_None:
            var t1965 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1965
        case Option__int_Some:
            var x35 int = self__111.(Option__int_Some)._0
            var inline3335 bool = x35 < x33
            var inline3337 _goml_m_std_p_cmp_p_Ordering
            if inline3335 {
                inline3337 = Less
            } else {
                var inline3339 bool = x35 > x33
                if inline3339 {
                    inline3337 = Greater
                } else {
                    inline3337 = Equal
                }
            }
            var inline3338 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3337,
            }
            return inline3338
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
            var inline3341 bool = x47 < x45
            var inline3343 _goml_m_std_p_cmp_p_Ordering
            if inline3341 {
                inline3343 = Less
            } else {
                var inline3345 bool = x47 > x45
                if inline3345 {
                    inline3343 = Greater
                } else {
                    inline3343 = Equal
                }
            }
            var inline3344 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3343,
            }
            return inline3344
        case Err:
            var t1974 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1974
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__120.(Err)._0
        switch self__119.(type) {
        case Ok:
            var t1977 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1977
        case Err:
            var x50 string = self__119.(Err)._0
            var inline3347 bool = x50 < x46
            var inline3349 _goml_m_std_p_cmp_p_Ordering
            if inline3347 {
                inline3349 = Less
            } else {
                var inline3351 bool = x50 > x46
                if inline3351 {
                    inline3349 = Greater
                } else {
                    inline3349 = Equal
                }
            }
            var inline3350 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3349,
            }
            return inline3350
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__101 []int, other__102 []int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1996 int
    var inline3375 int = len(self__101)
    t1996 = inline3375
    var t1997 int
    var inline3373 int = len(other__102)
    t1997 = inline3373
    var t1998 bool = t1996 < t1997
    var jp1982 int
    if t1998 {
        var inline3353 int = len(self__101)
        jp1982 = inline3353
    } else {
        var inline3355 int = len(other__102)
        jp1982 = inline3355
    }
    var index__104 int = 0
    Loop_loop1987:
    for {
        var t1988 bool = index__104 < jp1982
        if t1988 {
            var t1989 int = self__101[index__104]
            var t1990 int = other__102[index__104]
            var commute_field3662 _goml_m_std_p_cmp_p_Ordering
            var inline3357 bool = t1989 < t1990
            var inline3359 _goml_m_std_p_cmp_p_Ordering
            if inline3357 {
                inline3359 = Less
            } else {
                var inline3361 bool = t1989 > t1990
                if inline3361 {
                    inline3359 = Greater
                } else {
                    inline3359 = Equal
                }
            }
            commute_field3662 = inline3359
            switch commute_field3662 {
            case Equal:
                var compound_old21 int = index__104
                var compound_value22 int = 1
                var t1993 int = compound_old21 + compound_value22
                index__104 = t1993
                continue
            default:
                var t1995 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3662,
                }
                return t1995
            }
        } else {
            break Loop_loop1987
        }
    }
    var t1984 int
    var inline3371 int = len(self__101)
    t1984 = inline3371
    var t1985 int
    var inline3369 int = len(other__102)
    t1985 = inline3369
    var inline3363 bool = t1984 < t1985
    var inline3365 _goml_m_std_p_cmp_p_Ordering
    if inline3363 {
        inline3365 = Less
    } else {
        var inline3367 bool = t1984 > t1985
        if inline3367 {
            inline3365 = Greater
        } else {
            inline3365 = Equal
        }
    }
    var inline3366 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3365,
    }
    return inline3366
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__141 *_goml_vec_int, other__142 *_goml_vec_int) bool {
    var t2011 int
    var inline3385 int = vec_len__Vec_3int(self__141)
    t2011 = inline3385
    var t2012 int
    var inline3383 int = vec_len__Vec_3int(other__142)
    t2012 = inline3383
    var t2013 bool
    var inline3381 bool = t2011 == t2012
    t2013 = inline3381
    var t2014 bool = !t2013
    if t2014 {
        return false
    } else {
        var index__143 int = 0
        Loop_loop2016:
        for {
            var t2017 int
            var inline3379 int = vec_len__Vec_3int(self__141)
            t2017 = inline3379
            var t2018 bool = index__143 < t2017
            if t2018 {
                var t2020 int = vec_get__Vec_3int(self__141, index__143)
                var t2021 int = vec_get__Vec_3int(other__142, index__143)
                var t2022 bool
                var inline3377 bool = t2020 == t2021
                t2022 = inline3377
                if t2022 {
                    var compound_old43 int = index__143
                    var compound_value44 int = 1
                    var t2023 int = compound_old43 + compound_value44
                    index__143 = t2023
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2016
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__144 *_goml_vec_int) uint64 {
    var value__145 uint64 = 14695981039346656037
    var index__146 int = 0
    Loop_loop2028:
    for {
        var t2029 int
        var inline3389 int = vec_len__Vec_3int(self__144)
        t2029 = inline3389
        var t2030 bool = index__146 < t2029
        if t2030 {
            var t2031 uint64 = value__145 * 1099511628211
            var t2032 int = vec_get__Vec_3int(self__144, index__146)
            var t2033 uint64
            var inline3387 uint64 = _goml_runtime_core_int_hash(t2032)
            t2033 = inline3387
            var t2034 uint64 = t2031 + t2033
            value__145 = t2034
            var compound_old48 int = index__146
            var compound_value49 int = 1
            var t2035 int = compound_old48 + compound_value49
            index__146 = t2035
            continue
        } else {
            break Loop_loop2028
        }
    }
    return value__145
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2041 int = self._0
    var t2042 int = other._0
    var t2043 bool
    var inline3393 bool = t2041 == t2042
    t2043 = inline3393
    if t2043 {
        var t2046 string = self._1
        var t2047 string = other._1
        var t2048 bool
        var inline3391 bool = t2046 == t2047
        t2048 = inline3391
        if t2048 {
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
    var t2051 uint64 = _structural_hash_0 * 1099511628211
    var t2052 int = self._0
    var t2053 uint64
    var inline3397 uint64 = _goml_runtime_core_int_hash(t2052)
    t2053 = inline3397
    var _structural_hash_1 uint64 = t2051 + t2053
    var t2054 uint64 = _structural_hash_1 * 1099511628211
    var t2055 string = self._1
    var t2056 uint64
    var inline3395 uint64 = _goml_runtime_core_string_hash(t2055)
    t2056 = inline3395
    var _structural_hash_2 uint64 = t2054 + t2056
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2061 int = array_get__Array_2_3int(self, 0)
    var t2062 int = array_get__Array_2_3int(other, 0)
    var t2063 bool
    var inline3401 bool = t2061 == t2062
    t2063 = inline3401
    if t2063 {
        var t2066 int = array_get__Array_2_3int(self, 1)
        var t2067 int = array_get__Array_2_3int(other, 1)
        var t2068 bool
        var inline3399 bool = t2066 == t2067
        t2068 = inline3399
        if t2068 {
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
    var t2071 uint64 = _structural_hash_0 * 1099511628211
    var t2072 int = array_get__Array_2_3int(self, 0)
    var t2073 uint64
    var inline3405 uint64 = _goml_runtime_core_int_hash(t2072)
    t2073 = inline3405
    var _structural_hash_1 uint64 = t2071 + t2073
    var t2074 uint64 = _structural_hash_1 * 1099511628211
    var t2075 int = array_get__Array_2_3int(self, 1)
    var t2076 uint64
    var inline3403 uint64 = _goml_runtime_core_int_hash(t2075)
    t2076 = inline3403
    var _structural_hash_2 uint64 = t2074 + t2076
    return _structural_hash_2
}

func main() {
    main0()
}
