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
    var commute_field3393 _goml_m_std_p_cmp_p_Ordering
    var inline2473 bool = t886 < t887
    var inline2475 _goml_m_std_p_cmp_p_Ordering
    if inline2473 {
        inline2475 = Less
    } else {
        var inline2477 bool = t886 > t887
        if inline2477 {
            inline2475 = Greater
        } else {
            inline2475 = Equal
        }
    }
    commute_field3393 = inline2475
    switch commute_field3393 {
    case Equal:
        var t892 int = self__2.minor
        var t893 int = other__3.minor
        var commute_field3390 _goml_m_std_p_cmp_p_Ordering
        var inline2467 bool = t892 < t893
        var inline2469 _goml_m_std_p_cmp_p_Ordering
        if inline2467 {
            inline2469 = Less
        } else {
            var inline2471 bool = t892 > t893
            if inline2471 {
                inline2469 = Greater
            } else {
                inline2469 = Equal
            }
        }
        commute_field3390 = inline2469
        switch commute_field3390 {
        case Equal:
            var t898 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t898
        default:
            var t899 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3390,
            }
            return t899
        }
    default:
        var t900 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3393,
        }
        return t900
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2479 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2479.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2480 _goml_m_std_p_cmp_p_Ordering = inline2479.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2482 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2480, Less)
        return inline2482
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) _goml_m_std_p_cmp_p_Ordering {
    var t915 int = self__6.major
    var t916 int = other__7.major
    var _goml_m__i_derive1__ordering____8 _goml_m_std_p_cmp_p_Ordering
    var inline2506 bool = t915 < t916
    if inline2506 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2507 bool = t915 > t916
        if inline2507 {
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
        var inline2502 bool = t920 < t921
        if inline2502 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2503 bool = t920 > t921
            if inline2503 {
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
                    var commute_field3399 _goml_m_std_p_cmp_p_Ordering
                    var inline2527 bool = x217 < x209
                    var inline2529 _goml_m_std_p_cmp_p_Ordering
                    if inline2527 {
                        inline2529 = Less
                    } else {
                        var inline2531 bool = x217 > x209
                        if inline2531 {
                            inline2529 = Greater
                        } else {
                            inline2529 = Equal
                        }
                    }
                    commute_field3399 = inline2529
                    switch commute_field3399 {
                    case Equal:
                        var commute_field3396 _goml_m_std_p_cmp_p_Ordering
                        var inline2521 bool = x218 < x210
                        var inline2523 _goml_m_std_p_cmp_p_Ordering
                        if inline2521 {
                            inline2523 = Less
                        } else {
                            var inline2525 bool = x218 > x210
                            if inline2525 {
                                inline2523 = Greater
                            } else {
                                inline2523 = Equal
                            }
                        }
                        commute_field3396 = inline2523
                        switch commute_field3396 {
                        case Equal:
                            var t987 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t987
                        default:
                            var t988 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3396,
                            }
                            return t988
                        }
                    default:
                        var t989 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3399,
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
                    var commute_field3405 _goml_m_std_p_cmp_p_Ordering
                    var inline2539 bool = x227 < x211
                    var inline2541 _goml_m_std_p_cmp_p_Ordering
                    if inline2539 {
                        inline2541 = Less
                    } else {
                        var inline2543 bool = x227 > x211
                        if inline2543 {
                            inline2541 = Greater
                        } else {
                            inline2541 = Equal
                        }
                    }
                    commute_field3405 = inline2541
                    switch commute_field3405 {
                    case Equal:
                        var commute_field3402 _goml_m_std_p_cmp_p_Ordering
                        var inline2533 bool = x228 < x212
                        var inline2535 _goml_m_std_p_cmp_p_Ordering
                        if inline2533 {
                            inline2535 = Less
                        } else {
                            var inline2537 bool = x228 > x212
                            if inline2537 {
                                inline2535 = Greater
                            } else {
                                inline2535 = Equal
                            }
                        }
                        commute_field3402 = inline2535
                        switch commute_field3402 {
                        case Equal:
                            var t1001 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1001
                        default:
                            var t1002 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3402,
                            }
                            return t1002
                        }
                    default:
                        var t1003 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3405,
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
    var inline2545 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2545.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2546 _goml_m_std_p_cmp_p_Ordering = inline2545.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2548 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2546, Less)
        return inline2548
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
                    var inline2572 bool = x252 < x244
                    if inline2572 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2573 bool = x252 > x244
                        if inline2573 {
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
                        var inline2568 bool = x253 < x245
                        if inline2568 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2569 bool = x253 > x245
                            if inline2569 {
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
    var inline2584 bool = t1071 == t1072
    return inline2584
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1076 float64 = self__62.value
    var t1077 float64 = other__63.value
    var commute_field3408 _goml_m_std_p_cmp_p_Ordering
    var inline2586 bool = t1076 < t1077
    if inline2586 {
        commute_field3408 = Less
        switch commute_field3408 {
        case Equal:
            var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1082
        default:
            var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3408,
            }
            return t1083
        }
    } else {
        var inline2588 bool = t1076 > t1077
        if inline2588 {
            commute_field3408 = Greater
            switch commute_field3408 {
            case Equal:
                var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                return t1082
            default:
                var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3408,
                }
                return t1083
            }
        } else {
            var inline2590 bool = t1076 == t1077
            if inline2590 {
                commute_field3408 = Equal
                switch commute_field3408 {
                case Equal:
                    var t1082 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1082
                default:
                    var t1083 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: commute_field3408,
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
                    var commute_field3411 _goml_m_std_p_cmp_p_Ordering
                    var inline2617 bool = x297 < x296
                    if inline2617 {
                        commute_field3411 = Less
                        switch commute_field3411 {
                        case Equal:
                            var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1129
                        default:
                            var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3411,
                            }
                            return t1130
                        }
                    } else {
                        var inline2619 bool = x297 > x296
                        if inline2619 {
                            commute_field3411 = Greater
                            switch commute_field3411 {
                            case Equal:
                                var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1129
                            default:
                                var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: commute_field3411,
                                }
                                return t1130
                            }
                        } else {
                            var inline2621 bool = x297 == x296
                            if inline2621 {
                                commute_field3411 = Equal
                                switch commute_field3411 {
                                case Equal:
                                    var t1129 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1129
                                default:
                                    var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: commute_field3411,
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
        var inline2646 bool = t1237 == t1238
        jp1197 = inline2646
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
        var inline2650 bool = t1228 == t1229
        t1230 = inline2650
        if t1230 {
            var t1231 int = array_get__Array_3_3int(default_array__118, 2)
            var t1232 int = array_get__Array_3_3int(_eq_rhs329, 2)
            var inline2648 bool = t1231 == t1232
            jp1200 = inline2648
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
    var inline2691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1205)
    _goml_runtime_core_string_println(inline2691)
    var t1206 [2]int = [2]int{1, 2}
    var t1207 [2]int = [2]int{1, 3}
    var t1208 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1206, t1207)
    var t1209 string = ordering_name(t1208)
    var inline2688 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1209)
    _goml_runtime_core_string_println(inline2688)
    var t1210 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_7float64_7float64 = Tuple2_7float64_7float64{
        _0: 0,
        _1: t1210,
    }
    var t1211 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1212 bool
    var inline2685 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t1211)
    var inline2686 bool = !inline2685
    t1212 = inline2686
    var t1213 string
    var inline2683 string = _goml_runtime_core_bool_to_string(t1212)
    t1213 = inline2683
    var inline2680 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1213)
    _goml_runtime_core_string_println(inline2680)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2678 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2678
    var t1214 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2675 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1214, inline2675)
    var t1215 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1216 Option__string
    var inline2673 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1215)
    t1216 = inline2673
    var t1217 string
    var inline2669 string = "missing"
    switch t1216.(type) {
    case Option__string_None:
        t1217 = inline2669
    case Option__string_Some:
        var inline2670 string = t1216.(Option__string_Some)._0
        t1217 = inline2670
    default:
        panic("non-exhaustive match")
    }
    var inline2666 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1217)
    _goml_runtime_core_string_println(inline2666)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2664 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2664
    var t1218 [2]int = [2]int{1, 2}
    var inline2661 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1218, inline2661)
    var t1219 [2]int = [2]int{1, 2}
    var t1220 Option__string
    var inline2659 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1219)
    t1220 = inline2659
    var t1221 string
    var inline2655 string = "missing"
    switch t1220.(type) {
    case Option__string_None:
        t1221 = inline2655
    case Option__string_Some:
        var inline2656 string = t1220.(Option__string_Some)._0
        t1221 = inline2656
    default:
        panic("non-exhaustive match")
    }
    var inline2652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1221)
    _goml_runtime_core_string_println(inline2652)
    return struct{}{}
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(self__15 int, other__16 int) bool {
    var commute_field3474 _goml_m_std_p_cmp_p_Ordering
    var inline2815 bool = self__15 < other__16
    var inline2817 _goml_m_std_p_cmp_p_Ordering
    if inline2815 {
        inline2817 = Less
    } else {
        var inline2819 bool = self__15 > other__16
        if inline2819 {
            inline2817 = Greater
        } else {
            inline2817 = Equal
        }
    }
    commute_field3474 = inline2817
    switch commute_field3474 {
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

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t1616 bool = self__103 == other__104
    return t1616
}

func println__T_string(value__31 string) struct{} {
    var t1712 string
    t1712 = value__31
    _goml_runtime_core_string_println(t1712)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1716 string = _goml_runtime_core_bool_to_string(self__66)
    return t1716
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(self__289 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    var t1719 bool
    switch self__289.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        t1719 = false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        t1719 = true
    default:
        panic("non-exhaustive match")
    }
    var t1720 bool = !t1719
    return t1720
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1723 *_goml_vec_int = vec_new__Vec_3int()
    return t1723
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__176 *_goml_vec_int, elem__177 int) struct{} {
    vec_push__Vec_3int(self__176, elem__177)
    return struct{}{}
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3211 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3211.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3212 _goml_m_std_p_cmp_p_Ordering = inline3211.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3214 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3212, Less)
        return inline3214
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3216 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3216.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3217 _goml_m_std_p_cmp_p_Ordering = inline3216.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3219 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3217, Less)
        return inline3219
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3221 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3221.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3222 _goml_m_std_p_cmp_p_Ordering = inline3221.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3224 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3222, Less)
        return inline3224
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3226 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3231 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__225 *_goml_vec_int, start__226 int, end__227 int) []int {
    var t1743 []int = self__225.items[start__226:end__227]
    return t1743
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3236 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1749 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1749
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__248 *hashmap_Vec_3int_string_x, key__249 *_goml_vec_int, value__250 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__246 *hashmap_Vec_3int_string_x, key__247 *_goml_vec_int) Option__string {
    var t1754 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__246, key__247)
    return t1754
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
    var t1761 int
    t1761 = 0
    var t1762 string
    t1762 = ""
    var t1763 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1761,
        _1: t1762,
    }
    return t1763
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1769 int
    t1769 = 0
    var t1770 int
    t1770 = 0
    var t1771 int
    t1771 = 0
    var t1772 [3]int = [3]int{t1769, t1770, t1771}
    return t1772
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1777 int = self._0
    var t1778 int = other._0
    var t1779 bool
    var inline3252 bool = t1777 == t1778
    t1779 = inline3252
    if t1779 {
        var t1782 int = self._1
        var t1783 int = other._1
        var t1784 bool
        var inline3248 bool = t1782 == t1783
        t1784 = inline3248
        if t1784 {
            return false
        } else {
            var t1785 int = self._1
            var t1786 int = other._1
            var inline3246 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1785, t1786)
            return inline3246
        }
    } else {
        var t1788 int = self._0
        var t1789 int = other._0
        var inline3250 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1788, t1789)
        return inline3250
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) _goml_m_std_p_cmp_p_Ordering {
    var t1793 int = array_get__Array_2_3int(self, 0)
    var t1794 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 _goml_m_std_p_cmp_p_Ordering
    var inline3259 bool = t1793 < t1794
    if inline3259 {
        _structural_ordering_0 = Less
    } else {
        var inline3260 bool = t1793 > t1794
        if inline3260 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1797 bool
    switch _structural_ordering_0 {
    case Less:
        t1797 = false
    case Equal:
        t1797 = true
    case Greater:
        t1797 = false
    default:
        panic("non-exhaustive match")
    }
    if t1797 {
        var t1798 int = array_get__Array_2_3int(self, 1)
        var t1799 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 _goml_m_std_p_cmp_p_Ordering
        var inline3255 bool = t1798 < t1799
        if inline3255 {
            _structural_ordering_1 = Less
        } else {
            var inline3256 bool = t1798 > t1799
            if inline3256 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1802 bool
        switch _structural_ordering_1 {
        case Less:
            t1802 = false
        case Equal:
            t1802 = true
        case Greater:
            t1802 = false
        default:
            panic("non-exhaustive match")
        }
        if t1802 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(self Tuple2_7float64_7float64, other Tuple2_7float64_7float64) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1805 float64 = self._0
    var t1806 float64 = other._0
    var _structural_partial_ordering_0 _goml_m_Option____std_p_cmp_p_Ordering
    var commute_field3633 _goml_m_std_p_cmp_p_Ordering
    var inline3271 bool = t1805 < t1806
    if inline3271 {
        var inline3272 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        _structural_partial_ordering_0 = inline3272
        commute_field3633 = Less
        var t1811 bool
        switch commute_field3633 {
        case Less:
            t1811 = false
        case Equal:
            t1811 = true
        case Greater:
            t1811 = false
        default:
            panic("non-exhaustive match")
        }
        if t1811 {
            var t1812 float64 = self._1
            var t1813 float64 = other._1
            var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
            var commute_field3630 _goml_m_std_p_cmp_p_Ordering
            var inline3263 bool = t1812 < t1813
            if inline3263 {
                var inline3264 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Less,
                }
                _structural_partial_ordering_1 = inline3264
                commute_field3630 = Less
                var t1818 bool
                switch commute_field3630 {
                case Less:
                    t1818 = false
                case Equal:
                    t1818 = true
                case Greater:
                    t1818 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1818 {
                    var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1819
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3265 bool = t1812 > t1813
                if inline3265 {
                    var inline3266 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Greater,
                    }
                    _structural_partial_ordering_1 = inline3266
                    commute_field3630 = Greater
                    var t1818 bool
                    switch commute_field3630 {
                    case Less:
                        t1818 = false
                    case Equal:
                        t1818 = true
                    case Greater:
                        t1818 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1818 {
                        var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1819
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3267 bool = t1812 == t1813
                    if inline3267 {
                        var inline3268 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        _structural_partial_ordering_1 = inline3268
                        commute_field3630 = Equal
                        var t1818 bool
                        switch commute_field3630 {
                        case Less:
                            t1818 = false
                        case Equal:
                            t1818 = true
                        case Greater:
                            t1818 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1818 {
                            var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1819
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
        var inline3273 bool = t1805 > t1806
        if inline3273 {
            var inline3274 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            _structural_partial_ordering_0 = inline3274
            commute_field3633 = Greater
            var t1811 bool
            switch commute_field3633 {
            case Less:
                t1811 = false
            case Equal:
                t1811 = true
            case Greater:
                t1811 = false
            default:
                panic("non-exhaustive match")
            }
            if t1811 {
                var t1812 float64 = self._1
                var t1813 float64 = other._1
                var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                var commute_field3630 _goml_m_std_p_cmp_p_Ordering
                var inline3263 bool = t1812 < t1813
                if inline3263 {
                    var inline3264 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Less,
                    }
                    _structural_partial_ordering_1 = inline3264
                    commute_field3630 = Less
                    var t1818 bool
                    switch commute_field3630 {
                    case Less:
                        t1818 = false
                    case Equal:
                        t1818 = true
                    case Greater:
                        t1818 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1818 {
                        var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1819
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3265 bool = t1812 > t1813
                    if inline3265 {
                        var inline3266 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Greater,
                        }
                        _structural_partial_ordering_1 = inline3266
                        commute_field3630 = Greater
                        var t1818 bool
                        switch commute_field3630 {
                        case Less:
                            t1818 = false
                        case Equal:
                            t1818 = true
                        case Greater:
                            t1818 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1818 {
                            var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1819
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3267 bool = t1812 == t1813
                        if inline3267 {
                            var inline3268 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            _structural_partial_ordering_1 = inline3268
                            commute_field3630 = Equal
                            var t1818 bool
                            switch commute_field3630 {
                            case Less:
                                t1818 = false
                            case Equal:
                                t1818 = true
                            case Greater:
                                t1818 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1818 {
                                var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1819
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
            var inline3275 bool = t1805 == t1806
            if inline3275 {
                var inline3276 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                _structural_partial_ordering_0 = inline3276
                commute_field3633 = Equal
                var t1811 bool
                switch commute_field3633 {
                case Less:
                    t1811 = false
                case Equal:
                    t1811 = true
                case Greater:
                    t1811 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1811 {
                    var t1812 float64 = self._1
                    var t1813 float64 = other._1
                    var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                    var commute_field3630 _goml_m_std_p_cmp_p_Ordering
                    var inline3263 bool = t1812 < t1813
                    if inline3263 {
                        var inline3264 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Less,
                        }
                        _structural_partial_ordering_1 = inline3264
                        commute_field3630 = Less
                        var t1818 bool
                        switch commute_field3630 {
                        case Less:
                            t1818 = false
                        case Equal:
                            t1818 = true
                        case Greater:
                            t1818 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1818 {
                            var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1819
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3265 bool = t1812 > t1813
                        if inline3265 {
                            var inline3266 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Greater,
                            }
                            _structural_partial_ordering_1 = inline3266
                            commute_field3630 = Greater
                            var t1818 bool
                            switch commute_field3630 {
                            case Less:
                                t1818 = false
                            case Equal:
                                t1818 = true
                            case Greater:
                                t1818 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1818 {
                                var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1819
                            } else {
                                return _structural_partial_ordering_1
                            }
                        } else {
                            var inline3267 bool = t1812 == t1813
                            if inline3267 {
                                var inline3268 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                _structural_partial_ordering_1 = inline3268
                                commute_field3630 = Equal
                                var t1818 bool
                                switch commute_field3630 {
                                case Less:
                                    t1818 = false
                                case Equal:
                                    t1818 = true
                                case Greater:
                                    t1818 = false
                                default:
                                    panic("non-exhaustive match")
                                }
                                if t1818 {
                                    var t1819 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1819
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
    var t1881 int = self__67.first
    var t1882 int = other__68.first
    var commute_field3639 _goml_m_std_p_cmp_p_Ordering
    var inline3290 bool = t1881 < t1882
    var inline3292 _goml_m_std_p_cmp_p_Ordering
    if inline3290 {
        inline3292 = Less
    } else {
        var inline3294 bool = t1881 > t1882
        if inline3294 {
            inline3292 = Greater
        } else {
            inline3292 = Equal
        }
    }
    commute_field3639 = inline3292
    switch commute_field3639 {
    case Equal:
        var t1887 int = self__67.second
        var t1888 int = other__68.second
        var commute_field3636 _goml_m_std_p_cmp_p_Ordering
        var inline3284 bool = t1887 < t1888
        var inline3286 _goml_m_std_p_cmp_p_Ordering
        if inline3284 {
            inline3286 = Less
        } else {
            var inline3288 bool = t1887 > t1888
            if inline3288 {
                inline3286 = Greater
            } else {
                inline3286 = Equal
            }
        }
        commute_field3636 = inline3286
        switch commute_field3636 {
        case Equal:
            var t1893 *_goml_vec_int = self__67.nested
            var t1894 *_goml_vec_int = other__68.nested
            var mtmp270 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t1893, t1894)
            switch mtmp270.(type) {
            case _goml_m_Option____std_p_cmp_p_Ordering_None:
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            case _goml_m_Option____std_p_cmp_p_Ordering_Some:
                var x271 _goml_m_std_p_cmp_p_Ordering = mtmp270.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
                switch x271 {
                case Equal:
                    var t1899 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1899
                default:
                    var t1900 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: x271,
                    }
                    return t1900
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1901 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3636,
            }
            return t1901
        }
    default:
        var t1902 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3639,
        }
        return t1902
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1906 int
    switch self__83 {
    case First:
        jp1906 = 0
    case Second:
        jp1906 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1908 int
    switch other__84 {
    case First:
        jp1908 = 0
    case Second:
        jp1908 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1911 bool = jp1906 < jp1908
    if t1911 {
        var t1912 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1912
    } else {
        var t1915 bool = jp1906 > jp1908
        if t1915 {
            var t1916 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1916
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t1921 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1921
                default:
                    var t1922 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1922
                }
            case Second:
                switch self__83 {
                case Second:
                    var t1925 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1925
                default:
                    var t1926 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1926
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__91 *_goml_vec_int, other__92 *_goml_vec_int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1944 int
    var inline3318 int = vec_len__Vec_3int(self__91)
    t1944 = inline3318
    var t1945 int
    var inline3316 int = vec_len__Vec_3int(other__92)
    t1945 = inline3316
    var t1946 bool = t1944 < t1945
    var jp1930 int
    if t1946 {
        var inline3296 int = vec_len__Vec_3int(self__91)
        jp1930 = inline3296
    } else {
        var inline3298 int = vec_len__Vec_3int(other__92)
        jp1930 = inline3298
    }
    var index__94 int = 0
    Loop_loop1935:
    for {
        var t1936 bool = index__94 < jp1930
        if t1936 {
            var t1937 int = vec_get__Vec_3int(self__91, index__94)
            var t1938 int = vec_get__Vec_3int(other__92, index__94)
            var commute_field3642 _goml_m_std_p_cmp_p_Ordering
            var inline3300 bool = t1937 < t1938
            var inline3302 _goml_m_std_p_cmp_p_Ordering
            if inline3300 {
                inline3302 = Less
            } else {
                var inline3304 bool = t1937 > t1938
                if inline3304 {
                    inline3302 = Greater
                } else {
                    inline3302 = Equal
                }
            }
            commute_field3642 = inline3302
            switch commute_field3642 {
            case Equal:
                var compound_old10 int = index__94
                var compound_value11 int = 1
                var t1941 int = compound_old10 + compound_value11
                index__94 = t1941
                continue
            default:
                var t1943 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3642,
                }
                return t1943
            }
        } else {
            break Loop_loop1935
        }
    }
    var t1932 int
    var inline3314 int = vec_len__Vec_3int(self__91)
    t1932 = inline3314
    var t1933 int
    var inline3312 int = vec_len__Vec_3int(other__92)
    t1933 = inline3312
    var inline3306 bool = t1932 < t1933
    var inline3308 _goml_m_std_p_cmp_p_Ordering
    if inline3306 {
        inline3308 = Less
    } else {
        var inline3310 bool = t1932 > t1933
        if inline3310 {
            inline3308 = Greater
        } else {
            inline3308 = Equal
        }
    }
    var inline3309 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3308,
    }
    return inline3309
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__111 Option__int, other__112 Option__int) _goml_m_Option____std_p_cmp_p_Ordering {
    switch other__112.(type) {
    case Option__int_None:
        switch self__111.(type) {
        case Option__int_None:
            var t1955 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1955
        case Option__int_Some:
            var t1956 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1956
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__112.(Option__int_Some)._0
        switch self__111.(type) {
        case Option__int_None:
            var t1959 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1959
        case Option__int_Some:
            var x35 int = self__111.(Option__int_Some)._0
            var inline3320 bool = x35 < x33
            var inline3322 _goml_m_std_p_cmp_p_Ordering
            if inline3320 {
                inline3322 = Less
            } else {
                var inline3324 bool = x35 > x33
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
            var inline3326 bool = x47 < x45
            var inline3328 _goml_m_std_p_cmp_p_Ordering
            if inline3326 {
                inline3328 = Less
            } else {
                var inline3330 bool = x47 > x45
                if inline3330 {
                    inline3328 = Greater
                } else {
                    inline3328 = Equal
                }
            }
            var inline3329 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3328,
            }
            return inline3329
        case Err:
            var t1968 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1968
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__120.(Err)._0
        switch self__119.(type) {
        case Ok:
            var t1971 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1971
        case Err:
            var x50 string = self__119.(Err)._0
            var inline3332 bool = x50 < x46
            var inline3334 _goml_m_std_p_cmp_p_Ordering
            if inline3332 {
                inline3334 = Less
            } else {
                var inline3336 bool = x50 > x46
                if inline3336 {
                    inline3334 = Greater
                } else {
                    inline3334 = Equal
                }
            }
            var inline3335 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3334,
            }
            return inline3335
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__101 []int, other__102 []int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1990 int
    var inline3360 int = len(self__101)
    t1990 = inline3360
    var t1991 int
    var inline3358 int = len(other__102)
    t1991 = inline3358
    var t1992 bool = t1990 < t1991
    var jp1976 int
    if t1992 {
        var inline3338 int = len(self__101)
        jp1976 = inline3338
    } else {
        var inline3340 int = len(other__102)
        jp1976 = inline3340
    }
    var index__104 int = 0
    Loop_loop1981:
    for {
        var t1982 bool = index__104 < jp1976
        if t1982 {
            var t1983 int = self__101[index__104]
            var t1984 int = other__102[index__104]
            var commute_field3645 _goml_m_std_p_cmp_p_Ordering
            var inline3342 bool = t1983 < t1984
            var inline3344 _goml_m_std_p_cmp_p_Ordering
            if inline3342 {
                inline3344 = Less
            } else {
                var inline3346 bool = t1983 > t1984
                if inline3346 {
                    inline3344 = Greater
                } else {
                    inline3344 = Equal
                }
            }
            commute_field3645 = inline3344
            switch commute_field3645 {
            case Equal:
                var compound_old21 int = index__104
                var compound_value22 int = 1
                var t1987 int = compound_old21 + compound_value22
                index__104 = t1987
                continue
            default:
                var t1989 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3645,
                }
                return t1989
            }
        } else {
            break Loop_loop1981
        }
    }
    var t1978 int
    var inline3356 int = len(self__101)
    t1978 = inline3356
    var t1979 int
    var inline3354 int = len(other__102)
    t1979 = inline3354
    var inline3348 bool = t1978 < t1979
    var inline3350 _goml_m_std_p_cmp_p_Ordering
    if inline3348 {
        inline3350 = Less
    } else {
        var inline3352 bool = t1978 > t1979
        if inline3352 {
            inline3350 = Greater
        } else {
            inline3350 = Equal
        }
    }
    var inline3351 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3350,
    }
    return inline3351
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__141 *_goml_vec_int, other__142 *_goml_vec_int) bool {
    var t2005 int
    var inline3368 int = vec_len__Vec_3int(self__141)
    t2005 = inline3368
    var t2006 int
    var inline3366 int = vec_len__Vec_3int(other__142)
    t2006 = inline3366
    var t2007 bool = t2005 != t2006
    if t2007 {
        return false
    } else {
        var index__143 int = 0
        Loop_loop2009:
        for {
            var t2010 int
            var inline3364 int = vec_len__Vec_3int(self__141)
            t2010 = inline3364
            var t2011 bool = index__143 < t2010
            if t2011 {
                var t2013 int = vec_get__Vec_3int(self__141, index__143)
                var t2014 int = vec_get__Vec_3int(other__142, index__143)
                var t2015 bool
                var inline3362 bool = t2013 == t2014
                t2015 = inline3362
                if t2015 {
                    var compound_old43 int = index__143
                    var compound_value44 int = 1
                    var t2016 int = compound_old43 + compound_value44
                    index__143 = t2016
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2009
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__144 *_goml_vec_int) uint64 {
    var value__145 uint64 = 14695981039346656037
    var index__146 int = 0
    Loop_loop2021:
    for {
        var t2022 int
        var inline3372 int = vec_len__Vec_3int(self__144)
        t2022 = inline3372
        var t2023 bool = index__146 < t2022
        if t2023 {
            var t2024 uint64 = value__145 * 1099511628211
            var t2025 int = vec_get__Vec_3int(self__144, index__146)
            var t2026 uint64
            var inline3370 uint64 = _goml_runtime_core_int_hash(t2025)
            t2026 = inline3370
            var t2027 uint64 = t2024 + t2026
            value__145 = t2027
            var compound_old48 int = index__146
            var compound_value49 int = 1
            var t2028 int = compound_old48 + compound_value49
            index__146 = t2028
            continue
        } else {
            break Loop_loop2021
        }
    }
    return value__145
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2034 int = self._0
    var t2035 int = other._0
    var t2036 bool
    var inline3376 bool = t2034 == t2035
    t2036 = inline3376
    if t2036 {
        var t2039 string = self._1
        var t2040 string = other._1
        var t2041 bool
        var inline3374 bool = t2039 == t2040
        t2041 = inline3374
        if t2041 {
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
    var t2044 uint64 = _structural_hash_0 * 1099511628211
    var t2045 int = self._0
    var t2046 uint64
    var inline3380 uint64 = _goml_runtime_core_int_hash(t2045)
    t2046 = inline3380
    var _structural_hash_1 uint64 = t2044 + t2046
    var t2047 uint64 = _structural_hash_1 * 1099511628211
    var t2048 string = self._1
    var t2049 uint64
    var inline3378 uint64 = _goml_runtime_core_string_hash(t2048)
    t2049 = inline3378
    var _structural_hash_2 uint64 = t2047 + t2049
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2054 int = array_get__Array_2_3int(self, 0)
    var t2055 int = array_get__Array_2_3int(other, 0)
    var t2056 bool
    var inline3384 bool = t2054 == t2055
    t2056 = inline3384
    if t2056 {
        var t2059 int = array_get__Array_2_3int(self, 1)
        var t2060 int = array_get__Array_2_3int(other, 1)
        var t2061 bool
        var inline3382 bool = t2059 == t2060
        t2061 = inline3382
        if t2061 {
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
    var t2064 uint64 = _structural_hash_0 * 1099511628211
    var t2065 int = array_get__Array_2_3int(self, 0)
    var t2066 uint64
    var inline3388 uint64 = _goml_runtime_core_int_hash(t2065)
    t2066 = inline3388
    var _structural_hash_1 uint64 = t2064 + t2066
    var t2067 uint64 = _structural_hash_1 * 1099511628211
    var t2068 int = array_get__Array_2_3int(self, 1)
    var t2069 uint64
    var inline3386 uint64 = _goml_runtime_core_int_hash(t2068)
    t2069 = inline3386
    var _structural_hash_2 uint64 = t2067 + t2069
    return _structural_hash_2
}

func main() {
    main0()
}
