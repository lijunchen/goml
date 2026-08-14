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
    var t544 bool = self__47 < other__48
    if t544 {
        return Less
    } else {
        var t547 bool = self__47 > other__48
        if t547 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) _goml_m_Option____std_p_cmp_p_Ordering {
    var t896 int = self__2.major
    var t897 int = other__3.major
    var commute_field3402 _goml_m_std_p_cmp_p_Ordering
    var inline2483 bool = t896 < t897
    var inline2485 _goml_m_std_p_cmp_p_Ordering
    if inline2483 {
        inline2485 = Less
    } else {
        var inline2487 bool = t896 > t897
        if inline2487 {
            inline2485 = Greater
        } else {
            inline2485 = Equal
        }
    }
    commute_field3402 = inline2485
    switch commute_field3402 {
    case Equal:
        var t902 int = self__2.minor
        var t903 int = other__3.minor
        var commute_field3399 _goml_m_std_p_cmp_p_Ordering
        var inline2477 bool = t902 < t903
        var inline2479 _goml_m_std_p_cmp_p_Ordering
        if inline2477 {
            inline2479 = Less
        } else {
            var inline2481 bool = t902 > t903
            if inline2481 {
                inline2479 = Greater
            } else {
                inline2479 = Equal
            }
        }
        commute_field3399 = inline2479
        switch commute_field3399 {
        case Equal:
            var t908 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t908
        default:
            var t909 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3399,
            }
            return t909
        }
    default:
        var t910 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3402,
        }
        return t910
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2489 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2489.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2490 _goml_m_std_p_cmp_p_Ordering = inline2489.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2492 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2490, Less)
        return inline2492
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) _goml_m_std_p_cmp_p_Ordering {
    var t925 int = self__6.major
    var t926 int = other__7.major
    var _goml_m__i_derive1__ordering____8 _goml_m_std_p_cmp_p_Ordering
    var inline2516 bool = t925 < t926
    if inline2516 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2517 bool = t925 > t926
        if inline2517 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t929 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t929 = false
    case Equal:
        t929 = true
    case Greater:
        t929 = false
    default:
        panic("non-exhaustive match")
    }
    if t929 {
        var t930 int = self__6.minor
        var t931 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 _goml_m_std_p_cmp_p_Ordering
        var inline2512 bool = t930 < t931
        if inline2512 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2513 bool = t930 > t931
            if inline2513 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t934 bool
        switch _goml_m__i_derive0__ordering____9 {
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
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp970 int
    switch self__23.(type) {
    case Low:
        jp970 = 0
    case Medium:
        jp970 = 1
    case High:
        jp970 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp972 int
    switch other__24.(type) {
    case Low:
        jp972 = 0
    case Medium:
        jp972 = 1
    case High:
        jp972 = 2
    default:
        panic("non-exhaustive match")
    }
    var t975 bool = jp970 < jp972
    if t975 {
        var t976 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t976
    } else {
        var t979 bool = jp970 > jp972
        if t979 {
            var t980 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t980
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t985 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t985
                default:
                    var t986 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t986
                }
            case Medium:
                var x219 int = other__24.(Medium)._0
                var x220 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x227 int = self__23.(Medium)._0
                    var x228 int = self__23.(Medium)._1
                    var commute_field3408 _goml_m_std_p_cmp_p_Ordering
                    var inline2537 bool = x227 < x219
                    var inline2539 _goml_m_std_p_cmp_p_Ordering
                    if inline2537 {
                        inline2539 = Less
                    } else {
                        var inline2541 bool = x227 > x219
                        if inline2541 {
                            inline2539 = Greater
                        } else {
                            inline2539 = Equal
                        }
                    }
                    commute_field3408 = inline2539
                    switch commute_field3408 {
                    case Equal:
                        var commute_field3405 _goml_m_std_p_cmp_p_Ordering
                        var inline2531 bool = x228 < x220
                        var inline2533 _goml_m_std_p_cmp_p_Ordering
                        if inline2531 {
                            inline2533 = Less
                        } else {
                            var inline2535 bool = x228 > x220
                            if inline2535 {
                                inline2533 = Greater
                            } else {
                                inline2533 = Equal
                            }
                        }
                        commute_field3405 = inline2533
                        switch commute_field3405 {
                        case Equal:
                            var t997 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t997
                        default:
                            var t998 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3405,
                            }
                            return t998
                        }
                    default:
                        var t999 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3408,
                        }
                        return t999
                    }
                default:
                    var t1000 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1000
                }
            case High:
                var x221 int = other__24.(High)._0
                var x222 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x237 int = self__23.(High)._0
                    var x238 int = self__23.(High)._1
                    var commute_field3414 _goml_m_std_p_cmp_p_Ordering
                    var inline2549 bool = x237 < x221
                    var inline2551 _goml_m_std_p_cmp_p_Ordering
                    if inline2549 {
                        inline2551 = Less
                    } else {
                        var inline2553 bool = x237 > x221
                        if inline2553 {
                            inline2551 = Greater
                        } else {
                            inline2551 = Equal
                        }
                    }
                    commute_field3414 = inline2551
                    switch commute_field3414 {
                    case Equal:
                        var commute_field3411 _goml_m_std_p_cmp_p_Ordering
                        var inline2543 bool = x238 < x222
                        var inline2545 _goml_m_std_p_cmp_p_Ordering
                        if inline2543 {
                            inline2545 = Less
                        } else {
                            var inline2547 bool = x238 > x222
                            if inline2547 {
                                inline2545 = Greater
                            } else {
                                inline2545 = Equal
                            }
                        }
                        commute_field3411 = inline2545
                        switch commute_field3411 {
                        case Equal:
                            var t1011 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1011
                        default:
                            var t1012 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3411,
                            }
                            return t1012
                        }
                    default:
                        var t1013 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: commute_field3414,
                        }
                        return t1013
                    }
                default:
                    var t1014 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1014
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline2555 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2555.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline2556 _goml_m_std_p_cmp_p_Ordering = inline2555.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline2558 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline2556, Less)
        return inline2558
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) _goml_m_std_p_cmp_p_Ordering {
    var jp1030 int
    switch self__39.(type) {
    case Low:
        jp1030 = 0
    case Medium:
        jp1030 = 1
    case High:
        jp1030 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1032 int
    switch other__40.(type) {
    case Low:
        jp1032 = 0
    case Medium:
        jp1032 = 1
    case High:
        jp1032 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1035 bool = jp1030 < jp1032
    if t1035 {
        return Less
    } else {
        var t1038 bool = jp1030 > jp1032
        if t1038 {
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
                var x254 int = other__40.(Medium)._0
                var x255 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x262 int = self__39.(Medium)._0
                    var x263 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 _goml_m_std_p_cmp_p_Ordering
                    var inline2582 bool = x262 < x254
                    if inline2582 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2583 bool = x262 > x254
                        if inline2583 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1047 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1047 = false
                    case Equal:
                        t1047 = true
                    case Greater:
                        t1047 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1047 {
                        var _goml_m__i_derive4__ordering____48 _goml_m_std_p_cmp_p_Ordering
                        var inline2578 bool = x263 < x255
                        if inline2578 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2579 bool = x263 > x255
                            if inline2579 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1050 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1050 = false
                        case Equal:
                            t1050 = true
                        case Greater:
                            t1050 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1050 {
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
                var x256 int = other__40.(High)._0
                var x257 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x268 int = self__39.(High)._0
                    var x269 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x268, x256)
                    var t1055 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1055 {
                        var _goml_m__i_derive10__ordering____54 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x269, x257)
                        var t1058 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1058 = false
                        case Equal:
                            t1058 = true
                        case Greater:
                            t1058 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1058 {
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
    var t1081 float64 = self__60.value
    var t1082 float64 = other__61.value
    var inline2594 bool = t1081 == t1082
    return inline2594
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1086 float64 = self__62.value
    var t1087 float64 = other__63.value
    var commute_field3417 _goml_m_std_p_cmp_p_Ordering
    var inline2596 bool = t1086 < t1087
    if inline2596 {
        commute_field3417 = Less
        switch commute_field3417 {
        case Equal:
            var t1092 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1092
        default:
            var t1093 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3417,
            }
            return t1093
        }
    } else {
        var inline2598 bool = t1086 > t1087
        if inline2598 {
            commute_field3417 = Greater
            switch commute_field3417 {
            case Equal:
                var t1092 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                return t1092
            default:
                var t1093 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3417,
                }
                return t1093
            }
        } else {
            var inline2600 bool = t1086 == t1087
            if inline2600 {
                commute_field3417 = Equal
                switch commute_field3417 {
                case Equal:
                    var t1092 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1092
                default:
                    var t1093 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: commute_field3417,
                    }
                    return t1093
                }
            } else {
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1120 int
    switch self__95.(type) {
    case Value:
        jp1120 = 0
    case Empty:
        jp1120 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1122 int
    switch other__96.(type) {
    case Value:
        jp1122 = 0
    case Empty:
        jp1122 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1125 bool = jp1120 < jp1122
    if t1125 {
        var t1126 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1126
    } else {
        var t1129 bool = jp1120 > jp1122
        if t1129 {
            var t1130 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1130
        } else {
            switch other__96.(type) {
            case Value:
                var x306 float64 = other__96.(Value)._0
                switch self__95.(type) {
                case Value:
                    var x307 float64 = self__95.(Value)._0
                    var commute_field3420 _goml_m_std_p_cmp_p_Ordering
                    var inline2627 bool = x307 < x306
                    if inline2627 {
                        commute_field3420 = Less
                        switch commute_field3420 {
                        case Equal:
                            var t1139 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1139
                        default:
                            var t1140 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: commute_field3420,
                            }
                            return t1140
                        }
                    } else {
                        var inline2629 bool = x307 > x306
                        if inline2629 {
                            commute_field3420 = Greater
                            switch commute_field3420 {
                            case Equal:
                                var t1139 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1139
                            default:
                                var t1140 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: commute_field3420,
                                }
                                return t1140
                            }
                        } else {
                            var inline2631 bool = x307 == x306
                            if inline2631 {
                                commute_field3420 = Equal
                                switch commute_field3420 {
                                case Equal:
                                    var t1139 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1139
                                default:
                                    var t1140 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: commute_field3420,
                                    }
                                    return t1140
                                }
                            } else {
                                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
                            }
                        }
                    }
                default:
                    var t1141 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1141
                }
            case Empty:
                switch self__95.(type) {
                case Empty:
                    var t1144 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1144
                default:
                    var t1145 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1145
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
    var t1163 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1164 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1163)
    println__T_string(t1164)
    var t1165 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1166 string = ordering_name(t1165)
    println__T_string(t1166)
    var t1167 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1168 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1167)
    var t1169 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1168)
    println__T_string(t1169)
    var t1170 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1171 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1172 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1170, t1171)
    var t1173 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1172)
    println__T_string(t1173)
    var t1174 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1175 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1176 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1174, t1175)
    var t1177 string = ordering_name(t1176)
    println__T_string(t1177)
    var zero__105 float64 = 0
    var t1178 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1178,
    }
    var t1179 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1180 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1179)
    println__T_string(t1180)
    var t1181 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1182 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1181)
    var t1183 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1182)
    println__T_string(t1183)
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
    var t1184 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1184)
    println__T_string(t1185)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1186 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1187 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1186)
    println__T_string(t1187)
    var t1188 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = Value{
        _0: t1188,
    }
    var t1189 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1190 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(t1189)
    var t1191 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1190)
    println__T_string(t1191)
    var vec_literal__2131 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 2)
    var vec_literal__2178 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 3)
    var t1192 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(vec_literal__2131, vec_literal__2178)
    var t1193 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1192)
    println__T_string(t1193)
    var t1194 Option__int = Option__int_Some{
        _0: 2,
    }
    var t1195 Option__int = Option__int_Some{
        _0: 3,
    }
    var t1196 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1194, t1195)
    var t1197 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1196)
    println__T_string(t1197)
    var ok__114 Result__int__string = Ok{
        _0: 1,
    }
    var error__115 Result__int__string = Err{
        _0: "error",
    }
    var t1198 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1199 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1198)
    println__T_string(t1199)
    var t1200 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2131, 0, 2)
    var t1201 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2178, 0, 2)
    var t1202 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1200, t1201)
    var t1203 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1202)
    println__T_string(t1203)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, vec_literal__2131, "vector")
    var vec_literal__2661 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 2)
    var t1204 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, vec_literal__2661)
    var t1205 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1204, "missing")
    println__T_string(t1205)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default()
    var t1244 int = default_tuple__117._0
    var t1245 int = 0
    var t1246 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1244, t1245)
    var jp1207 bool
    if t1246 {
        var t1247 string = default_tuple__117._1
        var t1248 string = ""
        var inline2656 bool = t1247 == t1248
        jp1207 = inline2656
    } else {
        jp1207 = false
    }
    var t1208 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1207)
    println__T_string(t1208)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs339 [3]int = [3]int{0, 0, 0}
    var t1233 int = array_get__Array_3_3int(default_array__118, 0)
    var t1234 int = array_get__Array_3_3int(_eq_rhs339, 0)
    var t1235 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1233, t1234)
    var jp1210 bool
    if t1235 {
        var t1238 int = array_get__Array_3_3int(default_array__118, 1)
        var t1239 int = array_get__Array_3_3int(_eq_rhs339, 1)
        var t1240 bool
        var inline2660 bool = t1238 == t1239
        t1240 = inline2660
        if t1240 {
            var t1241 int = array_get__Array_3_3int(default_array__118, 2)
            var t1242 int = array_get__Array_3_3int(_eq_rhs339, 2)
            var inline2658 bool = t1241 == t1242
            jp1210 = inline2658
        } else {
            jp1210 = false
        }
    } else {
        jp1210 = false
    }
    var t1211 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1210)
    println__T_string(t1211)
    var t1212 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1213 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1214 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(t1212, t1213)
    var t1215 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1214)
    var inline2701 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1215)
    _goml_runtime_core_string_println(inline2701)
    var t1216 [2]int = [2]int{1, 2}
    var t1217 [2]int = [2]int{1, 3}
    var t1218 _goml_m_std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1216, t1217)
    var t1219 string = ordering_name(t1218)
    var inline2698 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1219)
    _goml_runtime_core_string_println(inline2698)
    var t1220 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_7float64_7float64 = Tuple2_7float64_7float64{
        _0: 0,
        _1: t1220,
    }
    var t1221 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1222 bool
    var inline2695 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t1221)
    var inline2696 bool = !inline2695
    t1222 = inline2696
    var t1223 string
    var inline2693 string = _goml_runtime_core_bool_to_string(t1222)
    t1223 = inline2693
    var inline2690 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1223)
    _goml_runtime_core_string_println(inline2690)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2688 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2688
    var t1224 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2685 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1224, inline2685)
    var t1225 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1226 Option__string
    var inline2683 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1225)
    t1226 = inline2683
    var t1227 string
    var inline2679 string = "missing"
    switch t1226.(type) {
    case Option__string_None:
        t1227 = inline2679
    case Option__string_Some:
        var inline2680 string = t1226.(Option__string_Some)._0
        t1227 = inline2680
    default:
        panic("non-exhaustive match")
    }
    var inline2676 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1227)
    _goml_runtime_core_string_println(inline2676)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2674 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2674
    var t1228 [2]int = [2]int{1, 2}
    var inline2671 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1228, inline2671)
    var t1229 [2]int = [2]int{1, 2}
    var t1230 Option__string
    var inline2669 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1229)
    t1230 = inline2669
    var t1231 string
    var inline2665 string = "missing"
    switch t1230.(type) {
    case Option__string_None:
        t1231 = inline2665
    case Option__string_Some:
        var inline2666 string = t1230.(Option__string_Some)._0
        t1231 = inline2666
    default:
        panic("non-exhaustive match")
    }
    var inline2662 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1231)
    _goml_runtime_core_string_println(inline2662)
    return struct{}{}
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(self__15 int, other__16 int) bool {
    var commute_field3483 _goml_m_std_p_cmp_p_Ordering
    var inline2825 bool = self__15 < other__16
    var inline2827 _goml_m_std_p_cmp_p_Ordering
    if inline2825 {
        inline2827 = Less
    } else {
        var inline2829 bool = self__15 > other__16
        if inline2829 {
            inline2827 = Greater
        } else {
            inline2827 = Equal
        }
    }
    commute_field3483 = inline2827
    switch commute_field3483 {
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
    var t1626 bool = self__101 == other__102
    return t1626
}

func println__T_string(value__1 string) struct{} {
    var t1722 string
    t1722 = value__1
    _goml_runtime_core_string_println(t1722)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1726 string = _goml_runtime_core_bool_to_string(self__64)
    return t1726
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__std_p_cmp_p_Ordering(self__296 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    var t1729 bool
    switch self__296.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        t1729 = false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        t1729 = true
    default:
        panic("non-exhaustive match")
    }
    var t1730 bool = !t1729
    return t1730
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1733 *_goml_vec_int = vec_new__Vec_3int()
    return t1733
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3220 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3220.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        var inline3221 _goml_m_std_p_cmp_p_Ordering = inline3220.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
        var inline3223 bool = _goml_m_trait__impl_i_PartialEq_i_std_p_cmp_p_Ordering_i_eq(inline3221, Less)
        return inline3223
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3225 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3230 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3235 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3240 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__227 *_goml_vec_int, start__228 int, end__229 int) []int {
    var t1753 []int = self__227.items[start__228:end__229]
    return t1753
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3245 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1759 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1759
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__261 *hashmap_Vec_3int_string_x, key__262 *_goml_vec_int, value__263 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__261, key__262, value__263)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__259 *hashmap_Vec_3int_string_x, key__260 *_goml_vec_int) Option__string {
    var t1764 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__259, key__260)
    return t1764
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__297 Option__string, fallback__298 string) string {
    switch self__297.(type) {
    case Option__string_None:
        return fallback__298
    case Option__string_Some:
        var x161 string = self__297.(Option__string_Some)._0
        return x161
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default() Tuple2_3int_6string {
    var t1771 int
    t1771 = 0
    var t1772 string
    t1772 = ""
    var t1773 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1771,
        _1: t1772,
    }
    return t1773
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1779 int
    t1779 = 0
    var t1780 int
    t1780 = 0
    var t1781 int
    t1781 = 0
    var t1782 [3]int = [3]int{t1779, t1780, t1781}
    return t1782
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1787 int = self._0
    var t1788 int = other._0
    var t1789 bool
    var inline3261 bool = t1787 == t1788
    t1789 = inline3261
    if t1789 {
        var t1792 int = self._1
        var t1793 int = other._1
        var t1794 bool
        var inline3257 bool = t1792 == t1793
        t1794 = inline3257
        if t1794 {
            return false
        } else {
            var t1795 int = self._1
            var t1796 int = other._1
            var inline3255 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1795, t1796)
            return inline3255
        }
    } else {
        var t1798 int = self._0
        var t1799 int = other._0
        var inline3259 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1798, t1799)
        return inline3259
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) _goml_m_std_p_cmp_p_Ordering {
    var t1803 int = array_get__Array_2_3int(self, 0)
    var t1804 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 _goml_m_std_p_cmp_p_Ordering
    var inline3268 bool = t1803 < t1804
    if inline3268 {
        _structural_ordering_0 = Less
    } else {
        var inline3269 bool = t1803 > t1804
        if inline3269 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1807 bool
    switch _structural_ordering_0 {
    case Less:
        t1807 = false
    case Equal:
        t1807 = true
    case Greater:
        t1807 = false
    default:
        panic("non-exhaustive match")
    }
    if t1807 {
        var t1808 int = array_get__Array_2_3int(self, 1)
        var t1809 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 _goml_m_std_p_cmp_p_Ordering
        var inline3264 bool = t1808 < t1809
        if inline3264 {
            _structural_ordering_1 = Less
        } else {
            var inline3265 bool = t1808 > t1809
            if inline3265 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1812 bool
        switch _structural_ordering_1 {
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
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_haa34d8491754c3aa6b5371402ef010cd__i_partial__cmp(self Tuple2_7float64_7float64, other Tuple2_7float64_7float64) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1815 float64 = self._0
    var t1816 float64 = other._0
    var _structural_partial_ordering_0 _goml_m_Option____std_p_cmp_p_Ordering
    var commute_field3642 _goml_m_std_p_cmp_p_Ordering
    var inline3280 bool = t1815 < t1816
    if inline3280 {
        var inline3281 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        _structural_partial_ordering_0 = inline3281
        commute_field3642 = Less
        var t1821 bool
        switch commute_field3642 {
        case Less:
            t1821 = false
        case Equal:
            t1821 = true
        case Greater:
            t1821 = false
        default:
            panic("non-exhaustive match")
        }
        if t1821 {
            var t1822 float64 = self._1
            var t1823 float64 = other._1
            var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
            var commute_field3639 _goml_m_std_p_cmp_p_Ordering
            var inline3272 bool = t1822 < t1823
            if inline3272 {
                var inline3273 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Less,
                }
                _structural_partial_ordering_1 = inline3273
                commute_field3639 = Less
                var t1828 bool
                switch commute_field3639 {
                case Less:
                    t1828 = false
                case Equal:
                    t1828 = true
                case Greater:
                    t1828 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1828 {
                    var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1829
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3274 bool = t1822 > t1823
                if inline3274 {
                    var inline3275 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Greater,
                    }
                    _structural_partial_ordering_1 = inline3275
                    commute_field3639 = Greater
                    var t1828 bool
                    switch commute_field3639 {
                    case Less:
                        t1828 = false
                    case Equal:
                        t1828 = true
                    case Greater:
                        t1828 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1828 {
                        var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1829
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3276 bool = t1822 == t1823
                    if inline3276 {
                        var inline3277 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        _structural_partial_ordering_1 = inline3277
                        commute_field3639 = Equal
                        var t1828 bool
                        switch commute_field3639 {
                        case Less:
                            t1828 = false
                        case Equal:
                            t1828 = true
                        case Greater:
                            t1828 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1828 {
                            var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1829
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
        var inline3282 bool = t1815 > t1816
        if inline3282 {
            var inline3283 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            _structural_partial_ordering_0 = inline3283
            commute_field3642 = Greater
            var t1821 bool
            switch commute_field3642 {
            case Less:
                t1821 = false
            case Equal:
                t1821 = true
            case Greater:
                t1821 = false
            default:
                panic("non-exhaustive match")
            }
            if t1821 {
                var t1822 float64 = self._1
                var t1823 float64 = other._1
                var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                var commute_field3639 _goml_m_std_p_cmp_p_Ordering
                var inline3272 bool = t1822 < t1823
                if inline3272 {
                    var inline3273 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Less,
                    }
                    _structural_partial_ordering_1 = inline3273
                    commute_field3639 = Less
                    var t1828 bool
                    switch commute_field3639 {
                    case Less:
                        t1828 = false
                    case Equal:
                        t1828 = true
                    case Greater:
                        t1828 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1828 {
                        var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Equal,
                        }
                        return t1829
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    var inline3274 bool = t1822 > t1823
                    if inline3274 {
                        var inline3275 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Greater,
                        }
                        _structural_partial_ordering_1 = inline3275
                        commute_field3639 = Greater
                        var t1828 bool
                        switch commute_field3639 {
                        case Less:
                            t1828 = false
                        case Equal:
                            t1828 = true
                        case Greater:
                            t1828 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1828 {
                            var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1829
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3276 bool = t1822 == t1823
                        if inline3276 {
                            var inline3277 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            _structural_partial_ordering_1 = inline3277
                            commute_field3639 = Equal
                            var t1828 bool
                            switch commute_field3639 {
                            case Less:
                                t1828 = false
                            case Equal:
                                t1828 = true
                            case Greater:
                                t1828 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1828 {
                                var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1829
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
            var inline3284 bool = t1815 == t1816
            if inline3284 {
                var inline3285 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                _structural_partial_ordering_0 = inline3285
                commute_field3642 = Equal
                var t1821 bool
                switch commute_field3642 {
                case Less:
                    t1821 = false
                case Equal:
                    t1821 = true
                case Greater:
                    t1821 = false
                default:
                    panic("non-exhaustive match")
                }
                if t1821 {
                    var t1822 float64 = self._1
                    var t1823 float64 = other._1
                    var _structural_partial_ordering_1 _goml_m_Option____std_p_cmp_p_Ordering
                    var commute_field3639 _goml_m_std_p_cmp_p_Ordering
                    var inline3272 bool = t1822 < t1823
                    if inline3272 {
                        var inline3273 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                            _0: Less,
                        }
                        _structural_partial_ordering_1 = inline3273
                        commute_field3639 = Less
                        var t1828 bool
                        switch commute_field3639 {
                        case Less:
                            t1828 = false
                        case Equal:
                            t1828 = true
                        case Greater:
                            t1828 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1828 {
                            var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Equal,
                            }
                            return t1829
                        } else {
                            return _structural_partial_ordering_1
                        }
                    } else {
                        var inline3274 bool = t1822 > t1823
                        if inline3274 {
                            var inline3275 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                _0: Greater,
                            }
                            _structural_partial_ordering_1 = inline3275
                            commute_field3639 = Greater
                            var t1828 bool
                            switch commute_field3639 {
                            case Less:
                                t1828 = false
                            case Equal:
                                t1828 = true
                            case Greater:
                                t1828 = false
                            default:
                                panic("non-exhaustive match")
                            }
                            if t1828 {
                                var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                return t1829
                            } else {
                                return _structural_partial_ordering_1
                            }
                        } else {
                            var inline3276 bool = t1822 == t1823
                            if inline3276 {
                                var inline3277 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                    _0: Equal,
                                }
                                _structural_partial_ordering_1 = inline3277
                                commute_field3639 = Equal
                                var t1828 bool
                                switch commute_field3639 {
                                case Less:
                                    t1828 = false
                                case Equal:
                                    t1828 = true
                                case Greater:
                                    t1828 = false
                                default:
                                    panic("non-exhaustive match")
                                }
                                if t1828 {
                                    var t1829 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1829
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

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__295 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__295.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(self__67 GenericPair__int, other__68 GenericPair__int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1891 int = self__67.first
    var t1892 int = other__68.first
    var commute_field3648 _goml_m_std_p_cmp_p_Ordering
    var inline3299 bool = t1891 < t1892
    var inline3301 _goml_m_std_p_cmp_p_Ordering
    if inline3299 {
        inline3301 = Less
    } else {
        var inline3303 bool = t1891 > t1892
        if inline3303 {
            inline3301 = Greater
        } else {
            inline3301 = Equal
        }
    }
    commute_field3648 = inline3301
    switch commute_field3648 {
    case Equal:
        var t1897 int = self__67.second
        var t1898 int = other__68.second
        var commute_field3645 _goml_m_std_p_cmp_p_Ordering
        var inline3293 bool = t1897 < t1898
        var inline3295 _goml_m_std_p_cmp_p_Ordering
        if inline3293 {
            inline3295 = Less
        } else {
            var inline3297 bool = t1897 > t1898
            if inline3297 {
                inline3295 = Greater
            } else {
                inline3295 = Equal
            }
        }
        commute_field3645 = inline3295
        switch commute_field3645 {
        case Equal:
            var t1903 *_goml_vec_int = self__67.nested
            var t1904 *_goml_vec_int = other__68.nested
            var mtmp280 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t1903, t1904)
            switch mtmp280.(type) {
            case _goml_m_Option____std_p_cmp_p_Ordering_None:
                return _goml_m_Option____std_p_cmp_p_Ordering_None{}
            case _goml_m_Option____std_p_cmp_p_Ordering_Some:
                var x281 _goml_m_std_p_cmp_p_Ordering = mtmp280.(_goml_m_Option____std_p_cmp_p_Ordering_Some)._0
                switch x281 {
                case Equal:
                    var t1909 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1909
                default:
                    var t1910 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: x281,
                    }
                    return t1910
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1911 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: commute_field3645,
            }
            return t1911
        }
    default:
        var t1912 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: commute_field3648,
        }
        return t1912
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) _goml_m_Option____std_p_cmp_p_Ordering {
    var jp1916 int
    switch self__83 {
    case First:
        jp1916 = 0
    case Second:
        jp1916 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1918 int
    switch other__84 {
    case First:
        jp1918 = 0
    case Second:
        jp1918 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1921 bool = jp1916 < jp1918
    if t1921 {
        var t1922 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        return t1922
    } else {
        var t1925 bool = jp1916 > jp1918
        if t1925 {
            var t1926 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1926
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
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
            case Second:
                switch self__83 {
                case Second:
                    var t1935 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1935
                default:
                    var t1936 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                        _0: Equal,
                    }
                    return t1936
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__91 *_goml_vec_int, other__92 *_goml_vec_int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t1954 int
    var inline3327 int = vec_len__Vec_3int(self__91)
    t1954 = inline3327
    var t1955 int
    var inline3325 int = vec_len__Vec_3int(other__92)
    t1955 = inline3325
    var t1956 bool = t1954 < t1955
    var jp1940 int
    if t1956 {
        var inline3305 int = vec_len__Vec_3int(self__91)
        jp1940 = inline3305
    } else {
        var inline3307 int = vec_len__Vec_3int(other__92)
        jp1940 = inline3307
    }
    var index__94 int = 0
    Loop_loop1945:
    for {
        var t1946 bool = index__94 < jp1940
        if t1946 {
            var t1947 int = vec_get__Vec_3int(self__91, index__94)
            var t1948 int = vec_get__Vec_3int(other__92, index__94)
            var commute_field3651 _goml_m_std_p_cmp_p_Ordering
            var inline3309 bool = t1947 < t1948
            var inline3311 _goml_m_std_p_cmp_p_Ordering
            if inline3309 {
                inline3311 = Less
            } else {
                var inline3313 bool = t1947 > t1948
                if inline3313 {
                    inline3311 = Greater
                } else {
                    inline3311 = Equal
                }
            }
            commute_field3651 = inline3311
            switch commute_field3651 {
            case Equal:
                var compound_old10 int = index__94
                var compound_value11 int = 1
                var t1951 int = compound_old10 + compound_value11
                index__94 = t1951
                continue
            default:
                var t1953 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3651,
                }
                return t1953
            }
        } else {
            break Loop_loop1945
        }
    }
    var t1942 int
    var inline3323 int = vec_len__Vec_3int(self__91)
    t1942 = inline3323
    var t1943 int
    var inline3321 int = vec_len__Vec_3int(other__92)
    t1943 = inline3321
    var inline3315 bool = t1942 < t1943
    var inline3317 _goml_m_std_p_cmp_p_Ordering
    if inline3315 {
        inline3317 = Less
    } else {
        var inline3319 bool = t1942 > t1943
        if inline3319 {
            inline3317 = Greater
        } else {
            inline3317 = Equal
        }
    }
    var inline3318 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3317,
    }
    return inline3318
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__111 Option__int, other__112 Option__int) _goml_m_Option____std_p_cmp_p_Ordering {
    switch other__112.(type) {
    case Option__int_None:
        switch self__111.(type) {
        case Option__int_None:
            var t1965 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Equal,
            }
            return t1965
        case Option__int_Some:
            var t1966 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1966
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__112.(Option__int_Some)._0
        switch self__111.(type) {
        case Option__int_None:
            var t1969 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1969
        case Option__int_Some:
            var x35 int = self__111.(Option__int_Some)._0
            var inline3329 bool = x35 < x33
            var inline3331 _goml_m_std_p_cmp_p_Ordering
            if inline3329 {
                inline3331 = Less
            } else {
                var inline3333 bool = x35 > x33
                if inline3333 {
                    inline3331 = Greater
                } else {
                    inline3331 = Equal
                }
            }
            var inline3332 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: inline3331,
            }
            return inline3332
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
            var inline3335 bool = x47 < x45
            var inline3337 _goml_m_std_p_cmp_p_Ordering
            if inline3335 {
                inline3337 = Less
            } else {
                var inline3339 bool = x47 > x45
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
        case Err:
            var t1978 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            return t1978
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__120.(Err)._0
        switch self__119.(type) {
        case Ok:
            var t1981 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Less,
            }
            return t1981
        case Err:
            var x50 string = self__119.(Err)._0
            var inline3341 bool = x50 < x46
            var inline3343 _goml_m_std_p_cmp_p_Ordering
            if inline3341 {
                inline3343 = Less
            } else {
                var inline3345 bool = x50 > x46
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
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__101 []int, other__102 []int) _goml_m_Option____std_p_cmp_p_Ordering {
    var t2000 int
    var inline3369 int = len(self__101)
    t2000 = inline3369
    var t2001 int
    var inline3367 int = len(other__102)
    t2001 = inline3367
    var t2002 bool = t2000 < t2001
    var jp1986 int
    if t2002 {
        var inline3347 int = len(self__101)
        jp1986 = inline3347
    } else {
        var inline3349 int = len(other__102)
        jp1986 = inline3349
    }
    var index__104 int = 0
    Loop_loop1991:
    for {
        var t1992 bool = index__104 < jp1986
        if t1992 {
            var t1993 int = self__101[index__104]
            var t1994 int = other__102[index__104]
            var commute_field3654 _goml_m_std_p_cmp_p_Ordering
            var inline3351 bool = t1993 < t1994
            var inline3353 _goml_m_std_p_cmp_p_Ordering
            if inline3351 {
                inline3353 = Less
            } else {
                var inline3355 bool = t1993 > t1994
                if inline3355 {
                    inline3353 = Greater
                } else {
                    inline3353 = Equal
                }
            }
            commute_field3654 = inline3353
            switch commute_field3654 {
            case Equal:
                var compound_old21 int = index__104
                var compound_value22 int = 1
                var t1997 int = compound_old21 + compound_value22
                index__104 = t1997
                continue
            default:
                var t1999 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: commute_field3654,
                }
                return t1999
            }
        } else {
            break Loop_loop1991
        }
    }
    var t1988 int
    var inline3365 int = len(self__101)
    t1988 = inline3365
    var t1989 int
    var inline3363 int = len(other__102)
    t1989 = inline3363
    var inline3357 bool = t1988 < t1989
    var inline3359 _goml_m_std_p_cmp_p_Ordering
    if inline3357 {
        inline3359 = Less
    } else {
        var inline3361 bool = t1988 > t1989
        if inline3361 {
            inline3359 = Greater
        } else {
            inline3359 = Equal
        }
    }
    var inline3360 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
        _0: inline3359,
    }
    return inline3360
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__139 *_goml_vec_int, other__140 *_goml_vec_int) bool {
    var t2015 int
    var inline3377 int = vec_len__Vec_3int(self__139)
    t2015 = inline3377
    var t2016 int
    var inline3375 int = vec_len__Vec_3int(other__140)
    t2016 = inline3375
    var t2017 bool = t2015 != t2016
    if t2017 {
        return false
    } else {
        var index__141 int = 0
        Loop_loop2019:
        for {
            var t2020 int
            var inline3373 int = vec_len__Vec_3int(self__139)
            t2020 = inline3373
            var t2021 bool = index__141 < t2020
            if t2021 {
                var t2023 int = vec_get__Vec_3int(self__139, index__141)
                var t2024 int = vec_get__Vec_3int(other__140, index__141)
                var t2025 bool
                var inline3371 bool = t2023 == t2024
                t2025 = inline3371
                if t2025 {
                    var compound_old43 int = index__141
                    var compound_value44 int = 1
                    var t2026 int = compound_old43 + compound_value44
                    index__141 = t2026
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2019
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__142 *_goml_vec_int) uint64 {
    var value__143 uint64 = 14695981039346656037
    var index__144 int = 0
    Loop_loop2031:
    for {
        var t2032 int
        var inline3381 int = vec_len__Vec_3int(self__142)
        t2032 = inline3381
        var t2033 bool = index__144 < t2032
        if t2033 {
            var t2034 uint64 = value__143 * 1099511628211
            var t2035 int = vec_get__Vec_3int(self__142, index__144)
            var t2036 uint64
            var inline3379 uint64 = _goml_runtime_core_int_hash(t2035)
            t2036 = inline3379
            var t2037 uint64 = t2034 + t2036
            value__143 = t2037
            var compound_old48 int = index__144
            var compound_value49 int = 1
            var t2038 int = compound_old48 + compound_value49
            index__144 = t2038
            continue
        } else {
            break Loop_loop2031
        }
    }
    return value__143
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2044 int = self._0
    var t2045 int = other._0
    var t2046 bool
    var inline3385 bool = t2044 == t2045
    t2046 = inline3385
    if t2046 {
        var t2049 string = self._1
        var t2050 string = other._1
        var t2051 bool
        var inline3383 bool = t2049 == t2050
        t2051 = inline3383
        if t2051 {
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
    var t2054 uint64 = _structural_hash_0 * 1099511628211
    var t2055 int = self._0
    var t2056 uint64
    var inline3389 uint64 = _goml_runtime_core_int_hash(t2055)
    t2056 = inline3389
    var _structural_hash_1 uint64 = t2054 + t2056
    var t2057 uint64 = _structural_hash_1 * 1099511628211
    var t2058 string = self._1
    var t2059 uint64
    var inline3387 uint64 = _goml_runtime_core_string_hash(t2058)
    t2059 = inline3387
    var _structural_hash_2 uint64 = t2057 + t2059
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2064 int = array_get__Array_2_3int(self, 0)
    var t2065 int = array_get__Array_2_3int(other, 0)
    var t2066 bool
    var inline3393 bool = t2064 == t2065
    t2066 = inline3393
    if t2066 {
        var t2069 int = array_get__Array_2_3int(self, 1)
        var t2070 int = array_get__Array_2_3int(other, 1)
        var t2071 bool
        var inline3391 bool = t2069 == t2070
        t2071 = inline3391
        if t2071 {
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
    var t2074 uint64 = _structural_hash_0 * 1099511628211
    var t2075 int = array_get__Array_2_3int(self, 0)
    var t2076 uint64
    var inline3397 uint64 = _goml_runtime_core_int_hash(t2075)
    t2076 = inline3397
    var _structural_hash_1 uint64 = t2074 + t2076
    var t2077 uint64 = _structural_hash_1 * 1099511628211
    var t2078 int = array_get__Array_2_3int(self, 1)
    var t2079 uint64
    var inline3395 uint64 = _goml_runtime_core_int_hash(t2078)
    t2079 = inline3395
    var _structural_hash_2 uint64 = t2077 + t2079
    return _structural_hash_2
}

func main() {
    main0()
}
