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

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
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

type Tuple2_3int_7float64 struct {
    _0 int
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

type Ordering int32

const (
    Less Ordering = 0
    Equal Ordering = 1
    Greater Ordering = 2
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

type Option__Ordering interface {
    isOption__Ordering()
}

type Option__Ordering_None struct {}

func (_ Option__Ordering_None) isOption__Ordering() {}

type Option__Ordering_Some struct {
    _0 Ordering
}

func (_ Option__Ordering_Some) isOption__Ordering() {}

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

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__32 int, other__33 int) Ordering {
    var t704 bool = self__32 < other__33
    if t704 {
        return Less
    } else {
        var t707 bool = self__32 > other__33
        if t707 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) Option__Ordering {
    var t1056 int = self__2.major
    var t1057 int = other__3.major
    var commute_field3566 Ordering
    var inline2648 bool = t1056 < t1057
    var inline2650 Ordering
    if inline2648 {
        inline2650 = Less
    } else {
        var inline2652 bool = t1056 > t1057
        if inline2652 {
            inline2650 = Greater
        } else {
            inline2650 = Equal
        }
    }
    commute_field3566 = inline2650
    switch commute_field3566 {
    case Equal:
        var t1062 int = self__2.minor
        var t1063 int = other__3.minor
        var commute_field3563 Ordering
        var inline2642 bool = t1062 < t1063
        var inline2644 Ordering
        if inline2642 {
            inline2644 = Less
        } else {
            var inline2646 bool = t1062 > t1063
            if inline2646 {
                inline2644 = Greater
            } else {
                inline2644 = Equal
            }
        }
        commute_field3563 = inline2644
        switch commute_field3563 {
        case Equal:
            var t1068 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t1068
        default:
            var t1069 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3563,
            }
            return t1069
        }
    default:
        var t1070 Option__Ordering = Option__Ordering_Some{
            _0: commute_field3566,
        }
        return t1070
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2654 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2654.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline2655 Ordering = inline2654.(Option__Ordering_Some)._0
        var inline2657 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2655, Less)
        return inline2657
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) Ordering {
    var t1085 int = self__6.major
    var t1086 int = other__7.major
    var _goml_m__i_derive1__ordering____8 Ordering
    var inline2681 bool = t1085 < t1086
    if inline2681 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2682 bool = t1085 > t1086
        if inline2682 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t1089 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t1089 = false
    case Equal:
        t1089 = true
    case Greater:
        t1089 = false
    default:
        panic("non-exhaustive match")
    }
    if t1089 {
        var t1090 int = self__6.minor
        var t1091 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 Ordering
        var inline2677 bool = t1090 < t1091
        if inline2677 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2678 bool = t1090 > t1091
            if inline2678 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t1094 bool
        switch _goml_m__i_derive0__ordering____9 {
        case Less:
            t1094 = false
        case Equal:
            t1094 = true
        case Greater:
            t1094 = false
        default:
            panic("non-exhaustive match")
        }
        if t1094 {
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) Option__Ordering {
    var jp1130 int
    switch self__23.(type) {
    case Low:
        jp1130 = 0
    case Medium:
        jp1130 = 1
    case High:
        jp1130 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1132 int
    switch other__24.(type) {
    case Low:
        jp1132 = 0
    case Medium:
        jp1132 = 1
    case High:
        jp1132 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1135 bool = jp1130 < jp1132
    if t1135 {
        var t1136 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t1136
    } else {
        var t1139 bool = jp1130 > jp1132
        if t1139 {
            var t1140 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t1140
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t1145 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1145
                default:
                    var t1146 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1146
                }
            case Medium:
                var x445 int = other__24.(Medium)._0
                var x446 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x453 int = self__23.(Medium)._0
                    var x454 int = self__23.(Medium)._1
                    var commute_field3572 Ordering
                    var inline2702 bool = x453 < x445
                    var inline2704 Ordering
                    if inline2702 {
                        inline2704 = Less
                    } else {
                        var inline2706 bool = x453 > x445
                        if inline2706 {
                            inline2704 = Greater
                        } else {
                            inline2704 = Equal
                        }
                    }
                    commute_field3572 = inline2704
                    switch commute_field3572 {
                    case Equal:
                        var commute_field3569 Ordering
                        var inline2696 bool = x454 < x446
                        var inline2698 Ordering
                        if inline2696 {
                            inline2698 = Less
                        } else {
                            var inline2700 bool = x454 > x446
                            if inline2700 {
                                inline2698 = Greater
                            } else {
                                inline2698 = Equal
                            }
                        }
                        commute_field3569 = inline2698
                        switch commute_field3569 {
                        case Equal:
                            var t1157 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1157
                        default:
                            var t1158 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3569,
                            }
                            return t1158
                        }
                    default:
                        var t1159 Option__Ordering = Option__Ordering_Some{
                            _0: commute_field3572,
                        }
                        return t1159
                    }
                default:
                    var t1160 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1160
                }
            case High:
                var x447 int = other__24.(High)._0
                var x448 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x463 int = self__23.(High)._0
                    var x464 int = self__23.(High)._1
                    var commute_field3578 Ordering
                    var inline2714 bool = x463 < x447
                    var inline2716 Ordering
                    if inline2714 {
                        inline2716 = Less
                    } else {
                        var inline2718 bool = x463 > x447
                        if inline2718 {
                            inline2716 = Greater
                        } else {
                            inline2716 = Equal
                        }
                    }
                    commute_field3578 = inline2716
                    switch commute_field3578 {
                    case Equal:
                        var commute_field3575 Ordering
                        var inline2708 bool = x464 < x448
                        var inline2710 Ordering
                        if inline2708 {
                            inline2710 = Less
                        } else {
                            var inline2712 bool = x464 > x448
                            if inline2712 {
                                inline2710 = Greater
                            } else {
                                inline2710 = Equal
                            }
                        }
                        commute_field3575 = inline2710
                        switch commute_field3575 {
                        case Equal:
                            var t1171 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1171
                        default:
                            var t1172 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3575,
                            }
                            return t1172
                        }
                    default:
                        var t1173 Option__Ordering = Option__Ordering_Some{
                            _0: commute_field3578,
                        }
                        return t1173
                    }
                default:
                    var t1174 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1174
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline2720 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2720.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline2721 Ordering = inline2720.(Option__Ordering_Some)._0
        var inline2723 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2721, Less)
        return inline2723
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) Ordering {
    var jp1190 int
    switch self__39.(type) {
    case Low:
        jp1190 = 0
    case Medium:
        jp1190 = 1
    case High:
        jp1190 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1192 int
    switch other__40.(type) {
    case Low:
        jp1192 = 0
    case Medium:
        jp1192 = 1
    case High:
        jp1192 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1195 bool = jp1190 < jp1192
    if t1195 {
        return Less
    } else {
        var t1198 bool = jp1190 > jp1192
        if t1198 {
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
                var x480 int = other__40.(Medium)._0
                var x481 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x488 int = self__39.(Medium)._0
                    var x489 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 Ordering
                    var inline2747 bool = x488 < x480
                    if inline2747 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2748 bool = x488 > x480
                        if inline2748 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1207 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1207 = false
                    case Equal:
                        t1207 = true
                    case Greater:
                        t1207 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1207 {
                        var _goml_m__i_derive4__ordering____48 Ordering
                        var inline2743 bool = x489 < x481
                        if inline2743 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2744 bool = x489 > x481
                            if inline2744 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1210 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1210 = false
                        case Equal:
                            t1210 = true
                        case Greater:
                            t1210 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1210 {
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
                var x482 int = other__40.(High)._0
                var x483 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x494 int = self__39.(High)._0
                    var x495 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x494, x482)
                    var t1215 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1215 {
                        var _goml_m__i_derive10__ordering____54 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x495, x483)
                        var t1218 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1218 = false
                        case Equal:
                            t1218 = true
                        case Greater:
                            t1218 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1218 {
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
    var t1241 float64 = self__60.value
    var t1242 float64 = other__61.value
    var inline2759 bool = t1241 == t1242
    return inline2759
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) Option__Ordering {
    var t1246 float64 = self__62.value
    var t1247 float64 = other__63.value
    var commute_field3581 Ordering
    var inline2761 bool = t1246 < t1247
    if inline2761 {
        commute_field3581 = Less
        switch commute_field3581 {
        case Equal:
            var t1252 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t1252
        default:
            var t1253 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3581,
            }
            return t1253
        }
    } else {
        var inline2763 bool = t1246 > t1247
        if inline2763 {
            commute_field3581 = Greater
            switch commute_field3581 {
            case Equal:
                var t1252 Option__Ordering = Option__Ordering_Some{
                    _0: Equal,
                }
                return t1252
            default:
                var t1253 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3581,
                }
                return t1253
            }
        } else {
            var inline2765 bool = t1246 == t1247
            if inline2765 {
                commute_field3581 = Equal
                switch commute_field3581 {
                case Equal:
                    var t1252 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1252
                default:
                    var t1253 Option__Ordering = Option__Ordering_Some{
                        _0: commute_field3581,
                    }
                    return t1253
                }
            } else {
                return Option__Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) Option__Ordering {
    var jp1280 int
    switch self__95.(type) {
    case Value:
        jp1280 = 0
    case Empty:
        jp1280 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1282 int
    switch other__96.(type) {
    case Value:
        jp1282 = 0
    case Empty:
        jp1282 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1285 bool = jp1280 < jp1282
    if t1285 {
        var t1286 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t1286
    } else {
        var t1289 bool = jp1280 > jp1282
        if t1289 {
            var t1290 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t1290
        } else {
            switch other__96.(type) {
            case Value:
                var x532 float64 = other__96.(Value)._0
                switch self__95.(type) {
                case Value:
                    var x533 float64 = self__95.(Value)._0
                    var commute_field3584 Ordering
                    var inline2792 bool = x533 < x532
                    if inline2792 {
                        commute_field3584 = Less
                        switch commute_field3584 {
                        case Equal:
                            var t1299 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1299
                        default:
                            var t1300 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3584,
                            }
                            return t1300
                        }
                    } else {
                        var inline2794 bool = x533 > x532
                        if inline2794 {
                            commute_field3584 = Greater
                            switch commute_field3584 {
                            case Equal:
                                var t1299 Option__Ordering = Option__Ordering_Some{
                                    _0: Equal,
                                }
                                return t1299
                            default:
                                var t1300 Option__Ordering = Option__Ordering_Some{
                                    _0: commute_field3584,
                                }
                                return t1300
                            }
                        } else {
                            var inline2796 bool = x533 == x532
                            if inline2796 {
                                commute_field3584 = Equal
                                switch commute_field3584 {
                                case Equal:
                                    var t1299 Option__Ordering = Option__Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1299
                                default:
                                    var t1300 Option__Ordering = Option__Ordering_Some{
                                        _0: commute_field3584,
                                    }
                                    return t1300
                                }
                            } else {
                                return Option__Ordering_None{}
                            }
                        }
                    }
                default:
                    var t1301 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1301
                }
            case Empty:
                switch self__95.(type) {
                case Empty:
                    var t1304 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1304
                default:
                    var t1305 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1305
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func ordering_name(value__102 Ordering) string {
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
    var t1323 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1324 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1323)
    println__T_string(t1324)
    var t1325 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1326 string = ordering_name(t1325)
    println__T_string(t1326)
    var t1327 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1328 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1327)
    var t1329 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1328)
    println__T_string(t1329)
    var t1330 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1331 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1332 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1330, t1331)
    var t1333 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1332)
    println__T_string(t1333)
    var t1334 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1335 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1336 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1334, t1335)
    var t1337 string = ordering_name(t1336)
    println__T_string(t1337)
    var zero__105 float64 = 0
    var t1338 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1338,
    }
    var t1339 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1340 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1339)
    println__T_string(t1340)
    var t1341 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1342 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1341)
    var t1343 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1342)
    println__T_string(t1343)
    var t1344 [1]int = [1]int{3}
    var t1345 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1344)
    var generic_first__107 GenericPair__int = GenericPair__int{
        first: 1,
        second: 2,
        nested: t1345,
    }
    var t1346 [1]int = [1]int{0}
    var t1347 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1346)
    var generic_second__108 GenericPair__int = GenericPair__int{
        first: 1,
        second: 3,
        nested: t1347,
    }
    var t1348 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1349 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1348)
    println__T_string(t1349)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1350 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1351 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1350)
    println__T_string(t1351)
    var t1352 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = Value{
        _0: t1352,
    }
    var t1353 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1354 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1353)
    var t1355 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1354)
    println__T_string(t1355)
    var t1356 [2]int = [2]int{1, 2}
    var first_values__112 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1356)
    var t1357 [2]int = [2]int{1, 3}
    var second_values__113 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1357)
    var t1358 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(first_values__112, second_values__113)
    var t1359 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1358)
    println__T_string(t1359)
    var t1360 Option__int = Option__int_Some{
        _0: 2,
    }
    var t1361 Option__int = Option__int_Some{
        _0: 3,
    }
    var t1362 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1360, t1361)
    var t1363 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1362)
    println__T_string(t1363)
    var ok__114 Result__int__string = Ok{
        _0: 1,
    }
    var error__115 Result__int__string = Err{
        _0: "error",
    }
    var t1364 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1365 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1364)
    println__T_string(t1365)
    var t1366 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(first_values__112, 0, 2)
    var t1367 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(second_values__113, 0, 2)
    var t1368 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1366, t1367)
    var t1369 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1368)
    println__T_string(t1369)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, first_values__112, "vector")
    var t1370 [2]int = [2]int{1, 2}
    var t1371 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1370)
    var t1372 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, t1371)
    var t1373 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1372, "missing")
    println__T_string(t1373)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default()
    var t1412 int = default_tuple__117._0
    var t1413 int = 0
    var t1414 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1412, t1413)
    var jp1375 bool
    if t1414 {
        var t1415 string = default_tuple__117._1
        var t1416 string = ""
        var inline2821 bool = t1415 == t1416
        jp1375 = inline2821
    } else {
        jp1375 = false
    }
    var t1376 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1375)
    println__T_string(t1376)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs557 [3]int = [3]int{0, 0, 0}
    var t1401 int = array_get__Array_3_3int(default_array__118, 0)
    var t1402 int = array_get__Array_3_3int(_eq_rhs557, 0)
    var t1403 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1401, t1402)
    var jp1378 bool
    if t1403 {
        var t1406 int = array_get__Array_3_3int(default_array__118, 1)
        var t1407 int = array_get__Array_3_3int(_eq_rhs557, 1)
        var t1408 bool
        var inline2825 bool = t1406 == t1407
        t1408 = inline2825
        if t1408 {
            var t1409 int = array_get__Array_3_3int(default_array__118, 2)
            var t1410 int = array_get__Array_3_3int(_eq_rhs557, 2)
            var inline2823 bool = t1409 == t1410
            jp1378 = inline2823
        } else {
            jp1378 = false
        }
    } else {
        jp1378 = false
    }
    var t1379 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1378)
    println__T_string(t1379)
    var t1380 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1381 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1382 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(t1380, t1381)
    var t1383 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1382)
    var inline2866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1383)
    _goml_runtime_core_string_println(inline2866)
    var t1384 [2]int = [2]int{1, 2}
    var t1385 [2]int = [2]int{1, 3}
    var t1386 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1384, t1385)
    var t1387 string = ordering_name(t1386)
    var inline2863 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1387)
    _goml_runtime_core_string_println(inline2863)
    var t1388 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_3int_7float64 = Tuple2_3int_7float64{
        _0: 0,
        _1: t1388,
    }
    var t1389 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1390 bool
    var inline2860 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t1389)
    var inline2861 bool = !inline2860
    t1390 = inline2861
    var t1391 string
    var inline2858 string = _goml_runtime_core_bool_to_string(t1390)
    t1391 = inline2858
    var inline2855 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1391)
    _goml_runtime_core_string_println(inline2855)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2853 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2853
    var t1392 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2850 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1392, inline2850)
    var t1393 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1394 Option__string
    var inline2848 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1393)
    t1394 = inline2848
    var t1395 string
    var inline2844 string = "missing"
    switch t1394.(type) {
    case Option__string_None:
        t1395 = inline2844
    case Option__string_Some:
        var inline2845 string = t1394.(Option__string_Some)._0
        t1395 = inline2845
    default:
        panic("non-exhaustive match")
    }
    var inline2841 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1395)
    _goml_runtime_core_string_println(inline2841)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2839 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2839
    var t1396 [2]int = [2]int{1, 2}
    var inline2836 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1396, inline2836)
    var t1397 [2]int = [2]int{1, 2}
    var t1398 Option__string
    var inline2834 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1397)
    t1398 = inline2834
    var t1399 string
    var inline2830 string = "missing"
    switch t1398.(type) {
    case Option__string_None:
        t1399 = inline2830
    case Option__string_Some:
        var inline2831 string = t1398.(Option__string_Some)._0
        t1399 = inline2831
    default:
        panic("non-exhaustive match")
    }
    var inline2827 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1399)
    _goml_runtime_core_string_println(inline2827)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(self__845 Ordering, other__846 Ordering) bool {
    switch self__845 {
    case Less:
        switch other__846 {
        case Less:
            return true
        default:
            return false
        }
    case Equal:
        switch other__846 {
        case Equal:
            return true
        default:
            return false
        }
    case Greater:
        switch other__846 {
        case Greater:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(self__0 int, other__1 int) bool {
    var commute_field3647 Ordering
    var inline2990 bool = self__0 < other__1
    var inline2992 Ordering
    if inline2990 {
        inline2992 = Less
    } else {
        var inline2994 bool = self__0 > other__1
        if inline2994 {
            inline2992 = Greater
        } else {
            inline2992 = Equal
        }
    }
    commute_field3647 = inline2992
    switch commute_field3647 {
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

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__185 int, other__186 int) bool {
    var t1804 bool = self__185 == other__186
    return t1804
}

func println__T_string(value__1 string) struct{} {
    var t1900 string
    t1900 = value__1
    _goml_runtime_core_string_println(t1900)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1904 string = _goml_runtime_core_bool_to_string(self__148)
    return t1904
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(self__457 Option__Ordering) bool {
    var t1907 bool
    switch self__457.(type) {
    case Option__Ordering_None:
        t1907 = false
    case Option__Ordering_Some:
        t1907 = true
    default:
        panic("non-exhaustive match")
    }
    var t1908 bool = !t1907
    return t1908
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3385 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3385.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3386 Ordering = inline3385.(Option__Ordering_Some)._0
        var inline3388 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3386, Less)
        return inline3388
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3390 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3390.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3391 Ordering = inline3390.(Option__Ordering_Some)._0
        var inline3393 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3391, Less)
        return inline3393
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3395 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3395.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3396 Ordering = inline3395.(Option__Ordering_Some)._0
        var inline3398 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3396, Less)
        return inline3398
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3400 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3400.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3401 Ordering = inline3400.(Option__Ordering_Some)._0
        var inline3403 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3401, Less)
        return inline3403
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3405 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
    switch inline3405.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3406 Ordering = inline3405.(Option__Ordering_Some)._0
        var inline3408 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3406, Less)
        return inline3408
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__311 *_goml_vec_int, start__312 int, end__313 int) []int {
    var t1926 []int = self__311.items[start__312:end__313]
    return t1926
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3410 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3410.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3411 Ordering = inline3410.(Option__Ordering_Some)._0
        var inline3413 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3411, Less)
        return inline3413
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1932 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1932
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__422 *hashmap_Vec_3int_string_x, key__423 *_goml_vec_int, value__424 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__420 *hashmap_Vec_3int_string_x, key__421 *_goml_vec_int) Option__string {
    var t1937 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__420, key__421)
    return t1937
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__458 Option__string, fallback__459 string) string {
    switch self__458.(type) {
    case Option__string_None:
        return fallback__459
    case Option__string_Some:
        var x387 string = self__458.(Option__string_Some)._0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default() Tuple2_3int_6string {
    var t1944 int
    t1944 = 0
    var t1945 string
    t1945 = ""
    var t1946 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1944,
        _1: t1945,
    }
    return t1946
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1952 int
    t1952 = 0
    var t1953 int
    t1953 = 0
    var t1954 int
    t1954 = 0
    var t1955 [3]int = [3]int{t1952, t1953, t1954}
    return t1955
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1960 int = self._0
    var t1961 int = other._0
    var t1962 bool
    var inline3426 bool = t1960 == t1961
    t1962 = inline3426
    if t1962 {
        var t1965 int = self._1
        var t1966 int = other._1
        var t1967 bool
        var inline3422 bool = t1965 == t1966
        t1967 = inline3422
        if t1967 {
            return false
        } else {
            var t1968 int = self._1
            var t1969 int = other._1
            var inline3420 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1968, t1969)
            return inline3420
        }
    } else {
        var t1971 int = self._0
        var t1972 int = other._0
        var inline3424 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1971, t1972)
        return inline3424
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) Ordering {
    var t1976 int = array_get__Array_2_3int(self, 0)
    var t1977 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 Ordering
    var inline3433 bool = t1976 < t1977
    if inline3433 {
        _structural_ordering_0 = Less
    } else {
        var inline3434 bool = t1976 > t1977
        if inline3434 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1980 bool
    switch _structural_ordering_0 {
    case Less:
        t1980 = false
    case Equal:
        t1980 = true
    case Greater:
        t1980 = false
    default:
        panic("non-exhaustive match")
    }
    if t1980 {
        var t1981 int = array_get__Array_2_3int(self, 1)
        var t1982 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 Ordering
        var inline3429 bool = t1981 < t1982
        if inline3429 {
            _structural_ordering_1 = Less
        } else {
            var inline3430 bool = t1981 > t1982
            if inline3430 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1985 bool
        switch _structural_ordering_1 {
        case Less:
            t1985 = false
        case Equal:
            t1985 = true
        case Greater:
            t1985 = false
        default:
            panic("non-exhaustive match")
        }
        if t1985 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(self Tuple2_3int_7float64, other Tuple2_3int_7float64) Option__Ordering {
    var t1988 int = self._0
    var t1989 int = other._0
    var _structural_partial_ordering_0 Option__Ordering
    var commute_field3806 Ordering
    var inline3445 bool = t1988 < t1989
    var inline3447 Ordering
    if inline3445 {
        inline3447 = Less
    } else {
        var inline3449 bool = t1988 > t1989
        if inline3449 {
            inline3447 = Greater
        } else {
            inline3447 = Equal
        }
    }
    var inline3448 Option__Ordering = Option__Ordering_Some{
        _0: inline3447,
    }
    _structural_partial_ordering_0 = inline3448
    commute_field3806 = inline3447
    var t1994 bool
    switch commute_field3806 {
    case Less:
        t1994 = false
    case Equal:
        t1994 = true
    case Greater:
        t1994 = false
    default:
        panic("non-exhaustive match")
    }
    if t1994 {
        var t1995 float64 = self._1
        var t1996 float64 = other._1
        var _structural_partial_ordering_1 Option__Ordering
        var commute_field3803 Ordering
        var inline3437 bool = t1995 < t1996
        if inline3437 {
            var inline3438 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            _structural_partial_ordering_1 = inline3438
            commute_field3803 = Less
            var t2001 bool
            switch commute_field3803 {
            case Less:
                t2001 = false
            case Equal:
                t2001 = true
            case Greater:
                t2001 = false
            default:
                panic("non-exhaustive match")
            }
            if t2001 {
                var t2002 Option__Ordering = Option__Ordering_Some{
                    _0: Equal,
                }
                return t2002
            } else {
                return _structural_partial_ordering_1
            }
        } else {
            var inline3439 bool = t1995 > t1996
            if inline3439 {
                var inline3440 Option__Ordering = Option__Ordering_Some{
                    _0: Greater,
                }
                _structural_partial_ordering_1 = inline3440
                commute_field3803 = Greater
                var t2001 bool
                switch commute_field3803 {
                case Less:
                    t2001 = false
                case Equal:
                    t2001 = true
                case Greater:
                    t2001 = false
                default:
                    panic("non-exhaustive match")
                }
                if t2001 {
                    var t2002 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2002
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3441 bool = t1995 == t1996
                if inline3441 {
                    var inline3442 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    _structural_partial_ordering_1 = inline3442
                    commute_field3803 = Equal
                    var t2001 bool
                    switch commute_field3803 {
                    case Less:
                        t2001 = false
                    case Equal:
                        t2001 = true
                    case Greater:
                        t2001 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t2001 {
                        var t2002 Option__Ordering = Option__Ordering_Some{
                            _0: Equal,
                        }
                        return t2002
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    return Option__Ordering_None{}
                }
            }
        }
    } else {
        return _structural_partial_ordering_0
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__456 Option__Ordering) bool {
    switch self__456.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(self__67 GenericPair__int, other__68 GenericPair__int) Option__Ordering {
    var t2064 int = self__67.first
    var t2065 int = other__68.first
    var commute_field3812 Ordering
    var inline3463 bool = t2064 < t2065
    var inline3465 Ordering
    if inline3463 {
        inline3465 = Less
    } else {
        var inline3467 bool = t2064 > t2065
        if inline3467 {
            inline3465 = Greater
        } else {
            inline3465 = Equal
        }
    }
    commute_field3812 = inline3465
    switch commute_field3812 {
    case Equal:
        var t2070 int = self__67.second
        var t2071 int = other__68.second
        var commute_field3809 Ordering
        var inline3457 bool = t2070 < t2071
        var inline3459 Ordering
        if inline3457 {
            inline3459 = Less
        } else {
            var inline3461 bool = t2070 > t2071
            if inline3461 {
                inline3459 = Greater
            } else {
                inline3459 = Equal
            }
        }
        commute_field3809 = inline3459
        switch commute_field3809 {
        case Equal:
            var t2076 *_goml_vec_int = self__67.nested
            var t2077 *_goml_vec_int = other__68.nested
            var mtmp506 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t2076, t2077)
            switch mtmp506.(type) {
            case Option__Ordering_None:
                return Option__Ordering_None{}
            case Option__Ordering_Some:
                var x507 Ordering = mtmp506.(Option__Ordering_Some)._0
                switch x507 {
                case Equal:
                    var t2082 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2082
                default:
                    var t2083 Option__Ordering = Option__Ordering_Some{
                        _0: x507,
                    }
                    return t2083
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t2084 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3809,
            }
            return t2084
        }
    default:
        var t2085 Option__Ordering = Option__Ordering_Some{
            _0: commute_field3812,
        }
        return t2085
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) Option__Ordering {
    var jp2089 int
    switch self__83 {
    case First:
        jp2089 = 0
    case Second:
        jp2089 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp2091 int
    switch other__84 {
    case First:
        jp2091 = 0
    case Second:
        jp2091 = 1
    default:
        panic("non-exhaustive match")
    }
    var t2094 bool = jp2089 < jp2091
    if t2094 {
        var t2095 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t2095
    } else {
        var t2098 bool = jp2089 > jp2091
        if t2098 {
            var t2099 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2099
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t2104 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2104
                default:
                    var t2105 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2105
                }
            case Second:
                switch self__83 {
                case Second:
                    var t2108 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2108
                default:
                    var t2109 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2109
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__76 *_goml_vec_int, other__77 *_goml_vec_int) Option__Ordering {
    var t2127 int
    var inline3491 int = vec_len__Vec_3int(self__76)
    t2127 = inline3491
    var t2128 int
    var inline3489 int = vec_len__Vec_3int(other__77)
    t2128 = inline3489
    var t2129 bool = t2127 < t2128
    var jp2113 int
    if t2129 {
        var inline3469 int = vec_len__Vec_3int(self__76)
        jp2113 = inline3469
    } else {
        var inline3471 int = vec_len__Vec_3int(other__77)
        jp2113 = inline3471
    }
    var index__79 int = 0
    Loop_loop2118:
    for {
        var t2119 bool = index__79 < jp2113
        if t2119 {
            var t2120 int = vec_get__Vec_3int(self__76, index__79)
            var t2121 int = vec_get__Vec_3int(other__77, index__79)
            var commute_field3815 Ordering
            var inline3473 bool = t2120 < t2121
            var inline3475 Ordering
            if inline3473 {
                inline3475 = Less
            } else {
                var inline3477 bool = t2120 > t2121
                if inline3477 {
                    inline3475 = Greater
                } else {
                    inline3475 = Equal
                }
            }
            commute_field3815 = inline3475
            switch commute_field3815 {
            case Equal:
                var compound_old10 int = index__79
                var compound_value11 int = 1
                var t2124 int = compound_old10 + compound_value11
                index__79 = t2124
                continue
            default:
                var t2126 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3815,
                }
                return t2126
            }
        } else {
            break Loop_loop2118
        }
    }
    var t2115 int
    var inline3487 int = vec_len__Vec_3int(self__76)
    t2115 = inline3487
    var t2116 int
    var inline3485 int = vec_len__Vec_3int(other__77)
    t2116 = inline3485
    var inline3479 bool = t2115 < t2116
    var inline3481 Ordering
    if inline3479 {
        inline3481 = Less
    } else {
        var inline3483 bool = t2115 > t2116
        if inline3483 {
            inline3481 = Greater
        } else {
            inline3481 = Equal
        }
    }
    var inline3482 Option__Ordering = Option__Ordering_Some{
        _0: inline3481,
    }
    return inline3482
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__96 Option__int, other__97 Option__int) Option__Ordering {
    switch other__97.(type) {
    case Option__int_None:
        switch self__96.(type) {
        case Option__int_None:
            var t2138 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t2138
        case Option__int_Some:
            var t2139 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2139
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__97.(Option__int_Some)._0
        switch self__96.(type) {
        case Option__int_None:
            var t2142 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            return t2142
        case Option__int_Some:
            var x35 int = self__96.(Option__int_Some)._0
            var inline3493 bool = x35 < x33
            var inline3495 Ordering
            if inline3493 {
                inline3495 = Less
            } else {
                var inline3497 bool = x35 > x33
                if inline3497 {
                    inline3495 = Greater
                } else {
                    inline3495 = Equal
                }
            }
            var inline3496 Option__Ordering = Option__Ordering_Some{
                _0: inline3495,
            }
            return inline3496
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(self__104 Result__int__string, other__105 Result__int__string) Option__Ordering {
    switch other__105.(type) {
    case Ok:
        var x45 int = other__105.(Ok)._0
        switch self__104.(type) {
        case Ok:
            var x47 int = self__104.(Ok)._0
            var inline3499 bool = x47 < x45
            var inline3501 Ordering
            if inline3499 {
                inline3501 = Less
            } else {
                var inline3503 bool = x47 > x45
                if inline3503 {
                    inline3501 = Greater
                } else {
                    inline3501 = Equal
                }
            }
            var inline3502 Option__Ordering = Option__Ordering_Some{
                _0: inline3501,
            }
            return inline3502
        case Err:
            var t2151 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2151
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__105.(Err)._0
        switch self__104.(type) {
        case Ok:
            var t2154 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            return t2154
        case Err:
            var x50 string = self__104.(Err)._0
            var inline3505 bool = x50 < x46
            var inline3507 Ordering
            if inline3505 {
                inline3507 = Less
            } else {
                var inline3509 bool = x50 > x46
                if inline3509 {
                    inline3507 = Greater
                } else {
                    inline3507 = Equal
                }
            }
            var inline3508 Option__Ordering = Option__Ordering_Some{
                _0: inline3507,
            }
            return inline3508
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__86 []int, other__87 []int) Option__Ordering {
    var t2173 int
    var inline3533 int = len(self__86)
    t2173 = inline3533
    var t2174 int
    var inline3531 int = len(other__87)
    t2174 = inline3531
    var t2175 bool = t2173 < t2174
    var jp2159 int
    if t2175 {
        var inline3511 int = len(self__86)
        jp2159 = inline3511
    } else {
        var inline3513 int = len(other__87)
        jp2159 = inline3513
    }
    var index__89 int = 0
    Loop_loop2164:
    for {
        var t2165 bool = index__89 < jp2159
        if t2165 {
            var t2166 int = self__86[index__89]
            var t2167 int = other__87[index__89]
            var commute_field3818 Ordering
            var inline3515 bool = t2166 < t2167
            var inline3517 Ordering
            if inline3515 {
                inline3517 = Less
            } else {
                var inline3519 bool = t2166 > t2167
                if inline3519 {
                    inline3517 = Greater
                } else {
                    inline3517 = Equal
                }
            }
            commute_field3818 = inline3517
            switch commute_field3818 {
            case Equal:
                var compound_old21 int = index__89
                var compound_value22 int = 1
                var t2170 int = compound_old21 + compound_value22
                index__89 = t2170
                continue
            default:
                var t2172 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3818,
                }
                return t2172
            }
        } else {
            break Loop_loop2164
        }
    }
    var t2161 int
    var inline3529 int = len(self__86)
    t2161 = inline3529
    var t2162 int
    var inline3527 int = len(other__87)
    t2162 = inline3527
    var inline3521 bool = t2161 < t2162
    var inline3523 Ordering
    if inline3521 {
        inline3523 = Less
    } else {
        var inline3525 bool = t2161 > t2162
        if inline3525 {
            inline3523 = Greater
        } else {
            inline3523 = Equal
        }
    }
    var inline3524 Option__Ordering = Option__Ordering_Some{
        _0: inline3523,
    }
    return inline3524
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__223 *_goml_vec_int, other__224 *_goml_vec_int) bool {
    var t2188 int
    var inline3541 int = vec_len__Vec_3int(self__223)
    t2188 = inline3541
    var t2189 int
    var inline3539 int = vec_len__Vec_3int(other__224)
    t2189 = inline3539
    var t2190 bool = t2188 != t2189
    if t2190 {
        return false
    } else {
        var index__225 int = 0
        Loop_loop2192:
        for {
            var t2193 int
            var inline3537 int = vec_len__Vec_3int(self__223)
            t2193 = inline3537
            var t2194 bool = index__225 < t2193
            if t2194 {
                var t2196 int = vec_get__Vec_3int(self__223, index__225)
                var t2197 int = vec_get__Vec_3int(other__224, index__225)
                var t2198 bool
                var inline3535 bool = t2196 == t2197
                t2198 = inline3535
                if t2198 {
                    var compound_old153 int = index__225
                    var compound_value154 int = 1
                    var t2199 int = compound_old153 + compound_value154
                    index__225 = t2199
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2192
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__226 *_goml_vec_int) uint64 {
    var value__227 uint64 = 14695981039346656037
    var index__228 int = 0
    Loop_loop2204:
    for {
        var t2205 int
        var inline3545 int = vec_len__Vec_3int(self__226)
        t2205 = inline3545
        var t2206 bool = index__228 < t2205
        if t2206 {
            var t2207 uint64 = value__227 * 1099511628211
            var t2208 int = vec_get__Vec_3int(self__226, index__228)
            var t2209 uint64
            var inline3543 uint64 = _goml_runtime_core_int_hash(t2208)
            t2209 = inline3543
            var t2210 uint64 = t2207 + t2209
            value__227 = t2210
            var compound_old158 int = index__228
            var compound_value159 int = 1
            var t2211 int = compound_old158 + compound_value159
            index__228 = t2211
            continue
        } else {
            break Loop_loop2204
        }
    }
    return value__227
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2217 int = self._0
    var t2218 int = other._0
    var t2219 bool
    var inline3549 bool = t2217 == t2218
    t2219 = inline3549
    if t2219 {
        var t2222 string = self._1
        var t2223 string = other._1
        var t2224 bool
        var inline3547 bool = t2222 == t2223
        t2224 = inline3547
        if t2224 {
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
    var t2227 uint64 = _structural_hash_0 * 1099511628211
    var t2228 int = self._0
    var t2229 uint64
    var inline3553 uint64 = _goml_runtime_core_int_hash(t2228)
    t2229 = inline3553
    var _structural_hash_1 uint64 = t2227 + t2229
    var t2230 uint64 = _structural_hash_1 * 1099511628211
    var t2231 string = self._1
    var t2232 uint64
    var inline3551 uint64 = _goml_runtime_core_string_hash(t2231)
    t2232 = inline3551
    var _structural_hash_2 uint64 = t2230 + t2232
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2237 int = array_get__Array_2_3int(self, 0)
    var t2238 int = array_get__Array_2_3int(other, 0)
    var t2239 bool
    var inline3557 bool = t2237 == t2238
    t2239 = inline3557
    if t2239 {
        var t2242 int = array_get__Array_2_3int(self, 1)
        var t2243 int = array_get__Array_2_3int(other, 1)
        var t2244 bool
        var inline3555 bool = t2242 == t2243
        t2244 = inline3555
        if t2244 {
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
    var t2247 uint64 = _structural_hash_0 * 1099511628211
    var t2248 int = array_get__Array_2_3int(self, 0)
    var t2249 uint64
    var inline3561 uint64 = _goml_runtime_core_int_hash(t2248)
    t2249 = inline3561
    var _structural_hash_1 uint64 = t2247 + t2249
    var t2250 uint64 = _structural_hash_1 * 1099511628211
    var t2251 int = array_get__Array_2_3int(self, 1)
    var t2252 uint64
    var inline3559 uint64 = _goml_runtime_core_int_hash(t2251)
    t2252 = inline3559
    var _structural_hash_2 uint64 = t2250 + t2252
    return _structural_hash_2
}

func main() {
    main0()
}
