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
    var t712 bool = self__32 < other__33
    if t712 {
        return Less
    } else {
        var t715 bool = self__32 > other__33
        if t715 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) Option__Ordering {
    var t1064 int = self__2.major
    var t1065 int = other__3.major
    var commute_field3571 Ordering
    var inline2653 bool = t1064 < t1065
    var inline2655 Ordering
    if inline2653 {
        inline2655 = Less
    } else {
        var inline2657 bool = t1064 > t1065
        if inline2657 {
            inline2655 = Greater
        } else {
            inline2655 = Equal
        }
    }
    commute_field3571 = inline2655
    switch commute_field3571 {
    case Equal:
        var t1070 int = self__2.minor
        var t1071 int = other__3.minor
        var commute_field3568 Ordering
        var inline2647 bool = t1070 < t1071
        var inline2649 Ordering
        if inline2647 {
            inline2649 = Less
        } else {
            var inline2651 bool = t1070 > t1071
            if inline2651 {
                inline2649 = Greater
            } else {
                inline2649 = Equal
            }
        }
        commute_field3568 = inline2649
        switch commute_field3568 {
        case Equal:
            var t1076 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t1076
        default:
            var t1077 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3568,
            }
            return t1077
        }
    default:
        var t1078 Option__Ordering = Option__Ordering_Some{
            _0: commute_field3571,
        }
        return t1078
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2659 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2659.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline2660 Ordering = inline2659.(Option__Ordering_Some)._0
        var inline2662 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2660, Less)
        return inline2662
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) Ordering {
    var t1093 int = self__6.major
    var t1094 int = other__7.major
    var _goml_m__i_derive1__ordering____8 Ordering
    var inline2686 bool = t1093 < t1094
    if inline2686 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2687 bool = t1093 > t1094
        if inline2687 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t1097 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t1097 = false
    case Equal:
        t1097 = true
    case Greater:
        t1097 = false
    default:
        panic("non-exhaustive match")
    }
    if t1097 {
        var t1098 int = self__6.minor
        var t1099 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 Ordering
        var inline2682 bool = t1098 < t1099
        if inline2682 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2683 bool = t1098 > t1099
            if inline2683 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t1102 bool
        switch _goml_m__i_derive0__ordering____9 {
        case Less:
            t1102 = false
        case Equal:
            t1102 = true
        case Greater:
            t1102 = false
        default:
            panic("non-exhaustive match")
        }
        if t1102 {
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) Option__Ordering {
    var jp1138 int
    switch self__23.(type) {
    case Low:
        jp1138 = 0
    case Medium:
        jp1138 = 1
    case High:
        jp1138 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1140 int
    switch other__24.(type) {
    case Low:
        jp1140 = 0
    case Medium:
        jp1140 = 1
    case High:
        jp1140 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1143 bool = jp1138 < jp1140
    if t1143 {
        var t1144 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t1144
    } else {
        var t1147 bool = jp1138 > jp1140
        if t1147 {
            var t1148 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t1148
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t1153 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1153
                default:
                    var t1154 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1154
                }
            case Medium:
                var x445 int = other__24.(Medium)._0
                var x446 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x453 int = self__23.(Medium)._0
                    var x454 int = self__23.(Medium)._1
                    var commute_field3577 Ordering
                    var inline2707 bool = x453 < x445
                    var inline2709 Ordering
                    if inline2707 {
                        inline2709 = Less
                    } else {
                        var inline2711 bool = x453 > x445
                        if inline2711 {
                            inline2709 = Greater
                        } else {
                            inline2709 = Equal
                        }
                    }
                    commute_field3577 = inline2709
                    switch commute_field3577 {
                    case Equal:
                        var commute_field3574 Ordering
                        var inline2701 bool = x454 < x446
                        var inline2703 Ordering
                        if inline2701 {
                            inline2703 = Less
                        } else {
                            var inline2705 bool = x454 > x446
                            if inline2705 {
                                inline2703 = Greater
                            } else {
                                inline2703 = Equal
                            }
                        }
                        commute_field3574 = inline2703
                        switch commute_field3574 {
                        case Equal:
                            var t1165 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1165
                        default:
                            var t1166 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3574,
                            }
                            return t1166
                        }
                    default:
                        var t1167 Option__Ordering = Option__Ordering_Some{
                            _0: commute_field3577,
                        }
                        return t1167
                    }
                default:
                    var t1168 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1168
                }
            case High:
                var x447 int = other__24.(High)._0
                var x448 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x463 int = self__23.(High)._0
                    var x464 int = self__23.(High)._1
                    var commute_field3583 Ordering
                    var inline2719 bool = x463 < x447
                    var inline2721 Ordering
                    if inline2719 {
                        inline2721 = Less
                    } else {
                        var inline2723 bool = x463 > x447
                        if inline2723 {
                            inline2721 = Greater
                        } else {
                            inline2721 = Equal
                        }
                    }
                    commute_field3583 = inline2721
                    switch commute_field3583 {
                    case Equal:
                        var commute_field3580 Ordering
                        var inline2713 bool = x464 < x448
                        var inline2715 Ordering
                        if inline2713 {
                            inline2715 = Less
                        } else {
                            var inline2717 bool = x464 > x448
                            if inline2717 {
                                inline2715 = Greater
                            } else {
                                inline2715 = Equal
                            }
                        }
                        commute_field3580 = inline2715
                        switch commute_field3580 {
                        case Equal:
                            var t1179 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1179
                        default:
                            var t1180 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3580,
                            }
                            return t1180
                        }
                    default:
                        var t1181 Option__Ordering = Option__Ordering_Some{
                            _0: commute_field3583,
                        }
                        return t1181
                    }
                default:
                    var t1182 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1182
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline2725 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2725.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline2726 Ordering = inline2725.(Option__Ordering_Some)._0
        var inline2728 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2726, Less)
        return inline2728
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) Ordering {
    var jp1198 int
    switch self__39.(type) {
    case Low:
        jp1198 = 0
    case Medium:
        jp1198 = 1
    case High:
        jp1198 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1200 int
    switch other__40.(type) {
    case Low:
        jp1200 = 0
    case Medium:
        jp1200 = 1
    case High:
        jp1200 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1203 bool = jp1198 < jp1200
    if t1203 {
        return Less
    } else {
        var t1206 bool = jp1198 > jp1200
        if t1206 {
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
                    var inline2752 bool = x488 < x480
                    if inline2752 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2753 bool = x488 > x480
                        if inline2753 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1215 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1215 = false
                    case Equal:
                        t1215 = true
                    case Greater:
                        t1215 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1215 {
                        var _goml_m__i_derive4__ordering____48 Ordering
                        var inline2748 bool = x489 < x481
                        if inline2748 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2749 bool = x489 > x481
                            if inline2749 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1218 bool
                        switch _goml_m__i_derive4__ordering____48 {
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
                    var t1223 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1223 {
                        var _goml_m__i_derive10__ordering____54 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x495, x483)
                        var t1226 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1226 = false
                        case Equal:
                            t1226 = true
                        case Greater:
                            t1226 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1226 {
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
    var t1249 float64 = self__60.value
    var t1250 float64 = other__61.value
    var inline2764 bool = t1249 == t1250
    return inline2764
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) Option__Ordering {
    var t1254 float64 = self__62.value
    var t1255 float64 = other__63.value
    var commute_field3586 Ordering
    var inline2766 bool = t1254 < t1255
    if inline2766 {
        commute_field3586 = Less
        switch commute_field3586 {
        case Equal:
            var t1260 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t1260
        default:
            var t1261 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3586,
            }
            return t1261
        }
    } else {
        var inline2768 bool = t1254 > t1255
        if inline2768 {
            commute_field3586 = Greater
            switch commute_field3586 {
            case Equal:
                var t1260 Option__Ordering = Option__Ordering_Some{
                    _0: Equal,
                }
                return t1260
            default:
                var t1261 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3586,
                }
                return t1261
            }
        } else {
            var inline2770 bool = t1254 == t1255
            if inline2770 {
                commute_field3586 = Equal
                switch commute_field3586 {
                case Equal:
                    var t1260 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1260
                default:
                    var t1261 Option__Ordering = Option__Ordering_Some{
                        _0: commute_field3586,
                    }
                    return t1261
                }
            } else {
                return Option__Ordering_None{}
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) Option__Ordering {
    var jp1288 int
    switch self__95.(type) {
    case Value:
        jp1288 = 0
    case Empty:
        jp1288 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1290 int
    switch other__96.(type) {
    case Value:
        jp1290 = 0
    case Empty:
        jp1290 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1293 bool = jp1288 < jp1290
    if t1293 {
        var t1294 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t1294
    } else {
        var t1297 bool = jp1288 > jp1290
        if t1297 {
            var t1298 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t1298
        } else {
            switch other__96.(type) {
            case Value:
                var x532 float64 = other__96.(Value)._0
                switch self__95.(type) {
                case Value:
                    var x533 float64 = self__95.(Value)._0
                    var commute_field3589 Ordering
                    var inline2797 bool = x533 < x532
                    if inline2797 {
                        commute_field3589 = Less
                        switch commute_field3589 {
                        case Equal:
                            var t1307 Option__Ordering = Option__Ordering_Some{
                                _0: Equal,
                            }
                            return t1307
                        default:
                            var t1308 Option__Ordering = Option__Ordering_Some{
                                _0: commute_field3589,
                            }
                            return t1308
                        }
                    } else {
                        var inline2799 bool = x533 > x532
                        if inline2799 {
                            commute_field3589 = Greater
                            switch commute_field3589 {
                            case Equal:
                                var t1307 Option__Ordering = Option__Ordering_Some{
                                    _0: Equal,
                                }
                                return t1307
                            default:
                                var t1308 Option__Ordering = Option__Ordering_Some{
                                    _0: commute_field3589,
                                }
                                return t1308
                            }
                        } else {
                            var inline2801 bool = x533 == x532
                            if inline2801 {
                                commute_field3589 = Equal
                                switch commute_field3589 {
                                case Equal:
                                    var t1307 Option__Ordering = Option__Ordering_Some{
                                        _0: Equal,
                                    }
                                    return t1307
                                default:
                                    var t1308 Option__Ordering = Option__Ordering_Some{
                                        _0: commute_field3589,
                                    }
                                    return t1308
                                }
                            } else {
                                return Option__Ordering_None{}
                            }
                        }
                    }
                default:
                    var t1309 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1309
                }
            case Empty:
                switch self__95.(type) {
                case Empty:
                    var t1312 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1312
                default:
                    var t1313 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t1313
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
    var t1331 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1332 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1331)
    println__T_string(t1332)
    var t1333 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1334 string = ordering_name(t1333)
    println__T_string(t1334)
    var t1335 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1336 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1335)
    var t1337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1336)
    println__T_string(t1337)
    var t1338 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1339 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1340 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1338, t1339)
    var t1341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1340)
    println__T_string(t1341)
    var t1342 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1343 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1344 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1342, t1343)
    var t1345 string = ordering_name(t1344)
    println__T_string(t1345)
    var zero__105 float64 = 0
    var t1346 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1346,
    }
    var t1347 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1348 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1347)
    println__T_string(t1348)
    var t1349 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1350 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1349)
    var t1351 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1350)
    println__T_string(t1351)
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
    var t1352 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1353 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1352)
    println__T_string(t1353)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1354 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1355 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1354)
    println__T_string(t1355)
    var t1356 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = Value{
        _0: t1356,
    }
    var t1357 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1358 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1357)
    var t1359 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1358)
    println__T_string(t1359)
    var vec_literal__2131 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2131, 2)
    var vec_literal__2178 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2178, 3)
    var t1360 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(vec_literal__2131, vec_literal__2178)
    var t1361 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1360)
    println__T_string(t1361)
    var t1362 Option__int = Option__int_Some{
        _0: 2,
    }
    var t1363 Option__int = Option__int_Some{
        _0: 3,
    }
    var t1364 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1362, t1363)
    var t1365 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1364)
    println__T_string(t1365)
    var ok__114 Result__int__string = Ok{
        _0: 1,
    }
    var error__115 Result__int__string = Err{
        _0: "error",
    }
    var t1366 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1367 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1366)
    println__T_string(t1367)
    var t1368 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2131, 0, 2)
    var t1369 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(vec_literal__2178, 0, 2)
    var t1370 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1368, t1369)
    var t1371 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1370)
    println__T_string(t1371)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, vec_literal__2131, "vector")
    var vec_literal__2661 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__2661, 2)
    var t1372 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, vec_literal__2661)
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
        var inline2826 bool = t1415 == t1416
        jp1375 = inline2826
    } else {
        jp1375 = false
    }
    var t1376 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1375)
    println__T_string(t1376)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs565 [3]int = [3]int{0, 0, 0}
    var t1401 int = array_get__Array_3_3int(default_array__118, 0)
    var t1402 int = array_get__Array_3_3int(_eq_rhs565, 0)
    var t1403 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1401, t1402)
    var jp1378 bool
    if t1403 {
        var t1406 int = array_get__Array_3_3int(default_array__118, 1)
        var t1407 int = array_get__Array_3_3int(_eq_rhs565, 1)
        var t1408 bool
        var inline2830 bool = t1406 == t1407
        t1408 = inline2830
        if t1408 {
            var t1409 int = array_get__Array_3_3int(default_array__118, 2)
            var t1410 int = array_get__Array_3_3int(_eq_rhs565, 2)
            var inline2828 bool = t1409 == t1410
            jp1378 = inline2828
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
    var inline2871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1383)
    _goml_runtime_core_string_println(inline2871)
    var t1384 [2]int = [2]int{1, 2}
    var t1385 [2]int = [2]int{1, 3}
    var t1386 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1384, t1385)
    var t1387 string = ordering_name(t1386)
    var inline2868 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1387)
    _goml_runtime_core_string_println(inline2868)
    var t1388 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_3int_7float64 = Tuple2_3int_7float64{
        _0: 0,
        _1: t1388,
    }
    var t1389 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1390 bool
    var inline2865 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t1389)
    var inline2866 bool = !inline2865
    t1390 = inline2866
    var t1391 string
    var inline2863 string = _goml_runtime_core_bool_to_string(t1390)
    t1391 = inline2863
    var inline2860 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1391)
    _goml_runtime_core_string_println(inline2860)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2858 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2858
    var t1392 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2855 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1392, inline2855)
    var t1393 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1394 Option__string
    var inline2853 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1393)
    t1394 = inline2853
    var t1395 string
    var inline2849 string = "missing"
    switch t1394.(type) {
    case Option__string_None:
        t1395 = inline2849
    case Option__string_Some:
        var inline2850 string = t1394.(Option__string_Some)._0
        t1395 = inline2850
    default:
        panic("non-exhaustive match")
    }
    var inline2846 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1395)
    _goml_runtime_core_string_println(inline2846)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2844 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2844
    var t1396 [2]int = [2]int{1, 2}
    var inline2841 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1396, inline2841)
    var t1397 [2]int = [2]int{1, 2}
    var t1398 Option__string
    var inline2839 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1397)
    t1398 = inline2839
    var t1399 string
    var inline2835 string = "missing"
    switch t1398.(type) {
    case Option__string_None:
        t1399 = inline2835
    case Option__string_Some:
        var inline2836 string = t1398.(Option__string_Some)._0
        t1399 = inline2836
    default:
        panic("non-exhaustive match")
    }
    var inline2832 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1399)
    _goml_runtime_core_string_println(inline2832)
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
    var commute_field3652 Ordering
    var inline2995 bool = self__0 < other__1
    var inline2997 Ordering
    if inline2995 {
        inline2997 = Less
    } else {
        var inline2999 bool = self__0 > other__1
        if inline2999 {
            inline2997 = Greater
        } else {
            inline2997 = Equal
        }
    }
    commute_field3652 = inline2997
    switch commute_field3652 {
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1911 *_goml_vec_int = vec_new__Vec_3int()
    return t1911
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3390 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3395 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3400 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3405 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3410 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__311 *_goml_vec_int, start__312 int, end__313 int) []int {
    var t1931 []int = self__311.items[start__312:end__313]
    return t1931
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3415 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3415.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        var inline3416 Ordering = inline3415.(Option__Ordering_Some)._0
        var inline3418 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3416, Less)
        return inline3418
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1937 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1937
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__422 *hashmap_Vec_3int_string_x, key__423 *_goml_vec_int, value__424 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__420 *hashmap_Vec_3int_string_x, key__421 *_goml_vec_int) Option__string {
    var t1942 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__420, key__421)
    return t1942
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
    var t1949 int
    t1949 = 0
    var t1950 string
    t1950 = ""
    var t1951 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1949,
        _1: t1950,
    }
    return t1951
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1957 int
    t1957 = 0
    var t1958 int
    t1958 = 0
    var t1959 int
    t1959 = 0
    var t1960 [3]int = [3]int{t1957, t1958, t1959}
    return t1960
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1965 int = self._0
    var t1966 int = other._0
    var t1967 bool
    var inline3431 bool = t1965 == t1966
    t1967 = inline3431
    if t1967 {
        var t1970 int = self._1
        var t1971 int = other._1
        var t1972 bool
        var inline3427 bool = t1970 == t1971
        t1972 = inline3427
        if t1972 {
            return false
        } else {
            var t1973 int = self._1
            var t1974 int = other._1
            var inline3425 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1973, t1974)
            return inline3425
        }
    } else {
        var t1976 int = self._0
        var t1977 int = other._0
        var inline3429 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1976, t1977)
        return inline3429
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) Ordering {
    var t1981 int = array_get__Array_2_3int(self, 0)
    var t1982 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 Ordering
    var inline3438 bool = t1981 < t1982
    if inline3438 {
        _structural_ordering_0 = Less
    } else {
        var inline3439 bool = t1981 > t1982
        if inline3439 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1985 bool
    switch _structural_ordering_0 {
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
        var t1986 int = array_get__Array_2_3int(self, 1)
        var t1987 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 Ordering
        var inline3434 bool = t1986 < t1987
        if inline3434 {
            _structural_ordering_1 = Less
        } else {
            var inline3435 bool = t1986 > t1987
            if inline3435 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1990 bool
        switch _structural_ordering_1 {
        case Less:
            t1990 = false
        case Equal:
            t1990 = true
        case Greater:
            t1990 = false
        default:
            panic("non-exhaustive match")
        }
        if t1990 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(self Tuple2_3int_7float64, other Tuple2_3int_7float64) Option__Ordering {
    var t1993 int = self._0
    var t1994 int = other._0
    var _structural_partial_ordering_0 Option__Ordering
    var commute_field3811 Ordering
    var inline3450 bool = t1993 < t1994
    var inline3452 Ordering
    if inline3450 {
        inline3452 = Less
    } else {
        var inline3454 bool = t1993 > t1994
        if inline3454 {
            inline3452 = Greater
        } else {
            inline3452 = Equal
        }
    }
    var inline3453 Option__Ordering = Option__Ordering_Some{
        _0: inline3452,
    }
    _structural_partial_ordering_0 = inline3453
    commute_field3811 = inline3452
    var t1999 bool
    switch commute_field3811 {
    case Less:
        t1999 = false
    case Equal:
        t1999 = true
    case Greater:
        t1999 = false
    default:
        panic("non-exhaustive match")
    }
    if t1999 {
        var t2000 float64 = self._1
        var t2001 float64 = other._1
        var _structural_partial_ordering_1 Option__Ordering
        var commute_field3808 Ordering
        var inline3442 bool = t2000 < t2001
        if inline3442 {
            var inline3443 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            _structural_partial_ordering_1 = inline3443
            commute_field3808 = Less
            var t2006 bool
            switch commute_field3808 {
            case Less:
                t2006 = false
            case Equal:
                t2006 = true
            case Greater:
                t2006 = false
            default:
                panic("non-exhaustive match")
            }
            if t2006 {
                var t2007 Option__Ordering = Option__Ordering_Some{
                    _0: Equal,
                }
                return t2007
            } else {
                return _structural_partial_ordering_1
            }
        } else {
            var inline3444 bool = t2000 > t2001
            if inline3444 {
                var inline3445 Option__Ordering = Option__Ordering_Some{
                    _0: Greater,
                }
                _structural_partial_ordering_1 = inline3445
                commute_field3808 = Greater
                var t2006 bool
                switch commute_field3808 {
                case Less:
                    t2006 = false
                case Equal:
                    t2006 = true
                case Greater:
                    t2006 = false
                default:
                    panic("non-exhaustive match")
                }
                if t2006 {
                    var t2007 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2007
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3446 bool = t2000 == t2001
                if inline3446 {
                    var inline3447 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    _structural_partial_ordering_1 = inline3447
                    commute_field3808 = Equal
                    var t2006 bool
                    switch commute_field3808 {
                    case Less:
                        t2006 = false
                    case Equal:
                        t2006 = true
                    case Greater:
                        t2006 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t2006 {
                        var t2007 Option__Ordering = Option__Ordering_Some{
                            _0: Equal,
                        }
                        return t2007
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
    var t2069 int = self__67.first
    var t2070 int = other__68.first
    var commute_field3817 Ordering
    var inline3468 bool = t2069 < t2070
    var inline3470 Ordering
    if inline3468 {
        inline3470 = Less
    } else {
        var inline3472 bool = t2069 > t2070
        if inline3472 {
            inline3470 = Greater
        } else {
            inline3470 = Equal
        }
    }
    commute_field3817 = inline3470
    switch commute_field3817 {
    case Equal:
        var t2075 int = self__67.second
        var t2076 int = other__68.second
        var commute_field3814 Ordering
        var inline3462 bool = t2075 < t2076
        var inline3464 Ordering
        if inline3462 {
            inline3464 = Less
        } else {
            var inline3466 bool = t2075 > t2076
            if inline3466 {
                inline3464 = Greater
            } else {
                inline3464 = Equal
            }
        }
        commute_field3814 = inline3464
        switch commute_field3814 {
        case Equal:
            var t2081 *_goml_vec_int = self__67.nested
            var t2082 *_goml_vec_int = other__68.nested
            var mtmp506 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t2081, t2082)
            switch mtmp506.(type) {
            case Option__Ordering_None:
                return Option__Ordering_None{}
            case Option__Ordering_Some:
                var x507 Ordering = mtmp506.(Option__Ordering_Some)._0
                switch x507 {
                case Equal:
                    var t2087 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2087
                default:
                    var t2088 Option__Ordering = Option__Ordering_Some{
                        _0: x507,
                    }
                    return t2088
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t2089 Option__Ordering = Option__Ordering_Some{
                _0: commute_field3814,
            }
            return t2089
        }
    default:
        var t2090 Option__Ordering = Option__Ordering_Some{
            _0: commute_field3817,
        }
        return t2090
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) Option__Ordering {
    var jp2094 int
    switch self__83 {
    case First:
        jp2094 = 0
    case Second:
        jp2094 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp2096 int
    switch other__84 {
    case First:
        jp2096 = 0
    case Second:
        jp2096 = 1
    default:
        panic("non-exhaustive match")
    }
    var t2099 bool = jp2094 < jp2096
    if t2099 {
        var t2100 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        return t2100
    } else {
        var t2103 bool = jp2094 > jp2096
        if t2103 {
            var t2104 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2104
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t2109 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2109
                default:
                    var t2110 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2110
                }
            case Second:
                switch self__83 {
                case Second:
                    var t2113 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2113
                default:
                    var t2114 Option__Ordering = Option__Ordering_Some{
                        _0: Equal,
                    }
                    return t2114
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__76 *_goml_vec_int, other__77 *_goml_vec_int) Option__Ordering {
    var t2132 int
    var inline3496 int = vec_len__Vec_3int(self__76)
    t2132 = inline3496
    var t2133 int
    var inline3494 int = vec_len__Vec_3int(other__77)
    t2133 = inline3494
    var t2134 bool = t2132 < t2133
    var jp2118 int
    if t2134 {
        var inline3474 int = vec_len__Vec_3int(self__76)
        jp2118 = inline3474
    } else {
        var inline3476 int = vec_len__Vec_3int(other__77)
        jp2118 = inline3476
    }
    var index__79 int = 0
    Loop_loop2123:
    for {
        var t2124 bool = index__79 < jp2118
        if t2124 {
            var t2125 int = vec_get__Vec_3int(self__76, index__79)
            var t2126 int = vec_get__Vec_3int(other__77, index__79)
            var commute_field3820 Ordering
            var inline3478 bool = t2125 < t2126
            var inline3480 Ordering
            if inline3478 {
                inline3480 = Less
            } else {
                var inline3482 bool = t2125 > t2126
                if inline3482 {
                    inline3480 = Greater
                } else {
                    inline3480 = Equal
                }
            }
            commute_field3820 = inline3480
            switch commute_field3820 {
            case Equal:
                var compound_old10 int = index__79
                var compound_value11 int = 1
                var t2129 int = compound_old10 + compound_value11
                index__79 = t2129
                continue
            default:
                var t2131 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3820,
                }
                return t2131
            }
        } else {
            break Loop_loop2123
        }
    }
    var t2120 int
    var inline3492 int = vec_len__Vec_3int(self__76)
    t2120 = inline3492
    var t2121 int
    var inline3490 int = vec_len__Vec_3int(other__77)
    t2121 = inline3490
    var inline3484 bool = t2120 < t2121
    var inline3486 Ordering
    if inline3484 {
        inline3486 = Less
    } else {
        var inline3488 bool = t2120 > t2121
        if inline3488 {
            inline3486 = Greater
        } else {
            inline3486 = Equal
        }
    }
    var inline3487 Option__Ordering = Option__Ordering_Some{
        _0: inline3486,
    }
    return inline3487
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__96 Option__int, other__97 Option__int) Option__Ordering {
    switch other__97.(type) {
    case Option__int_None:
        switch self__96.(type) {
        case Option__int_None:
            var t2143 Option__Ordering = Option__Ordering_Some{
                _0: Equal,
            }
            return t2143
        case Option__int_Some:
            var t2144 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2144
        default:
            panic("non-exhaustive match")
        }
    case Option__int_Some:
        var x33 int = other__97.(Option__int_Some)._0
        switch self__96.(type) {
        case Option__int_None:
            var t2147 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            return t2147
        case Option__int_Some:
            var x35 int = self__96.(Option__int_Some)._0
            var inline3498 bool = x35 < x33
            var inline3500 Ordering
            if inline3498 {
                inline3500 = Less
            } else {
                var inline3502 bool = x35 > x33
                if inline3502 {
                    inline3500 = Greater
                } else {
                    inline3500 = Equal
                }
            }
            var inline3501 Option__Ordering = Option__Ordering_Some{
                _0: inline3500,
            }
            return inline3501
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
            var inline3504 bool = x47 < x45
            var inline3506 Ordering
            if inline3504 {
                inline3506 = Less
            } else {
                var inline3508 bool = x47 > x45
                if inline3508 {
                    inline3506 = Greater
                } else {
                    inline3506 = Equal
                }
            }
            var inline3507 Option__Ordering = Option__Ordering_Some{
                _0: inline3506,
            }
            return inline3507
        case Err:
            var t2156 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            return t2156
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var x46 string = other__105.(Err)._0
        switch self__104.(type) {
        case Ok:
            var t2159 Option__Ordering = Option__Ordering_Some{
                _0: Less,
            }
            return t2159
        case Err:
            var x50 string = self__104.(Err)._0
            var inline3510 bool = x50 < x46
            var inline3512 Ordering
            if inline3510 {
                inline3512 = Less
            } else {
                var inline3514 bool = x50 > x46
                if inline3514 {
                    inline3512 = Greater
                } else {
                    inline3512 = Equal
                }
            }
            var inline3513 Option__Ordering = Option__Ordering_Some{
                _0: inline3512,
            }
            return inline3513
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__86 []int, other__87 []int) Option__Ordering {
    var t2178 int
    var inline3538 int = len(self__86)
    t2178 = inline3538
    var t2179 int
    var inline3536 int = len(other__87)
    t2179 = inline3536
    var t2180 bool = t2178 < t2179
    var jp2164 int
    if t2180 {
        var inline3516 int = len(self__86)
        jp2164 = inline3516
    } else {
        var inline3518 int = len(other__87)
        jp2164 = inline3518
    }
    var index__89 int = 0
    Loop_loop2169:
    for {
        var t2170 bool = index__89 < jp2164
        if t2170 {
            var t2171 int = self__86[index__89]
            var t2172 int = other__87[index__89]
            var commute_field3823 Ordering
            var inline3520 bool = t2171 < t2172
            var inline3522 Ordering
            if inline3520 {
                inline3522 = Less
            } else {
                var inline3524 bool = t2171 > t2172
                if inline3524 {
                    inline3522 = Greater
                } else {
                    inline3522 = Equal
                }
            }
            commute_field3823 = inline3522
            switch commute_field3823 {
            case Equal:
                var compound_old21 int = index__89
                var compound_value22 int = 1
                var t2175 int = compound_old21 + compound_value22
                index__89 = t2175
                continue
            default:
                var t2177 Option__Ordering = Option__Ordering_Some{
                    _0: commute_field3823,
                }
                return t2177
            }
        } else {
            break Loop_loop2169
        }
    }
    var t2166 int
    var inline3534 int = len(self__86)
    t2166 = inline3534
    var t2167 int
    var inline3532 int = len(other__87)
    t2167 = inline3532
    var inline3526 bool = t2166 < t2167
    var inline3528 Ordering
    if inline3526 {
        inline3528 = Less
    } else {
        var inline3530 bool = t2166 > t2167
        if inline3530 {
            inline3528 = Greater
        } else {
            inline3528 = Equal
        }
    }
    var inline3529 Option__Ordering = Option__Ordering_Some{
        _0: inline3528,
    }
    return inline3529
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__223 *_goml_vec_int, other__224 *_goml_vec_int) bool {
    var t2193 int
    var inline3546 int = vec_len__Vec_3int(self__223)
    t2193 = inline3546
    var t2194 int
    var inline3544 int = vec_len__Vec_3int(other__224)
    t2194 = inline3544
    var t2195 bool = t2193 != t2194
    if t2195 {
        return false
    } else {
        var index__225 int = 0
        Loop_loop2197:
        for {
            var t2198 int
            var inline3542 int = vec_len__Vec_3int(self__223)
            t2198 = inline3542
            var t2199 bool = index__225 < t2198
            if t2199 {
                var t2201 int = vec_get__Vec_3int(self__223, index__225)
                var t2202 int = vec_get__Vec_3int(other__224, index__225)
                var t2203 bool
                var inline3540 bool = t2201 == t2202
                t2203 = inline3540
                if t2203 {
                    var compound_old153 int = index__225
                    var compound_value154 int = 1
                    var t2204 int = compound_old153 + compound_value154
                    index__225 = t2204
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2197
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__226 *_goml_vec_int) uint64 {
    var value__227 uint64 = 14695981039346656037
    var index__228 int = 0
    Loop_loop2209:
    for {
        var t2210 int
        var inline3550 int = vec_len__Vec_3int(self__226)
        t2210 = inline3550
        var t2211 bool = index__228 < t2210
        if t2211 {
            var t2212 uint64 = value__227 * 1099511628211
            var t2213 int = vec_get__Vec_3int(self__226, index__228)
            var t2214 uint64
            var inline3548 uint64 = _goml_runtime_core_int_hash(t2213)
            t2214 = inline3548
            var t2215 uint64 = t2212 + t2214
            value__227 = t2215
            var compound_old158 int = index__228
            var compound_value159 int = 1
            var t2216 int = compound_old158 + compound_value159
            index__228 = t2216
            continue
        } else {
            break Loop_loop2209
        }
    }
    return value__227
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2222 int = self._0
    var t2223 int = other._0
    var t2224 bool
    var inline3554 bool = t2222 == t2223
    t2224 = inline3554
    if t2224 {
        var t2227 string = self._1
        var t2228 string = other._1
        var t2229 bool
        var inline3552 bool = t2227 == t2228
        t2229 = inline3552
        if t2229 {
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
    var t2232 uint64 = _structural_hash_0 * 1099511628211
    var t2233 int = self._0
    var t2234 uint64
    var inline3558 uint64 = _goml_runtime_core_int_hash(t2233)
    t2234 = inline3558
    var _structural_hash_1 uint64 = t2232 + t2234
    var t2235 uint64 = _structural_hash_1 * 1099511628211
    var t2236 string = self._1
    var t2237 uint64
    var inline3556 uint64 = _goml_runtime_core_string_hash(t2236)
    t2237 = inline3556
    var _structural_hash_2 uint64 = t2235 + t2237
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2242 int = array_get__Array_2_3int(self, 0)
    var t2243 int = array_get__Array_2_3int(other, 0)
    var t2244 bool
    var inline3562 bool = t2242 == t2243
    t2244 = inline3562
    if t2244 {
        var t2247 int = array_get__Array_2_3int(self, 1)
        var t2248 int = array_get__Array_2_3int(other, 1)
        var t2249 bool
        var inline3560 bool = t2247 == t2248
        t2249 = inline3560
        if t2249 {
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
    var t2252 uint64 = _structural_hash_0 * 1099511628211
    var t2253 int = array_get__Array_2_3int(self, 0)
    var t2254 uint64
    var inline3566 uint64 = _goml_runtime_core_int_hash(t2253)
    t2254 = inline3566
    var _structural_hash_1 uint64 = t2252 + t2254
    var t2255 uint64 = _structural_hash_1 * 1099511628211
    var t2256 int = array_get__Array_2_3int(self, 1)
    var t2257 uint64
    var inline3564 uint64 = _goml_runtime_core_int_hash(t2256)
    t2257 = inline3564
    var _structural_hash_2 uint64 = t2255 + t2257
    return _structural_hash_2
}

func main() {
    main0()
}
