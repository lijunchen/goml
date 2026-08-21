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
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
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
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
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
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
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

type PartialLevel struct {
    _tag int32
    _v0_0 float64
}

type Option__Ordering struct {
    _tag int32
    _v1_0 Ordering
}

type Phantom__NoTraits int32

const (
    First Phantom__NoTraits = 0
    Second Phantom__NoTraits = 1
)

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Result__int__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__32 int, other__33 int) Ordering {
    var t707 bool = self__32 < other__33
    if t707 {
        return Less
    } else {
        var t710 bool = self__32 > other__33
        if t710 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) Option__Ordering {
    var t1059 int = self__2.major
    var t1060 int = other__3.major
    var commute_field3569 Ordering
    var inline2651 bool = t1059 < t1060
    var inline2653 Ordering
    if inline2651 {
        inline2653 = Less
    } else {
        var inline2655 bool = t1059 > t1060
        if inline2655 {
            inline2653 = Greater
        } else {
            inline2653 = Equal
        }
    }
    commute_field3569 = inline2653
    switch commute_field3569 {
    case Equal:
        var t1065 int = self__2.minor
        var t1066 int = other__3.minor
        var commute_field3566 Ordering
        var inline2645 bool = t1065 < t1066
        var inline2647 Ordering
        if inline2645 {
            inline2647 = Less
        } else {
            var inline2649 bool = t1065 > t1066
            if inline2649 {
                inline2647 = Greater
            } else {
                inline2647 = Equal
            }
        }
        commute_field3566 = inline2647
        switch commute_field3566 {
        case Equal:
            var t1071 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t1071
        default:
            var t1072 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field3566,
            }
            return t1072
        }
    default:
        var t1073 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: commute_field3569,
        }
        return t1073
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline2657 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline2657._tag {
    case 0:
        return false
    case 1:
        var inline2658 Ordering = inline2657._v1_0
        var inline2660 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2658, Less)
        return inline2660
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) Ordering {
    var t1088 int = self__6.major
    var t1089 int = other__7.major
    var _goml_m__i_derive1__ordering____8 Ordering
    var inline2684 bool = t1088 < t1089
    if inline2684 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline2685 bool = t1088 > t1089
        if inline2685 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t1092 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t1092 = false
    case Equal:
        t1092 = true
    case Greater:
        t1092 = false
    default:
        panic("non-exhaustive match")
    }
    if t1092 {
        var t1093 int = self__6.minor
        var t1094 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 Ordering
        var inline2680 bool = t1093 < t1094
        if inline2680 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline2681 bool = t1093 > t1094
            if inline2681 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t1097 bool
        switch _goml_m__i_derive0__ordering____9 {
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
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) Option__Ordering {
    var jp1133 int
    switch self__23.(type) {
    case Low:
        jp1133 = 0
    case Medium:
        jp1133 = 1
    case High:
        jp1133 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1135 int
    switch other__24.(type) {
    case Low:
        jp1135 = 0
    case Medium:
        jp1135 = 1
    case High:
        jp1135 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1138 bool = jp1133 < jp1135
    if t1138 {
        var t1139 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t1139
    } else {
        var t1142 bool = jp1133 > jp1135
        if t1142 {
            var t1143 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t1143
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t1148 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1148
                default:
                    var t1149 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1149
                }
            case Medium:
                var x448 int = other__24.(Medium)._0
                var x449 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x456 int = self__23.(Medium)._0
                    var x457 int = self__23.(Medium)._1
                    var commute_field3575 Ordering
                    var inline2705 bool = x456 < x448
                    var inline2707 Ordering
                    if inline2705 {
                        inline2707 = Less
                    } else {
                        var inline2709 bool = x456 > x448
                        if inline2709 {
                            inline2707 = Greater
                        } else {
                            inline2707 = Equal
                        }
                    }
                    commute_field3575 = inline2707
                    switch commute_field3575 {
                    case Equal:
                        var commute_field3572 Ordering
                        var inline2699 bool = x457 < x449
                        var inline2701 Ordering
                        if inline2699 {
                            inline2701 = Less
                        } else {
                            var inline2703 bool = x457 > x449
                            if inline2703 {
                                inline2701 = Greater
                            } else {
                                inline2701 = Equal
                            }
                        }
                        commute_field3572 = inline2701
                        switch commute_field3572 {
                        case Equal:
                            var t1160 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1160
                        default:
                            var t1161 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field3572,
                            }
                            return t1161
                        }
                    default:
                        var t1162 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: commute_field3575,
                        }
                        return t1162
                    }
                default:
                    var t1163 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1163
                }
            case High:
                var x450 int = other__24.(High)._0
                var x451 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x466 int = self__23.(High)._0
                    var x467 int = self__23.(High)._1
                    var commute_field3581 Ordering
                    var inline2717 bool = x466 < x450
                    var inline2719 Ordering
                    if inline2717 {
                        inline2719 = Less
                    } else {
                        var inline2721 bool = x466 > x450
                        if inline2721 {
                            inline2719 = Greater
                        } else {
                            inline2719 = Equal
                        }
                    }
                    commute_field3581 = inline2719
                    switch commute_field3581 {
                    case Equal:
                        var commute_field3578 Ordering
                        var inline2711 bool = x467 < x451
                        var inline2713 Ordering
                        if inline2711 {
                            inline2713 = Less
                        } else {
                            var inline2715 bool = x467 > x451
                            if inline2715 {
                                inline2713 = Greater
                            } else {
                                inline2713 = Equal
                            }
                        }
                        commute_field3578 = inline2713
                        switch commute_field3578 {
                        case Equal:
                            var t1174 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1174
                        default:
                            var t1175 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field3578,
                            }
                            return t1175
                        }
                    default:
                        var t1176 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: commute_field3581,
                        }
                        return t1176
                    }
                default:
                    var t1177 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1177
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline2723 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline2723._tag {
    case 0:
        return false
    case 1:
        var inline2724 Ordering = inline2723._v1_0
        var inline2726 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline2724, Less)
        return inline2726
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) Ordering {
    var jp1193 int
    switch self__39.(type) {
    case Low:
        jp1193 = 0
    case Medium:
        jp1193 = 1
    case High:
        jp1193 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1195 int
    switch other__40.(type) {
    case Low:
        jp1195 = 0
    case Medium:
        jp1195 = 1
    case High:
        jp1195 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1198 bool = jp1193 < jp1195
    if t1198 {
        return Less
    } else {
        var t1201 bool = jp1193 > jp1195
        if t1201 {
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
                var x483 int = other__40.(Medium)._0
                var x484 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x491 int = self__39.(Medium)._0
                    var x492 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 Ordering
                    var inline2750 bool = x491 < x483
                    if inline2750 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline2751 bool = x491 > x483
                        if inline2751 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1210 bool
                    switch _goml_m__i_derive7__ordering____47 {
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
                        var _goml_m__i_derive4__ordering____48 Ordering
                        var inline2746 bool = x492 < x484
                        if inline2746 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline2747 bool = x492 > x484
                            if inline2747 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1213 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1213 = false
                        case Equal:
                            t1213 = true
                        case Greater:
                            t1213 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1213 {
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
                var x485 int = other__40.(High)._0
                var x486 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x497 int = self__39.(High)._0
                    var x498 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x497, x485)
                    var t1218 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1218 {
                        var _goml_m__i_derive10__ordering____54 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(x498, x486)
                        var t1221 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1221 = false
                        case Equal:
                            t1221 = true
                        case Greater:
                            t1221 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1221 {
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
    var t1244 float64 = self__60.value
    var t1245 float64 = other__61.value
    var inline2762 bool = t1244 == t1245
    return inline2762
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) Option__Ordering {
    var t1249 float64 = self__62.value
    var t1250 float64 = other__63.value
    var commute_field3584 Ordering
    var inline2764 bool = t1249 < t1250
    if inline2764 {
        commute_field3584 = Less
        switch commute_field3584 {
        case Equal:
            var t1255 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t1255
        default:
            var t1256 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field3584,
            }
            return t1256
        }
    } else {
        var inline2766 bool = t1249 > t1250
        if inline2766 {
            commute_field3584 = Greater
            switch commute_field3584 {
            case Equal:
                var t1255 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                return t1255
            default:
                var t1256 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field3584,
                }
                return t1256
            }
        } else {
            var inline2768 bool = t1249 == t1250
            if inline2768 {
                commute_field3584 = Equal
                switch commute_field3584 {
                case Equal:
                    var t1255 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1255
                default:
                    var t1256 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: commute_field3584,
                    }
                    return t1256
                }
            } else {
                return Option__Ordering{
                    _tag: 0,
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(self__95 PartialLevel, other__96 PartialLevel) Option__Ordering {
    var jp1283 int
    switch self__95._tag {
    case 0:
        jp1283 = 0
    case 1:
        jp1283 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1285 int
    switch other__96._tag {
    case 0:
        jp1285 = 0
    case 1:
        jp1285 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1288 bool = jp1283 < jp1285
    if t1288 {
        var t1289 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t1289
    } else {
        var t1292 bool = jp1283 > jp1285
        if t1292 {
            var t1293 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t1293
        } else {
            switch other__96._tag {
            case 0:
                var x535 float64 = other__96._v0_0
                switch self__95._tag {
                case 0:
                    var x536 float64 = self__95._v0_0
                    var commute_field3587 Ordering
                    var inline2795 bool = x536 < x535
                    if inline2795 {
                        commute_field3587 = Less
                        switch commute_field3587 {
                        case Equal:
                            var t1302 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1302
                        default:
                            var t1303 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field3587,
                            }
                            return t1303
                        }
                    } else {
                        var inline2797 bool = x536 > x535
                        if inline2797 {
                            commute_field3587 = Greater
                            switch commute_field3587 {
                            case Equal:
                                var t1302 Option__Ordering = Option__Ordering{
                                    _tag: 1,
                                    _v1_0: Equal,
                                }
                                return t1302
                            default:
                                var t1303 Option__Ordering = Option__Ordering{
                                    _tag: 1,
                                    _v1_0: commute_field3587,
                                }
                                return t1303
                            }
                        } else {
                            var inline2799 bool = x536 == x535
                            if inline2799 {
                                commute_field3587 = Equal
                                switch commute_field3587 {
                                case Equal:
                                    var t1302 Option__Ordering = Option__Ordering{
                                        _tag: 1,
                                        _v1_0: Equal,
                                    }
                                    return t1302
                                default:
                                    var t1303 Option__Ordering = Option__Ordering{
                                        _tag: 1,
                                        _v1_0: commute_field3587,
                                    }
                                    return t1303
                                }
                            } else {
                                return Option__Ordering{
                                    _tag: 0,
                                }
                            }
                        }
                    }
                default:
                    var t1304 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1304
                }
            case 1:
                switch self__95._tag {
                case 1:
                    var t1307 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1307
                default:
                    var t1308 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1308
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
    var t1326 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1327 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1326)
    println__T_string(t1327)
    var t1328 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1329 string = ordering_name(t1328)
    println__T_string(t1329)
    var t1330 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1331 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1330)
    var t1332 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1331)
    println__T_string(t1332)
    var t1333 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1334 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1335 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1333, t1334)
    var t1336 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1335)
    println__T_string(t1336)
    var t1337 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1338 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1339 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1337, t1338)
    var t1340 string = ordering_name(t1339)
    println__T_string(t1340)
    var zero__105 float64 = 0
    var t1341 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1341,
    }
    var t1342 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1343 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1342)
    println__T_string(t1343)
    var t1344 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1345 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1344)
    var t1346 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1345)
    println__T_string(t1346)
    var t1347 [1]int = [1]int{3}
    var t1348 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [1]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1347)
    var generic_first__107 GenericPair__int = GenericPair__int{
        first: 1,
        second: 2,
        nested: t1348,
    }
    var t1349 [1]int = [1]int{0}
    var t1350 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [1]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1349)
    var generic_second__108 GenericPair__int = GenericPair__int{
        first: 1,
        second: 3,
        nested: t1350,
    }
    var t1351 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(generic_first__107, generic_second__108)
    var t1352 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1351)
    println__T_string(t1352)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1353 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1354 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1353)
    println__T_string(t1354)
    var t1355 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = PartialLevel{
        _tag: 0,
        _v0_0: t1355,
    }
    var t1356 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1357 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1356)
    var t1358 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1357)
    println__T_string(t1358)
    var t1359 [2]int = [2]int{1, 2}
    var first_values__112 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1359)
    var t1360 [2]int = [2]int{1, 3}
    var second_values__113 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1360)
    var t1361 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(first_values__112, second_values__113)
    var t1362 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1361)
    println__T_string(t1362)
    var t1363 Option__int = Option__int{
        _tag: 1,
        _v1_0: 2,
    }
    var t1364 Option__int = Option__int{
        _tag: 1,
        _v1_0: 3,
    }
    var t1365 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(t1363, t1364)
    var t1366 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1365)
    println__T_string(t1366)
    var ok__114 Result__int__string = Result__int__string{
        _tag: 0,
        _v0_0: 1,
    }
    var error__115 Result__int__string = Result__int__string{
        _tag: 1,
        _v1_0: "error",
    }
    var t1367 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(ok__114, error__115)
    var t1368 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1367)
    println__T_string(t1368)
    var t1369 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(first_values__112, 0, 2)
    var t1370 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(second_values__113, 0, 2)
    var t1371 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(t1369, t1370)
    var t1372 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1371)
    println__T_string(t1372)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(values__116, first_values__112, "vector")
    var t1373 [2]int = [2]int{1, 2}
    var t1374 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1373)
    var t1375 Option__string = _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(values__116, t1374)
    var t1376 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1375, "missing")
    println__T_string(t1376)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default()
    var t1415 int = default_tuple__117._0
    var t1416 int = 0
    var t1417 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1415, t1416)
    var jp1378 bool
    if t1417 {
        var t1418 string = default_tuple__117._1
        var t1419 string = ""
        var inline2824 bool = t1418 == t1419
        jp1378 = inline2824
    } else {
        jp1378 = false
    }
    var t1379 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1378)
    println__T_string(t1379)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default()
    var _eq_rhs560 [3]int = [3]int{0, 0, 0}
    var t1404 int = array_get__Array_3_3int(default_array__118, 0)
    var t1405 int = array_get__Array_3_3int(_eq_rhs560, 0)
    var t1406 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t1404, t1405)
    var jp1381 bool
    if t1406 {
        var t1409 int = array_get__Array_3_3int(default_array__118, 1)
        var t1410 int = array_get__Array_3_3int(_eq_rhs560, 1)
        var t1411 bool
        var inline2828 bool = t1409 == t1410
        t1411 = inline2828
        if t1411 {
            var t1412 int = array_get__Array_3_3int(default_array__118, 2)
            var t1413 int = array_get__Array_3_3int(_eq_rhs560, 2)
            var inline2826 bool = t1412 == t1413
            jp1381 = inline2826
        } else {
            jp1381 = false
        }
    } else {
        jp1381 = false
    }
    var t1382 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1381)
    println__T_string(t1382)
    var t1383 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1384 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1385 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(t1383, t1384)
    var t1386 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1385)
    var inline2869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1386)
    _goml_runtime_core_string_println(inline2869)
    var t1387 [2]int = [2]int{1, 2}
    var t1388 [2]int = [2]int{1, 3}
    var t1389 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(t1387, t1388)
    var t1390 string = ordering_name(t1389)
    var inline2866 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1390)
    _goml_runtime_core_string_println(inline2866)
    var t1391 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_3int_7float64 = Tuple2_3int_7float64{
        _0: 0,
        _1: t1391,
    }
    var t1392 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1393 bool
    var inline2863 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t1392)
    var inline2864 bool = !inline2863
    t1393 = inline2864
    var t1394 string
    var inline2861 string = _goml_runtime_core_bool_to_string(t1393)
    t1394 = inline2861
    var inline2858 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1394)
    _goml_runtime_core_string_println(inline2858)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline2856 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline2856
    var t1395 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline2853 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1395, inline2853)
    var t1396 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1397 Option__string
    var inline2851 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1396)
    t1397 = inline2851
    var t1398 string
    var inline2847 string = "missing"
    switch t1397._tag {
    case 0:
        t1398 = inline2847
    case 1:
        var inline2848 string = t1397._v1_0
        t1398 = inline2848
    default:
        panic("non-exhaustive match")
    }
    var inline2844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1398)
    _goml_runtime_core_string_println(inline2844)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline2842 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline2842
    var t1399 [2]int = [2]int{1, 2}
    var inline2839 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1399, inline2839)
    var t1400 [2]int = [2]int{1, 2}
    var t1401 Option__string
    var inline2837 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1400)
    t1401 = inline2837
    var t1402 string
    var inline2833 string = "missing"
    switch t1401._tag {
    case 0:
        t1402 = inline2833
    case 1:
        var inline2834 string = t1401._v1_0
        t1402 = inline2834
    default:
        panic("non-exhaustive match")
    }
    var inline2830 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1402)
    _goml_runtime_core_string_println(inline2830)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(self__854 Ordering, other__855 Ordering) bool {
    switch self__854 {
    case Less:
        switch other__855 {
        case Less:
            return true
        default:
            return false
        }
    case Equal:
        switch other__855 {
        case Equal:
            return true
        default:
            return false
        }
    case Greater:
        switch other__855 {
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
    var commute_field3650 Ordering
    var inline2993 bool = self__0 < other__1
    var inline2995 Ordering
    if inline2993 {
        inline2995 = Less
    } else {
        var inline2997 bool = self__0 > other__1
        if inline2997 {
            inline2995 = Greater
        } else {
            inline2995 = Equal
        }
    }
    commute_field3650 = inline2995
    switch commute_field3650 {
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
    var t1807 bool = self__185 == other__186
    return t1807
}

func println__T_string(value__1 string) struct{} {
    var t1903 string
    t1903 = value__1
    _goml_runtime_core_string_println(t1903)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1907 string = _goml_runtime_core_bool_to_string(self__148)
    return t1907
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(self__466 Option__Ordering) bool {
    var t1910 bool
    switch self__466._tag {
    case 0:
        t1910 = false
    case 1:
        t1910 = true
    default:
        panic("non-exhaustive match")
    }
    var t1911 bool = !t1910
    return t1911
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_lt(default_arg0 GenericPair__int, default_arg1 GenericPair__int) bool {
    var inline3388 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3388._tag {
    case 0:
        return false
    case 1:
        var inline3389 Ordering = inline3388._v1_0
        var inline3391 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3389, Less)
        return inline3391
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3393 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3393._tag {
    case 0:
        return false
    case 1:
        var inline3394 Ordering = inline3393._v1_0
        var inline3396 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3394, Less)
        return inline3396
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3398 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3398._tag {
    case 0:
        return false
    case 1:
        var inline3399 Ordering = inline3398._v1_0
        var inline3401 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3399, Less)
        return inline3401
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_lt(default_arg0 Option__int, default_arg1 Option__int) bool {
    var inline3403 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(default_arg0, default_arg1)
    switch inline3403._tag {
    case 0:
        return false
    case 1:
        var inline3404 Ordering = inline3403._v1_0
        var inline3406 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3404, Less)
        return inline3406
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____int____string_i_lt(default_arg0 Result__int__string, default_arg1 Result__int__string) bool {
    var inline3408 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(default_arg0, default_arg1)
    switch inline3408._tag {
    case 0:
        return false
    case 1:
        var inline3409 Ordering = inline3408._v1_0
        var inline3411 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3409, Less)
        return inline3411
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int(self__311 *_goml_vec_int, start__312 int, end__313 int) []int {
    var t1929 []int = self__311.items[start__312:end__313]
    return t1929
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3413 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3413._tag {
    case 0:
        return false
    case 1:
        var inline3414 Ordering = inline3413._v1_0
        var inline3416 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3414, Less)
        return inline3416
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hb3faab5fbb375c42497e028b370acdb2_r_____V__string() *hashmap_Vec_3int_string_x {
    var t1935 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t1935
}

func _goml_m_inherent_i_HashMap_i_H_h3b61239acaedb5e8f14c03ddfc2f1db6_r_____V__string(self__422 *hashmap_Vec_3int_string_x, key__423 *_goml_vec_int, value__424 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hb4451e8158a298bbb7da359694bb16fc_r_____V__string(self__420 *hashmap_Vec_3int_string_x, key__421 *_goml_vec_int) Option__string {
    var t1940 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__420, key__421)
    return t1940
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__467 Option__string, fallback__468 string) string {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 string = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_int_c_string_q__i_default() Tuple2_3int_6string {
    var t1947 int
    t1947 = 0
    var t1948 string
    t1948 = ""
    var t1949 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t1947,
        _1: t1948,
    }
    return t1949
}

func _goml_m_trait__impl_i_Default_i__l_int_x3b_3_r__i_default() [3]int {
    var t1955 int
    t1955 = 0
    var t1956 int
    t1956 = 0
    var t1957 int
    t1957 = 0
    var t1958 [3]int = [3]int{t1955, t1956, t1957}
    return t1958
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_int_c_int_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t1963 int = self._0
    var t1964 int = other._0
    var t1965 bool
    var inline3429 bool = t1963 == t1964
    t1965 = inline3429
    if t1965 {
        var t1968 int = self._1
        var t1969 int = other._1
        var t1970 bool
        var inline3425 bool = t1968 == t1969
        t1970 = inline3425
        if t1970 {
            return false
        } else {
            var t1971 int = self._1
            var t1972 int = other._1
            var inline3423 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1971, t1972)
            return inline3423
        }
    } else {
        var t1974 int = self._0
        var t1975 int = other._0
        var inline3427 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__int(t1974, t1975)
        return inline3427
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_int_x3b_2_r__i_cmp(self [2]int, other [2]int) Ordering {
    var t1979 int = array_get__Array_2_3int(self, 0)
    var t1980 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 Ordering
    var inline3436 bool = t1979 < t1980
    if inline3436 {
        _structural_ordering_0 = Less
    } else {
        var inline3437 bool = t1979 > t1980
        if inline3437 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t1983 bool
    switch _structural_ordering_0 {
    case Less:
        t1983 = false
    case Equal:
        t1983 = true
    case Greater:
        t1983 = false
    default:
        panic("non-exhaustive match")
    }
    if t1983 {
        var t1984 int = array_get__Array_2_3int(self, 1)
        var t1985 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 Ordering
        var inline3432 bool = t1984 < t1985
        if inline3432 {
            _structural_ordering_1 = Less
        } else {
            var inline3433 bool = t1984 > t1985
            if inline3433 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t1988 bool
        switch _structural_ordering_1 {
        case Less:
            t1988 = false
        case Equal:
            t1988 = true
        case Greater:
            t1988 = false
        default:
            panic("non-exhaustive match")
        }
        if t1988 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cm_h13f72987621c6328b14d0237c229fa31__i_partial__cmp(self Tuple2_3int_7float64, other Tuple2_3int_7float64) Option__Ordering {
    var t1991 int = self._0
    var t1992 int = other._0
    var _structural_partial_ordering_0 Option__Ordering
    var commute_field3809 Ordering
    var inline3448 bool = t1991 < t1992
    var inline3450 Ordering
    if inline3448 {
        inline3450 = Less
    } else {
        var inline3452 bool = t1991 > t1992
        if inline3452 {
            inline3450 = Greater
        } else {
            inline3450 = Equal
        }
    }
    var inline3451 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3450,
    }
    _structural_partial_ordering_0 = inline3451
    commute_field3809 = inline3450
    var t1997 bool
    switch commute_field3809 {
    case Less:
        t1997 = false
    case Equal:
        t1997 = true
    case Greater:
        t1997 = false
    default:
        panic("non-exhaustive match")
    }
    if t1997 {
        var t1998 float64 = self._1
        var t1999 float64 = other._1
        var _structural_partial_ordering_1 Option__Ordering
        var commute_field3806 Ordering
        var inline3440 bool = t1998 < t1999
        if inline3440 {
            var inline3441 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            _structural_partial_ordering_1 = inline3441
            commute_field3806 = Less
            var t2004 bool
            switch commute_field3806 {
            case Less:
                t2004 = false
            case Equal:
                t2004 = true
            case Greater:
                t2004 = false
            default:
                panic("non-exhaustive match")
            }
            if t2004 {
                var t2005 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                return t2005
            } else {
                return _structural_partial_ordering_1
            }
        } else {
            var inline3442 bool = t1998 > t1999
            if inline3442 {
                var inline3443 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Greater,
                }
                _structural_partial_ordering_1 = inline3443
                commute_field3806 = Greater
                var t2004 bool
                switch commute_field3806 {
                case Less:
                    t2004 = false
                case Equal:
                    t2004 = true
                case Greater:
                    t2004 = false
                default:
                    panic("non-exhaustive match")
                }
                if t2004 {
                    var t2005 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2005
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3444 bool = t1998 == t1999
                if inline3444 {
                    var inline3445 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    _structural_partial_ordering_1 = inline3445
                    commute_field3806 = Equal
                    var t2004 bool
                    switch commute_field3806 {
                    case Less:
                        t2004 = false
                    case Equal:
                        t2004 = true
                    case Greater:
                        t2004 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t2004 {
                        var t2005 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: Equal,
                        }
                        return t2005
                    } else {
                        return _structural_partial_ordering_1
                    }
                } else {
                    return Option__Ordering{
                        _tag: 0,
                    }
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

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__465 Option__Ordering) bool {
    switch self__465._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____int_i_partial__cmp(self__67 GenericPair__int, other__68 GenericPair__int) Option__Ordering {
    var t2067 int = self__67.first
    var t2068 int = other__68.first
    var commute_field3815 Ordering
    var inline3466 bool = t2067 < t2068
    var inline3468 Ordering
    if inline3466 {
        inline3468 = Less
    } else {
        var inline3470 bool = t2067 > t2068
        if inline3470 {
            inline3468 = Greater
        } else {
            inline3468 = Equal
        }
    }
    commute_field3815 = inline3468
    switch commute_field3815 {
    case Equal:
        var t2073 int = self__67.second
        var t2074 int = other__68.second
        var commute_field3812 Ordering
        var inline3460 bool = t2073 < t2074
        var inline3462 Ordering
        if inline3460 {
            inline3462 = Less
        } else {
            var inline3464 bool = t2073 > t2074
            if inline3464 {
                inline3462 = Greater
            } else {
                inline3462 = Equal
            }
        }
        commute_field3812 = inline3462
        switch commute_field3812 {
        case Equal:
            var t2079 *_goml_vec_int = self__67.nested
            var t2080 *_goml_vec_int = other__68.nested
            var mtmp509 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(t2079, t2080)
            switch mtmp509._tag {
            case 0:
                return Option__Ordering{
                    _tag: 0,
                }
            case 1:
                var x510 Ordering = mtmp509._v1_0
                switch x510 {
                case Equal:
                    var t2085 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2085
                default:
                    var t2086 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: x510,
                    }
                    return t2086
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t2087 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field3812,
            }
            return t2087
        }
    default:
        var t2088 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: commute_field3815,
        }
        return t2088
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) Option__Ordering {
    var jp2092 int
    switch self__83 {
    case First:
        jp2092 = 0
    case Second:
        jp2092 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp2094 int
    switch other__84 {
    case First:
        jp2094 = 0
    case Second:
        jp2094 = 1
    default:
        panic("non-exhaustive match")
    }
    var t2097 bool = jp2092 < jp2094
    if t2097 {
        var t2098 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t2098
    } else {
        var t2101 bool = jp2092 > jp2094
        if t2101 {
            var t2102 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2102
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t2107 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2107
                default:
                    var t2108 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2108
                }
            case Second:
                switch self__83 {
                case Second:
                    var t2111 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2111
                default:
                    var t2112 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2112
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_int_r__i_partial__cmp(self__76 *_goml_vec_int, other__77 *_goml_vec_int) Option__Ordering {
    var t2130 int
    var inline3494 int = vec_len__Vec_3int(self__76)
    t2130 = inline3494
    var t2131 int
    var inline3492 int = vec_len__Vec_3int(other__77)
    t2131 = inline3492
    var t2132 bool = t2130 < t2131
    var jp2116 int
    if t2132 {
        var inline3472 int = vec_len__Vec_3int(self__76)
        jp2116 = inline3472
    } else {
        var inline3474 int = vec_len__Vec_3int(other__77)
        jp2116 = inline3474
    }
    var index__79 int = 0
    Loop_loop2121:
    for {
        var t2122 bool = index__79 < jp2116
        if t2122 {
            var t2123 int = vec_get__Vec_3int(self__76, index__79)
            var t2124 int = vec_get__Vec_3int(other__77, index__79)
            var commute_field3818 Ordering
            var inline3476 bool = t2123 < t2124
            var inline3478 Ordering
            if inline3476 {
                inline3478 = Less
            } else {
                var inline3480 bool = t2123 > t2124
                if inline3480 {
                    inline3478 = Greater
                } else {
                    inline3478 = Equal
                }
            }
            commute_field3818 = inline3478
            switch commute_field3818 {
            case Equal:
                var compound_old10 int = index__79
                var compound_value11 int = 1
                var t2127 int = compound_old10 + compound_value11
                index__79 = t2127
                continue
            default:
                var t2129 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field3818,
                }
                return t2129
            }
        } else {
            break Loop_loop2121
        }
    }
    var t2118 int
    var inline3490 int = vec_len__Vec_3int(self__76)
    t2118 = inline3490
    var t2119 int
    var inline3488 int = vec_len__Vec_3int(other__77)
    t2119 = inline3488
    var inline3482 bool = t2118 < t2119
    var inline3484 Ordering
    if inline3482 {
        inline3484 = Less
    } else {
        var inline3486 bool = t2118 > t2119
        if inline3486 {
            inline3484 = Greater
        } else {
            inline3484 = Equal
        }
    }
    var inline3485 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3484,
    }
    return inline3485
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____int_i_partial__cmp(self__96 Option__int, other__97 Option__int) Option__Ordering {
    switch other__97._tag {
    case 0:
        switch self__96._tag {
        case 0:
            var t2141 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t2141
        case 1:
            var t2142 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2142
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x33 int = other__97._v1_0
        switch self__96._tag {
        case 0:
            var t2145 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            return t2145
        case 1:
            var x35 int = self__96._v1_0
            var inline3496 bool = x35 < x33
            var inline3498 Ordering
            if inline3496 {
                inline3498 = Less
            } else {
                var inline3500 bool = x35 > x33
                if inline3500 {
                    inline3498 = Greater
                } else {
                    inline3498 = Equal
                }
            }
            var inline3499 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3498,
            }
            return inline3499
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cm_hfc86bc8120f8982d8d0ed6b909b6f353__i_partial__cmp(self__104 Result__int__string, other__105 Result__int__string) Option__Ordering {
    switch other__105._tag {
    case 0:
        var x45 int = other__105._v0_0
        switch self__104._tag {
        case 0:
            var x47 int = self__104._v0_0
            var inline3502 bool = x47 < x45
            var inline3504 Ordering
            if inline3502 {
                inline3504 = Less
            } else {
                var inline3506 bool = x47 > x45
                if inline3506 {
                    inline3504 = Greater
                } else {
                    inline3504 = Equal
                }
            }
            var inline3505 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3504,
            }
            return inline3505
        case 1:
            var t2154 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2154
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x46 string = other__105._v1_0
        switch self__104._tag {
        case 0:
            var t2157 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            return t2157
        case 1:
            var x50 string = self__104._v1_0
            var inline3508 bool = x50 < x46
            var inline3510 Ordering
            if inline3508 {
                inline3510 = Less
            } else {
                var inline3512 bool = x50 > x46
                if inline3512 {
                    inline3510 = Greater
                } else {
                    inline3510 = Equal
                }
            }
            var inline3511 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3510,
            }
            return inline3511
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_int_r__i_partial__cmp(self__86 []int, other__87 []int) Option__Ordering {
    var t2176 int
    var inline3536 int = len(self__86)
    t2176 = inline3536
    var t2177 int
    var inline3534 int = len(other__87)
    t2177 = inline3534
    var t2178 bool = t2176 < t2177
    var jp2162 int
    if t2178 {
        var inline3514 int = len(self__86)
        jp2162 = inline3514
    } else {
        var inline3516 int = len(other__87)
        jp2162 = inline3516
    }
    var index__89 int = 0
    Loop_loop2167:
    for {
        var t2168 bool = index__89 < jp2162
        if t2168 {
            var t2169 int = self__86[index__89]
            var t2170 int = other__87[index__89]
            var commute_field3821 Ordering
            var inline3518 bool = t2169 < t2170
            var inline3520 Ordering
            if inline3518 {
                inline3520 = Less
            } else {
                var inline3522 bool = t2169 > t2170
                if inline3522 {
                    inline3520 = Greater
                } else {
                    inline3520 = Equal
                }
            }
            commute_field3821 = inline3520
            switch commute_field3821 {
            case Equal:
                var compound_old21 int = index__89
                var compound_value22 int = 1
                var t2173 int = compound_old21 + compound_value22
                index__89 = t2173
                continue
            default:
                var t2175 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field3821,
                }
                return t2175
            }
        } else {
            break Loop_loop2167
        }
    }
    var t2164 int
    var inline3532 int = len(self__86)
    t2164 = inline3532
    var t2165 int
    var inline3530 int = len(other__87)
    t2165 = inline3530
    var inline3524 bool = t2164 < t2165
    var inline3526 Ordering
    if inline3524 {
        inline3526 = Less
    } else {
        var inline3528 bool = t2164 > t2165
        if inline3528 {
            inline3526 = Greater
        } else {
            inline3526 = Equal
        }
    }
    var inline3527 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3526,
    }
    return inline3527
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_int_r__i_eq(self__223 *_goml_vec_int, other__224 *_goml_vec_int) bool {
    var t2191 int
    var inline3544 int = vec_len__Vec_3int(self__223)
    t2191 = inline3544
    var t2192 int
    var inline3542 int = vec_len__Vec_3int(other__224)
    t2192 = inline3542
    var t2193 bool = t2191 != t2192
    if t2193 {
        return false
    } else {
        var index__225 int = 0
        Loop_loop2195:
        for {
            var t2196 int
            var inline3540 int = vec_len__Vec_3int(self__223)
            t2196 = inline3540
            var t2197 bool = index__225 < t2196
            if t2197 {
                var t2199 int = vec_get__Vec_3int(self__223, index__225)
                var t2200 int = vec_get__Vec_3int(other__224, index__225)
                var t2201 bool
                var inline3538 bool = t2199 == t2200
                t2201 = inline3538
                if t2201 {
                    var compound_old153 int = index__225
                    var compound_value154 int = 1
                    var t2202 int = compound_old153 + compound_value154
                    index__225 = t2202
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2195
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_int_r__i_hash(self__226 *_goml_vec_int) uint64 {
    var value__227 uint64 = 14695981039346656037
    var index__228 int = 0
    Loop_loop2207:
    for {
        var t2208 int
        var inline3548 int = vec_len__Vec_3int(self__226)
        t2208 = inline3548
        var t2209 bool = index__228 < t2208
        if t2209 {
            var t2210 uint64 = value__227 * 1099511628211
            var t2211 int = vec_get__Vec_3int(self__226, index__228)
            var t2212 uint64
            var inline3546 uint64 = _goml_runtime_core_int_hash(t2211)
            t2212 = inline3546
            var t2213 uint64 = t2210 + t2212
            value__227 = t2213
            var compound_old158 int = index__228
            var compound_value159 int = 1
            var t2214 int = compound_old158 + compound_value159
            index__228 = t2214
            continue
        } else {
            break Loop_loop2207
        }
    }
    return value__227
}

func _goml_m_trait__impl_i_PartialEq_i__o_int_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2220 int = self._0
    var t2221 int = other._0
    var t2222 bool
    var inline3552 bool = t2220 == t2221
    t2222 = inline3552
    if t2222 {
        var t2225 string = self._1
        var t2226 string = other._1
        var t2227 bool
        var inline3550 bool = t2225 == t2226
        t2227 = inline3550
        if t2227 {
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
    var t2230 uint64 = _structural_hash_0 * 1099511628211
    var t2231 int = self._0
    var t2232 uint64
    var inline3556 uint64 = _goml_runtime_core_int_hash(t2231)
    t2232 = inline3556
    var _structural_hash_1 uint64 = t2230 + t2232
    var t2233 uint64 = _structural_hash_1 * 1099511628211
    var t2234 string = self._1
    var t2235 uint64
    var inline3554 uint64 = _goml_runtime_core_string_hash(t2234)
    t2235 = inline3554
    var _structural_hash_2 uint64 = t2233 + t2235
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_int_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2240 int = array_get__Array_2_3int(self, 0)
    var t2241 int = array_get__Array_2_3int(other, 0)
    var t2242 bool
    var inline3560 bool = t2240 == t2241
    t2242 = inline3560
    if t2242 {
        var t2245 int = array_get__Array_2_3int(self, 1)
        var t2246 int = array_get__Array_2_3int(other, 1)
        var t2247 bool
        var inline3558 bool = t2245 == t2246
        t2247 = inline3558
        if t2247 {
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
    var t2250 uint64 = _structural_hash_0 * 1099511628211
    var t2251 int = array_get__Array_2_3int(self, 0)
    var t2252 uint64
    var inline3564 uint64 = _goml_runtime_core_int_hash(t2251)
    t2252 = inline3564
    var _structural_hash_1 uint64 = t2250 + t2252
    var t2253 uint64 = _structural_hash_1 * 1099511628211
    var t2254 int = array_get__Array_2_3int(self, 1)
    var t2255 uint64
    var inline3562 uint64 = _goml_runtime_core_int_hash(t2254)
    t2255 = inline3562
    var _structural_hash_2 uint64 = t2253 + t2255
    return _structural_hash_2
}

func main() {
    main0()
}
