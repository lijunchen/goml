package main

import (
    _goml_os "os"
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
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_uint32 struct {
    items []uint32
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Vec_l_isize_r__i_hash(key)
    var bucket []hashmap_Vec_3int_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Vec_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Vec_l_isize_r__i_eq(entry.key, key) {
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Vec_l_isize_r__i_hash(key)
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Vec_l_isize_r__i_eq(entry.key, key) {
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i__o_isize_c_string_q__i_hash(key)
    var bucket []hashmap_Tuple2_3int_6string_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Tuple2_3int_6string_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__o_isize_c_string_q__i_eq(entry.key, key) {
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i__o_isize_c_string_q__i_hash(key)
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__o_isize_c_string_q__i_eq(entry.key, key) {
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i__l_isize_x3b_2_r__i_hash(key)
    var bucket []hashmap_Array_2_3int_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Array_2_3int_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__l_isize_x3b_2_r__i_eq(entry.key, key) {
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i__l_isize_x3b_2_r__i_hash(key)
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i__l_isize_x3b_2_r__i_eq(entry.key, key) {
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

type Version struct {
    major int
    minor int
}

type MaybeNumber struct {
    value float64
}

type NoTraits struct {}

type GenericPair__isize struct {
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

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(self__32 int, other__33 int) Ordering {
    var t1092 bool = self__32 < other__33
    if t1092 {
        return Less
    } else {
        var t1095 bool = self__32 > other__33
        if t1095 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(self__2 Version, other__3 Version) Option__Ordering {
    var t1444 int = self__2.major
    var t1445 int = other__3.major
    var commute_field4018 Ordering
    var inline3079 bool = t1444 < t1445
    var inline3081 Ordering
    if inline3079 {
        inline3081 = Less
    } else {
        var inline3083 bool = t1444 > t1445
        if inline3083 {
            inline3081 = Greater
        } else {
            inline3081 = Equal
        }
    }
    commute_field4018 = inline3081
    switch commute_field4018 {
    case Equal:
        var t1450 int = self__2.minor
        var t1451 int = other__3.minor
        var commute_field4015 Ordering
        var inline3073 bool = t1450 < t1451
        var inline3075 Ordering
        if inline3073 {
            inline3075 = Less
        } else {
            var inline3077 bool = t1450 > t1451
            if inline3077 {
                inline3075 = Greater
            } else {
                inline3075 = Equal
            }
        }
        commute_field4015 = inline3075
        switch commute_field4015 {
        case Equal:
            var t1456 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t1456
        default:
            var t1457 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field4015,
            }
            return t1457
        }
    default:
        var t1458 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: commute_field4018,
        }
        return t1458
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(default_arg0 Version, default_arg1 Version) bool {
    var inline3085 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_partial__cmp(default_arg0, default_arg1)
    switch inline3085._tag {
    case 0:
        return false
    case 1:
        var inline3086 Ordering = inline3085._v1_0
        var inline3088 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3086, Less)
        return inline3088
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(self__6 Version, other__7 Version) Ordering {
    var t1473 int = self__6.major
    var t1474 int = other__7.major
    var _goml_m__i_derive1__ordering____8 Ordering
    var inline3112 bool = t1473 < t1474
    if inline3112 {
        _goml_m__i_derive1__ordering____8 = Less
    } else {
        var inline3113 bool = t1473 > t1474
        if inline3113 {
            _goml_m__i_derive1__ordering____8 = Greater
        } else {
            _goml_m__i_derive1__ordering____8 = Equal
        }
    }
    var t1477 bool
    switch _goml_m__i_derive1__ordering____8 {
    case Less:
        t1477 = false
    case Equal:
        t1477 = true
    case Greater:
        t1477 = false
    default:
        panic("non-exhaustive match")
    }
    if t1477 {
        var t1478 int = self__6.minor
        var t1479 int = other__7.minor
        var _goml_m__i_derive0__ordering____9 Ordering
        var inline3108 bool = t1478 < t1479
        if inline3108 {
            _goml_m__i_derive0__ordering____9 = Less
        } else {
            var inline3109 bool = t1478 > t1479
            if inline3109 {
                _goml_m__i_derive0__ordering____9 = Greater
            } else {
                _goml_m__i_derive0__ordering____9 = Equal
            }
        }
        var t1482 bool
        switch _goml_m__i_derive0__ordering____9 {
        case Less:
            t1482 = false
        case Equal:
            t1482 = true
        case Greater:
            t1482 = false
        default:
            panic("non-exhaustive match")
        }
        if t1482 {
            return Equal
        } else {
            return _goml_m__i_derive0__ordering____9
        }
    } else {
        return _goml_m__i_derive1__ordering____8
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(self__23 Level, other__24 Level) Option__Ordering {
    var jp1518 int
    switch self__23.(type) {
    case Low:
        jp1518 = 0
    case Medium:
        jp1518 = 1
    case High:
        jp1518 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1520 int
    switch other__24.(type) {
    case Low:
        jp1520 = 0
    case Medium:
        jp1520 = 1
    case High:
        jp1520 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1523 bool = jp1518 < jp1520
    if t1523 {
        var t1524 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t1524
    } else {
        var t1527 bool = jp1518 > jp1520
        if t1527 {
            var t1528 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t1528
        } else {
            switch other__24.(type) {
            case Low:
                switch self__23.(type) {
                case Low:
                    var t1533 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1533
                default:
                    var t1534 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1534
                }
            case Medium:
                var x833 int = other__24.(Medium)._0
                var x834 int = other__24.(Medium)._1
                switch self__23.(type) {
                case Medium:
                    var x841 int = self__23.(Medium)._0
                    var x842 int = self__23.(Medium)._1
                    var commute_field4024 Ordering
                    var inline3133 bool = x841 < x833
                    var inline3135 Ordering
                    if inline3133 {
                        inline3135 = Less
                    } else {
                        var inline3137 bool = x841 > x833
                        if inline3137 {
                            inline3135 = Greater
                        } else {
                            inline3135 = Equal
                        }
                    }
                    commute_field4024 = inline3135
                    switch commute_field4024 {
                    case Equal:
                        var commute_field4021 Ordering
                        var inline3127 bool = x842 < x834
                        var inline3129 Ordering
                        if inline3127 {
                            inline3129 = Less
                        } else {
                            var inline3131 bool = x842 > x834
                            if inline3131 {
                                inline3129 = Greater
                            } else {
                                inline3129 = Equal
                            }
                        }
                        commute_field4021 = inline3129
                        switch commute_field4021 {
                        case Equal:
                            var t1545 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1545
                        default:
                            var t1546 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field4021,
                            }
                            return t1546
                        }
                    default:
                        var t1547 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: commute_field4024,
                        }
                        return t1547
                    }
                default:
                    var t1548 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1548
                }
            case High:
                var x835 int = other__24.(High)._0
                var x836 int = other__24.(High)._1
                switch self__23.(type) {
                case High:
                    var x851 int = self__23.(High)._0
                    var x852 int = self__23.(High)._1
                    var commute_field4030 Ordering
                    var inline3145 bool = x851 < x835
                    var inline3147 Ordering
                    if inline3145 {
                        inline3147 = Less
                    } else {
                        var inline3149 bool = x851 > x835
                        if inline3149 {
                            inline3147 = Greater
                        } else {
                            inline3147 = Equal
                        }
                    }
                    commute_field4030 = inline3147
                    switch commute_field4030 {
                    case Equal:
                        var commute_field4027 Ordering
                        var inline3139 bool = x852 < x836
                        var inline3141 Ordering
                        if inline3139 {
                            inline3141 = Less
                        } else {
                            var inline3143 bool = x852 > x836
                            if inline3143 {
                                inline3141 = Greater
                            } else {
                                inline3141 = Equal
                            }
                        }
                        commute_field4027 = inline3141
                        switch commute_field4027 {
                        case Equal:
                            var t1559 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1559
                        default:
                            var t1560 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field4027,
                            }
                            return t1560
                        }
                    default:
                        var t1561 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: commute_field4030,
                        }
                        return t1561
                    }
                default:
                    var t1562 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1562
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(default_arg0 Level, default_arg1 Level) bool {
    var inline3151 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_partial__cmp(default_arg0, default_arg1)
    switch inline3151._tag {
    case 0:
        return false
    case 1:
        var inline3152 Ordering = inline3151._v1_0
        var inline3154 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3152, Less)
        return inline3154
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(self__39 Level, other__40 Level) Ordering {
    var jp1578 int
    switch self__39.(type) {
    case Low:
        jp1578 = 0
    case Medium:
        jp1578 = 1
    case High:
        jp1578 = 2
    default:
        panic("non-exhaustive match")
    }
    var jp1580 int
    switch other__40.(type) {
    case Low:
        jp1580 = 0
    case Medium:
        jp1580 = 1
    case High:
        jp1580 = 2
    default:
        panic("non-exhaustive match")
    }
    var t1583 bool = jp1578 < jp1580
    if t1583 {
        return Less
    } else {
        var t1586 bool = jp1578 > jp1580
        if t1586 {
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
                var x868 int = other__40.(Medium)._0
                var x869 int = other__40.(Medium)._1
                switch self__39.(type) {
                case Medium:
                    var x876 int = self__39.(Medium)._0
                    var x877 int = self__39.(Medium)._1
                    var _goml_m__i_derive7__ordering____47 Ordering
                    var inline3178 bool = x876 < x868
                    if inline3178 {
                        _goml_m__i_derive7__ordering____47 = Less
                    } else {
                        var inline3179 bool = x876 > x868
                        if inline3179 {
                            _goml_m__i_derive7__ordering____47 = Greater
                        } else {
                            _goml_m__i_derive7__ordering____47 = Equal
                        }
                    }
                    var t1595 bool
                    switch _goml_m__i_derive7__ordering____47 {
                    case Less:
                        t1595 = false
                    case Equal:
                        t1595 = true
                    case Greater:
                        t1595 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t1595 {
                        var _goml_m__i_derive4__ordering____48 Ordering
                        var inline3174 bool = x877 < x869
                        if inline3174 {
                            _goml_m__i_derive4__ordering____48 = Less
                        } else {
                            var inline3175 bool = x877 > x869
                            if inline3175 {
                                _goml_m__i_derive4__ordering____48 = Greater
                            } else {
                                _goml_m__i_derive4__ordering____48 = Equal
                            }
                        }
                        var t1598 bool
                        switch _goml_m__i_derive4__ordering____48 {
                        case Less:
                            t1598 = false
                        case Equal:
                            t1598 = true
                        case Greater:
                            t1598 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1598 {
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
                var x870 int = other__40.(High)._0
                var x871 int = other__40.(High)._1
                switch self__39.(type) {
                case High:
                    var x882 int = self__39.(High)._0
                    var x883 int = self__39.(High)._1
                    var _goml_m__i_derive13__ordering____53 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(x882, x870)
                    var t1603 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(_goml_m__i_derive13__ordering____53, Equal)
                    if t1603 {
                        var _goml_m__i_derive10__ordering____54 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(x883, x871)
                        var t1606 bool
                        switch _goml_m__i_derive10__ordering____54 {
                        case Less:
                            t1606 = false
                        case Equal:
                            t1606 = true
                        case Greater:
                            t1606 = false
                        default:
                            panic("non-exhaustive match")
                        }
                        if t1606 {
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
    var t1629 float64 = self__60.value
    var t1630 float64 = other__61.value
    var inline3190 bool = t1629 == t1630
    return inline3190
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(self__62 MaybeNumber, other__63 MaybeNumber) Option__Ordering {
    var t1634 float64 = self__62.value
    var t1635 float64 = other__63.value
    var commute_field4033 Ordering
    var inline3192 bool = t1634 < t1635
    if inline3192 {
        commute_field4033 = Less
        switch commute_field4033 {
        case Equal:
            var t1640 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t1640
        default:
            var t1641 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field4033,
            }
            return t1641
        }
    } else {
        var inline3194 bool = t1634 > t1635
        if inline3194 {
            commute_field4033 = Greater
            switch commute_field4033 {
            case Equal:
                var t1640 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                return t1640
            default:
                var t1641 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field4033,
                }
                return t1641
            }
        } else {
            var inline3196 bool = t1634 == t1635
            if inline3196 {
                commute_field4033 = Equal
                switch commute_field4033 {
                case Equal:
                    var t1640 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1640
                default:
                    var t1641 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: commute_field4033,
                    }
                    return t1641
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
    var jp1668 int
    switch self__95._tag {
    case 0:
        jp1668 = 0
    case 1:
        jp1668 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp1670 int
    switch other__96._tag {
    case 0:
        jp1670 = 0
    case 1:
        jp1670 = 1
    default:
        panic("non-exhaustive match")
    }
    var t1673 bool = jp1668 < jp1670
    if t1673 {
        var t1674 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t1674
    } else {
        var t1677 bool = jp1668 > jp1670
        if t1677 {
            var t1678 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t1678
        } else {
            switch other__96._tag {
            case 0:
                var x920 float64 = other__96._v0_0
                switch self__95._tag {
                case 0:
                    var x921 float64 = self__95._v0_0
                    var commute_field4036 Ordering
                    var inline3223 bool = x921 < x920
                    if inline3223 {
                        commute_field4036 = Less
                        switch commute_field4036 {
                        case Equal:
                            var t1687 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: Equal,
                            }
                            return t1687
                        default:
                            var t1688 Option__Ordering = Option__Ordering{
                                _tag: 1,
                                _v1_0: commute_field4036,
                            }
                            return t1688
                        }
                    } else {
                        var inline3225 bool = x921 > x920
                        if inline3225 {
                            commute_field4036 = Greater
                            switch commute_field4036 {
                            case Equal:
                                var t1687 Option__Ordering = Option__Ordering{
                                    _tag: 1,
                                    _v1_0: Equal,
                                }
                                return t1687
                            default:
                                var t1688 Option__Ordering = Option__Ordering{
                                    _tag: 1,
                                    _v1_0: commute_field4036,
                                }
                                return t1688
                            }
                        } else {
                            var inline3227 bool = x921 == x920
                            if inline3227 {
                                commute_field4036 = Equal
                                switch commute_field4036 {
                                case Equal:
                                    var t1687 Option__Ordering = Option__Ordering{
                                        _tag: 1,
                                        _v1_0: Equal,
                                    }
                                    return t1687
                                default:
                                    var t1688 Option__Ordering = Option__Ordering{
                                        _tag: 1,
                                        _v1_0: commute_field4036,
                                    }
                                    return t1688
                                }
                            } else {
                                return Option__Ordering{
                                    _tag: 0,
                                }
                            }
                        }
                    }
                default:
                    var t1689 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1689
                }
            case 1:
                switch self__95._tag {
                case 1:
                    var t1692 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1692
                default:
                    var t1693 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t1693
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
    var t1711 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Version_i_lt(first__103, second__104)
    var t1712 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1711)
    println__T_string(t1712)
    var t1713 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Version_i_cmp(first__103, second__104)
    var t1714 string = ordering_name(t1713)
    println__T_string(t1714)
    var t1715 Level = Medium{
        _0: 0,
        _1: 0,
    }
    var t1716 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(Low{}, t1715)
    var t1717 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1716)
    println__T_string(t1717)
    var t1718 Level = Medium{
        _0: 1,
        _1: 9,
    }
    var t1719 Level = Medium{
        _0: 2,
        _1: 0,
    }
    var t1720 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Level_i_lt(t1718, t1719)
    var t1721 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1720)
    println__T_string(t1721)
    var t1722 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1723 Level = High{
        _0: 3,
        _1: 1,
    }
    var t1724 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_Level_i_cmp(t1722, t1723)
    var t1725 string = ordering_name(t1724)
    println__T_string(t1725)
    var zero__105 float64 = 0
    var t1726 float64 = zero__105 / zero__105
    var nan__106 MaybeNumber = MaybeNumber{
        value: t1726,
    }
    var t1727 bool = _goml_m_trait__impl_i_PartialEq_i_MaybeNumber_i_eq(nan__106, nan__106)
    var t1728 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1727)
    println__T_string(t1728)
    var t1729 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_MaybeNumber_i_partial__cmp(nan__106, nan__106)
    var t1730 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1729)
    var t1731 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1730)
    println__T_string(t1731)
    var t1732 [1]int = [1]int{3}
    var t1733 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [1]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1732)
    var generic_first__107 GenericPair__isize = GenericPair__isize{
        first: 1,
        second: 2,
        nested: t1733,
    }
    var t1734 [1]int = [1]int{0}
    var t1735 *_goml_vec_int = func(values [1]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [1]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1734)
    var generic_second__108 GenericPair__isize = GenericPair__isize{
        first: 1,
        second: 3,
        nested: t1735,
    }
    var t1736 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____isize_i_lt(generic_first__107, generic_second__108)
    var t1737 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1736)
    println__T_string(t1737)
    var phantom_first__109 Phantom__NoTraits = First
    var phantom_second__110 Phantom__NoTraits = Second
    var t1738 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(phantom_first__109, phantom_second__110)
    var t1739 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1738)
    println__T_string(t1739)
    var t1740 float64 = zero__105 / zero__105
    var partial_nan__111 PartialLevel = PartialLevel{
        _tag: 0,
        _v0_0: t1740,
    }
    var t1741 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_PartialLevel_i_partial__cmp(partial_nan__111, partial_nan__111)
    var t1742 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(t1741)
    var t1743 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1742)
    println__T_string(t1743)
    var t1744 [2]int = [2]int{1, 2}
    var first_values__112 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1744)
    var t1745 [2]int = [2]int{1, 3}
    var second_values__113 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1745)
    var t1746 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_isize_r__i_lt(first_values__112, second_values__113)
    var t1747 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1746)
    println__T_string(t1747)
    var t1748 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 2,
    }
    var t1749 Option__isize = Option__isize{
        _tag: 1,
        _v1_0: 3,
    }
    var t1750 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____isize_i_lt(t1748, t1749)
    var t1751 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1750)
    println__T_string(t1751)
    var ok__114 Result__isize__string = Result__isize__string{
        _tag: 0,
        _v0_0: 1,
    }
    var error__115 Result__isize__string = Result__isize__string{
        _tag: 1,
        _v1_0: "error",
    }
    var t1752 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____isize____string_i_lt(ok__114, error__115)
    var t1753 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1752)
    println__T_string(t1753)
    var t1754 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__isize(first_values__112, 0, 2)
    var t1755 []int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__isize(second_values__113, 0, 2)
    var t1756 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_isize_r__i_lt(t1754, t1755)
    var t1757 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1756)
    println__T_string(t1757)
    var values__116 *hashmap_Vec_3int_string_x = _goml_m_inherent_i_HashMap_i_H_hca0cffb759e2572d7286850a43f94208_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_hfc6d57fdfd8ace95a78adbd7155c7d4b_r_____V__string(values__116, first_values__112, "vector")
    var t1758 [2]int = [2]int{1, 2}
    var t1759 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1758)
    var t1760 Option__string = _goml_m_inherent_i_HashMap_i_H_h0069152d187f39ec37f55a9b6f59774d_r_____V__string(values__116, t1759)
    var t1761 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1760, "missing")
    println__T_string(t1761)
    var default_tuple__117 Tuple2_3int_6string = _goml_m_trait__impl_i_Default_i__o_isize_c_string_q__i_default()
    var t1800 int = default_tuple__117._0
    var t1801 int = 0
    var t1802 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(t1800, t1801)
    var jp1763 bool
    if t1802 {
        var t1803 string = default_tuple__117._1
        var t1804 string = ""
        var inline3252 bool = t1803 == t1804
        jp1763 = inline3252
    } else {
        jp1763 = false
    }
    var t1764 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1763)
    println__T_string(t1764)
    var default_array__118 [3]int = _goml_m_trait__impl_i_Default_i__l_isize_x3b_3_r__i_default()
    var _eq_rhs945 [3]int = [3]int{0, 0, 0}
    var t1789 int = array_get__Array_3_3int(default_array__118, 0)
    var t1790 int = array_get__Array_3_3int(_eq_rhs945, 0)
    var t1791 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(t1789, t1790)
    var jp1766 bool
    if t1791 {
        var t1794 int = array_get__Array_3_3int(default_array__118, 1)
        var t1795 int = array_get__Array_3_3int(_eq_rhs945, 1)
        var t1796 bool
        var inline3256 bool = t1794 == t1795
        t1796 = inline3256
        if t1796 {
            var t1797 int = array_get__Array_3_3int(default_array__118, 2)
            var t1798 int = array_get__Array_3_3int(_eq_rhs945, 2)
            var inline3254 bool = t1797 == t1798
            jp1766 = inline3254
        } else {
            jp1766 = false
        }
    } else {
        jp1766 = false
    }
    var t1767 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp1766)
    println__T_string(t1767)
    var t1768 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1769 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 3,
    }
    var t1770 bool = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_isize_c_isize_q__i_lt(t1768, t1769)
    var t1771 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1770)
    var inline3297 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1771)
    _goml_runtime_core_string_println(inline3297)
    var t1772 [2]int = [2]int{1, 2}
    var t1773 [2]int = [2]int{1, 3}
    var t1774 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_isize_x3b_2_r__i_cmp(t1772, t1773)
    var t1775 string = ordering_name(t1774)
    var inline3294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1775)
    _goml_runtime_core_string_println(inline3294)
    var t1776 float64 = zero__105 / zero__105
    var tuple_nan__119 Tuple2_3int_7float64 = Tuple2_3int_7float64{
        _0: 0,
        _1: t1776,
    }
    var t1777 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_isize_c_f64_q__i_partial__cmp(tuple_nan__119, tuple_nan__119)
    var t1778 bool
    var inline3291 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t1777)
    var inline3292 bool = !inline3291
    t1778 = inline3292
    var t1779 string
    var inline3289 string = _goml_runtime_core_bool_to_string(t1778)
    t1779 = inline3289
    var inline3286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1779)
    _goml_runtime_core_string_println(inline3286)
    var tuple_values__120 *hashmap_Tuple2_3int_6string_string_x
    var inline3284 *hashmap_Tuple2_3int_6string_string_x = hashmap_new__HashMap_19Tuple2_3int_6string_6string()
    tuple_values__120 = inline3284
    var t1780 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var inline3281 string = "tuple"
    hashmap_set__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1780, inline3281)
    var t1781 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "one",
    }
    var t1782 Option__string
    var inline3279 Option__string = hashmap_get__HashMap_19Tuple2_3int_6string_6string(tuple_values__120, t1781)
    t1782 = inline3279
    var t1783 string
    var inline3275 string = "missing"
    switch t1782._tag {
    case 0:
        t1783 = inline3275
    case 1:
        var inline3276 string = t1782._v1_0
        t1783 = inline3276
    default:
        panic("non-exhaustive match")
    }
    var inline3272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1783)
    _goml_runtime_core_string_println(inline3272)
    var array_values__121 *hashmap_Array_2_3int_string_x
    var inline3270 *hashmap_Array_2_3int_string_x = hashmap_new__HashMap_12Array_2_3int_6string()
    array_values__121 = inline3270
    var t1784 [2]int = [2]int{1, 2}
    var inline3267 string = "array"
    hashmap_set__HashMap_12Array_2_3int_6string(array_values__121, t1784, inline3267)
    var t1785 [2]int = [2]int{1, 2}
    var t1786 Option__string
    var inline3265 Option__string = hashmap_get__HashMap_12Array_2_3int_6string(array_values__121, t1785)
    t1786 = inline3265
    var t1787 string
    var inline3261 string = "missing"
    switch t1786._tag {
    case 0:
        t1787 = inline3261
    case 1:
        var inline3262 string = t1786._v1_0
        t1787 = inline3262
    default:
        panic("non-exhaustive match")
    }
    var inline3258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1787)
    _goml_runtime_core_string_println(inline3258)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(self__1113 Ordering, other__1114 Ordering) bool {
    switch self__1113 {
    case Less:
        switch other__1114 {
        case Less:
            return true
        default:
            return false
        }
    case Equal:
        switch other__1114 {
        case Equal:
            return true
        default:
            return false
        }
    case Greater:
        switch other__1114 {
        case Greater:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__isize(self__0 int, other__1 int) bool {
    var commute_field4099 Ordering
    var inline3421 bool = self__0 < other__1
    var inline3423 Ordering
    if inline3421 {
        inline3423 = Less
    } else {
        var inline3425 bool = self__0 > other__1
        if inline3425 {
            inline3423 = Greater
        } else {
            inline3423 = Equal
        }
    }
    commute_field4099 = inline3423
    switch commute_field4099 {
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

func _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(self__438 int, other__439 int) bool {
    var t2192 bool = self__438 == other__439
    return t2192
}

func println__T_string(value__1 string) struct{} {
    var t2288 string
    t2288 = value__1
    _goml_runtime_core_string_println(t2288)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t2292 string = _goml_runtime_core_bool_to_string(self__401)
    return t2292
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__Ordering(self__719 Option__Ordering) bool {
    var t2295 bool
    switch self__719._tag {
    case 0:
        t2295 = false
    case 1:
        t2295 = true
    default:
        panic("non-exhaustive match")
    }
    var t2296 bool = !t2295
    return t2296
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_GenericPair____isize_i_lt(default_arg0 GenericPair__isize, default_arg1 GenericPair__isize) bool {
    var inline3816 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_hf6de147f382a0c7851836d0995f09328__i_partial__cmp(default_arg0, default_arg1)
    switch inline3816._tag {
    case 0:
        return false
    case 1:
        var inline3817 Ordering = inline3816._v1_0
        var inline3819 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3817, Less)
        return inline3819
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Phantom____NoTraits_i_lt(default_arg0 Phantom__NoTraits, default_arg1 Phantom__NoTraits) bool {
    var inline3821 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(default_arg0, default_arg1)
    switch inline3821._tag {
    case 0:
        return false
    case 1:
        var inline3822 Ordering = inline3821._v1_0
        var inline3824 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3822, Less)
        return inline3824
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_isize_r__i_lt(default_arg0 *_goml_vec_int, default_arg1 *_goml_vec_int) bool {
    var inline3826 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_isize_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3826._tag {
    case 0:
        return false
    case 1:
        var inline3827 Ordering = inline3826._v1_0
        var inline3829 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3827, Less)
        return inline3829
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____isize_i_lt(default_arg0 Option__isize, default_arg1 Option__isize) bool {
    var inline3831 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____isize_i_partial__cmp(default_arg0, default_arg1)
    switch inline3831._tag {
    case 0:
        return false
    case 1:
        var inline3832 Ordering = inline3831._v1_0
        var inline3834 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3832, Less)
        return inline3834
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Result____isize____string_i_lt(default_arg0 Result__isize__string, default_arg1 Result__isize__string) bool {
    var inline3836 Option__Ordering = _goml_m_trait__impl_i_std_p_cm_hc3f80c466e4f303ea6e82f1783f2c793__i_partial__cmp(default_arg0, default_arg1)
    switch inline3836._tag {
    case 0:
        return false
    case 1:
        var inline3837 Ordering = inline3836._v1_0
        var inline3839 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3837, Less)
        return inline3839
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__isize(self__564 *_goml_vec_int, start__565 int, end__566 int) []int {
    var t2314 []int = self__564.items[start__565:end__566]
    return t2314
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_isize_r__i_lt(default_arg0 []int, default_arg1 []int) bool {
    var inline3841 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_isize_r__i_partial__cmp(default_arg0, default_arg1)
    switch inline3841._tag {
    case 0:
        return false
    case 1:
        var inline3842 Ordering = inline3841._v1_0
        var inline3844 bool = _goml_m_trait__impl_i_PartialEq_i_Ordering_i_eq(inline3842, Less)
        return inline3844
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_hca0cffb759e2572d7286850a43f94208_r_____V__string() *hashmap_Vec_3int_string_x {
    var t2320 *hashmap_Vec_3int_string_x = hashmap_new__HashMap_8Vec_3int_6string()
    return t2320
}

func _goml_m_inherent_i_HashMap_i_H_hfc6d57fdfd8ace95a78adbd7155c7d4b_r_____V__string(self__675 *hashmap_Vec_3int_string_x, key__676 *_goml_vec_int, value__677 string) struct{} {
    hashmap_set__HashMap_8Vec_3int_6string(self__675, key__676, value__677)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h0069152d187f39ec37f55a9b6f59774d_r_____V__string(self__673 *hashmap_Vec_3int_string_x, key__674 *_goml_vec_int) Option__string {
    var t2325 Option__string = hashmap_get__HashMap_8Vec_3int_6string(self__673, key__674)
    return t2325
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__720 Option__string, fallback__721 string) string {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 string = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Default_i__o_isize_c_string_q__i_default() Tuple2_3int_6string {
    var t2332 int
    t2332 = 0
    var t2333 string
    t2333 = ""
    var t2334 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: t2332,
        _1: t2333,
    }
    return t2334
}

func _goml_m_trait__impl_i_Default_i__l_isize_x3b_3_r__i_default() [3]int {
    var t2340 int
    t2340 = 0
    var t2341 int
    t2341 = 0
    var t2342 int
    t2342 = 0
    var t2343 [3]int = [3]int{t2340, t2341, t2342}
    return t2343
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_isize_c_isize_q__i_lt(self Tuple2_3int_3int, other Tuple2_3int_3int) bool {
    var t2348 int = self._0
    var t2349 int = other._0
    var t2350 bool
    var inline3857 bool = t2348 == t2349
    t2350 = inline3857
    if t2350 {
        var t2353 int = self._1
        var t2354 int = other._1
        var t2355 bool
        var inline3853 bool = t2353 == t2354
        t2355 = inline3853
        if t2355 {
            return false
        } else {
            var t2356 int = self._1
            var t2357 int = other._1
            var inline3851 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__isize(t2356, t2357)
            return inline3851
        }
    } else {
        var t2359 int = self._0
        var t2360 int = other._0
        var inline3855 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__isize(t2359, t2360)
        return inline3855
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i__l_isize_x3b_2_r__i_cmp(self [2]int, other [2]int) Ordering {
    var t2364 int = array_get__Array_2_3int(self, 0)
    var t2365 int = array_get__Array_2_3int(other, 0)
    var _structural_ordering_0 Ordering
    var inline3864 bool = t2364 < t2365
    if inline3864 {
        _structural_ordering_0 = Less
    } else {
        var inline3865 bool = t2364 > t2365
        if inline3865 {
            _structural_ordering_0 = Greater
        } else {
            _structural_ordering_0 = Equal
        }
    }
    var t2368 bool
    switch _structural_ordering_0 {
    case Less:
        t2368 = false
    case Equal:
        t2368 = true
    case Greater:
        t2368 = false
    default:
        panic("non-exhaustive match")
    }
    if t2368 {
        var t2369 int = array_get__Array_2_3int(self, 1)
        var t2370 int = array_get__Array_2_3int(other, 1)
        var _structural_ordering_1 Ordering
        var inline3860 bool = t2369 < t2370
        if inline3860 {
            _structural_ordering_1 = Less
        } else {
            var inline3861 bool = t2369 > t2370
            if inline3861 {
                _structural_ordering_1 = Greater
            } else {
                _structural_ordering_1 = Equal
            }
        }
        var t2373 bool
        switch _structural_ordering_1 {
        case Less:
            t2373 = false
        case Equal:
            t2373 = true
        case Greater:
            t2373 = false
        default:
            panic("non-exhaustive match")
        }
        if t2373 {
            return Equal
        } else {
            return _structural_ordering_1
        }
    } else {
        return _structural_ordering_0
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i__o_isize_c_f64_q__i_partial__cmp(self Tuple2_3int_7float64, other Tuple2_3int_7float64) Option__Ordering {
    var t2376 int = self._0
    var t2377 int = other._0
    var _structural_partial_ordering_0 Option__Ordering
    var commute_field4258 Ordering
    var inline3876 bool = t2376 < t2377
    var inline3878 Ordering
    if inline3876 {
        inline3878 = Less
    } else {
        var inline3880 bool = t2376 > t2377
        if inline3880 {
            inline3878 = Greater
        } else {
            inline3878 = Equal
        }
    }
    var inline3879 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3878,
    }
    _structural_partial_ordering_0 = inline3879
    commute_field4258 = inline3878
    var t2382 bool
    switch commute_field4258 {
    case Less:
        t2382 = false
    case Equal:
        t2382 = true
    case Greater:
        t2382 = false
    default:
        panic("non-exhaustive match")
    }
    if t2382 {
        var t2383 float64 = self._1
        var t2384 float64 = other._1
        var _structural_partial_ordering_1 Option__Ordering
        var commute_field4255 Ordering
        var inline3868 bool = t2383 < t2384
        if inline3868 {
            var inline3869 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            _structural_partial_ordering_1 = inline3869
            commute_field4255 = Less
            var t2389 bool
            switch commute_field4255 {
            case Less:
                t2389 = false
            case Equal:
                t2389 = true
            case Greater:
                t2389 = false
            default:
                panic("non-exhaustive match")
            }
            if t2389 {
                var t2390 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                return t2390
            } else {
                return _structural_partial_ordering_1
            }
        } else {
            var inline3870 bool = t2383 > t2384
            if inline3870 {
                var inline3871 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Greater,
                }
                _structural_partial_ordering_1 = inline3871
                commute_field4255 = Greater
                var t2389 bool
                switch commute_field4255 {
                case Less:
                    t2389 = false
                case Equal:
                    t2389 = true
                case Greater:
                    t2389 = false
                default:
                    panic("non-exhaustive match")
                }
                if t2389 {
                    var t2390 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2390
                } else {
                    return _structural_partial_ordering_1
                }
            } else {
                var inline3872 bool = t2383 == t2384
                if inline3872 {
                    var inline3873 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    _structural_partial_ordering_1 = inline3873
                    commute_field4255 = Equal
                    var t2389 bool
                    switch commute_field4255 {
                    case Less:
                        t2389 = false
                    case Equal:
                        t2389 = true
                    case Greater:
                        t2389 = false
                    default:
                        panic("non-exhaustive match")
                    }
                    if t2389 {
                        var t2390 Option__Ordering = Option__Ordering{
                            _tag: 1,
                            _v1_0: Equal,
                        }
                        return t2390
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

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__718 Option__Ordering) bool {
    switch self__718._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cm_hf6de147f382a0c7851836d0995f09328__i_partial__cmp(self__67 GenericPair__isize, other__68 GenericPair__isize) Option__Ordering {
    var t2456 int = self__67.first
    var t2457 int = other__68.first
    var commute_field4264 Ordering
    var inline3905 bool = t2456 < t2457
    var inline3907 Ordering
    if inline3905 {
        inline3907 = Less
    } else {
        var inline3909 bool = t2456 > t2457
        if inline3909 {
            inline3907 = Greater
        } else {
            inline3907 = Equal
        }
    }
    commute_field4264 = inline3907
    switch commute_field4264 {
    case Equal:
        var t2462 int = self__67.second
        var t2463 int = other__68.second
        var commute_field4261 Ordering
        var inline3899 bool = t2462 < t2463
        var inline3901 Ordering
        if inline3899 {
            inline3901 = Less
        } else {
            var inline3903 bool = t2462 > t2463
            if inline3903 {
                inline3901 = Greater
            } else {
                inline3901 = Equal
            }
        }
        commute_field4261 = inline3901
        switch commute_field4261 {
        case Equal:
            var t2468 *_goml_vec_int = self__67.nested
            var t2469 *_goml_vec_int = other__68.nested
            var mtmp894 Option__Ordering = _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_isize_r__i_partial__cmp(t2468, t2469)
            switch mtmp894._tag {
            case 0:
                return Option__Ordering{
                    _tag: 0,
                }
            case 1:
                var x895 Ordering = mtmp894._v1_0
                switch x895 {
                case Equal:
                    var t2474 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2474
                default:
                    var t2475 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: x895,
                    }
                    return t2475
                }
            default:
                panic("non-exhaustive match")
            }
        default:
            var t2476 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: commute_field4261,
            }
            return t2476
        }
    default:
        var t2477 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: commute_field4264,
        }
        return t2477
    }
}

func _goml_m_trait__impl_i_std_p_cm_h70a1f5151189c8228387cc52486880c2__i_partial__cmp(self__83 Phantom__NoTraits, other__84 Phantom__NoTraits) Option__Ordering {
    var jp2481 int
    switch self__83 {
    case First:
        jp2481 = 0
    case Second:
        jp2481 = 1
    default:
        panic("non-exhaustive match")
    }
    var jp2483 int
    switch other__84 {
    case First:
        jp2483 = 0
    case Second:
        jp2483 = 1
    default:
        panic("non-exhaustive match")
    }
    var t2486 bool = jp2481 < jp2483
    if t2486 {
        var t2487 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        return t2487
    } else {
        var t2490 bool = jp2481 > jp2483
        if t2490 {
            var t2491 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2491
        } else {
            switch other__84 {
            case First:
                switch self__83 {
                case First:
                    var t2496 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2496
                default:
                    var t2497 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2497
                }
            case Second:
                switch self__83 {
                case Second:
                    var t2500 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2500
                default:
                    var t2501 Option__Ordering = Option__Ordering{
                        _tag: 1,
                        _v1_0: Equal,
                    }
                    return t2501
                }
            default:
                panic("non-exhaustive match")
            }
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Vec_l_isize_r__i_partial__cmp(self__76 *_goml_vec_int, other__77 *_goml_vec_int) Option__Ordering {
    var t2519 int
    var inline3933 int = vec_len__Vec_3int(self__76)
    t2519 = inline3933
    var t2520 int
    var inline3931 int = vec_len__Vec_3int(other__77)
    t2520 = inline3931
    var t2521 bool = t2519 < t2520
    var jp2505 int
    if t2521 {
        var inline3911 int = vec_len__Vec_3int(self__76)
        jp2505 = inline3911
    } else {
        var inline3913 int = vec_len__Vec_3int(other__77)
        jp2505 = inline3913
    }
    var index__79 int = 0
    Loop_loop2510:
    for {
        var t2511 bool = index__79 < jp2505
        if t2511 {
            var t2512 int = vec_get__Vec_3int(self__76, index__79)
            var t2513 int = vec_get__Vec_3int(other__77, index__79)
            var commute_field4267 Ordering
            var inline3915 bool = t2512 < t2513
            var inline3917 Ordering
            if inline3915 {
                inline3917 = Less
            } else {
                var inline3919 bool = t2512 > t2513
                if inline3919 {
                    inline3917 = Greater
                } else {
                    inline3917 = Equal
                }
            }
            commute_field4267 = inline3917
            switch commute_field4267 {
            case Equal:
                var compound_old10 int = index__79
                var compound_value11 int = 1
                var t2516 int = compound_old10 + compound_value11
                index__79 = t2516
                continue
            default:
                var t2518 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field4267,
                }
                return t2518
            }
        } else {
            break Loop_loop2510
        }
    }
    var t2507 int
    var inline3929 int = vec_len__Vec_3int(self__76)
    t2507 = inline3929
    var t2508 int
    var inline3927 int = vec_len__Vec_3int(other__77)
    t2508 = inline3927
    var inline3921 bool = t2507 < t2508
    var inline3923 Ordering
    if inline3921 {
        inline3923 = Less
    } else {
        var inline3925 bool = t2507 > t2508
        if inline3925 {
            inline3923 = Greater
        } else {
            inline3923 = Equal
        }
    }
    var inline3924 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3923,
    }
    return inline3924
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Option____isize_i_partial__cmp(self__96 Option__isize, other__97 Option__isize) Option__Ordering {
    switch other__97._tag {
    case 0:
        switch self__96._tag {
        case 0:
            var t2530 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Equal,
            }
            return t2530
        case 1:
            var t2531 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2531
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x33 int = other__97._v1_0
        switch self__96._tag {
        case 0:
            var t2534 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            return t2534
        case 1:
            var x35 int = self__96._v1_0
            var inline3935 bool = x35 < x33
            var inline3937 Ordering
            if inline3935 {
                inline3937 = Less
            } else {
                var inline3939 bool = x35 > x33
                if inline3939 {
                    inline3937 = Greater
                } else {
                    inline3937 = Equal
                }
            }
            var inline3938 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3937,
            }
            return inline3938
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cm_hc3f80c466e4f303ea6e82f1783f2c793__i_partial__cmp(self__104 Result__isize__string, other__105 Result__isize__string) Option__Ordering {
    switch other__105._tag {
    case 0:
        var x45 int = other__105._v0_0
        switch self__104._tag {
        case 0:
            var x47 int = self__104._v0_0
            var inline3941 bool = x47 < x45
            var inline3943 Ordering
            if inline3941 {
                inline3943 = Less
            } else {
                var inline3945 bool = x47 > x45
                if inline3945 {
                    inline3943 = Greater
                } else {
                    inline3943 = Equal
                }
            }
            var inline3944 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3943,
            }
            return inline3944
        case 1:
            var t2543 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            return t2543
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x46 string = other__105._v1_0
        switch self__104._tag {
        case 0:
            var t2546 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Less,
            }
            return t2546
        case 1:
            var x50 string = self__104._v1_0
            var inline3947 bool = x50 < x46
            var inline3949 Ordering
            if inline3947 {
                inline3949 = Less
            } else {
                var inline3951 bool = x50 > x46
                if inline3951 {
                    inline3949 = Greater
                } else {
                    inline3949 = Equal
                }
            }
            var inline3950 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: inline3949,
            }
            return inline3950
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_PartialOrd_i_Slice_l_isize_r__i_partial__cmp(self__86 []int, other__87 []int) Option__Ordering {
    var t2565 int
    var inline3975 int = len(self__86)
    t2565 = inline3975
    var t2566 int
    var inline3973 int = len(other__87)
    t2566 = inline3973
    var t2567 bool = t2565 < t2566
    var jp2551 int
    if t2567 {
        var inline3953 int = len(self__86)
        jp2551 = inline3953
    } else {
        var inline3955 int = len(other__87)
        jp2551 = inline3955
    }
    var index__89 int = 0
    Loop_loop2556:
    for {
        var t2557 bool = index__89 < jp2551
        if t2557 {
            var t2558 int = self__86[index__89]
            var t2559 int = other__87[index__89]
            var commute_field4270 Ordering
            var inline3957 bool = t2558 < t2559
            var inline3959 Ordering
            if inline3957 {
                inline3959 = Less
            } else {
                var inline3961 bool = t2558 > t2559
                if inline3961 {
                    inline3959 = Greater
                } else {
                    inline3959 = Equal
                }
            }
            commute_field4270 = inline3959
            switch commute_field4270 {
            case Equal:
                var compound_old21 int = index__89
                var compound_value22 int = 1
                var t2562 int = compound_old21 + compound_value22
                index__89 = t2562
                continue
            default:
                var t2564 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: commute_field4270,
                }
                return t2564
            }
        } else {
            break Loop_loop2556
        }
    }
    var t2553 int
    var inline3971 int = len(self__86)
    t2553 = inline3971
    var t2554 int
    var inline3969 int = len(other__87)
    t2554 = inline3969
    var inline3963 bool = t2553 < t2554
    var inline3965 Ordering
    if inline3963 {
        inline3965 = Less
    } else {
        var inline3967 bool = t2553 > t2554
        if inline3967 {
            inline3965 = Greater
        } else {
            inline3965 = Equal
        }
    }
    var inline3966 Option__Ordering = Option__Ordering{
        _tag: 1,
        _v1_0: inline3965,
    }
    return inline3966
}

func _goml_m_trait__impl_i_PartialEq_i_Vec_l_isize_r__i_eq(self__476 *_goml_vec_int, other__477 *_goml_vec_int) bool {
    var t2619 int
    var inline3993 int = vec_len__Vec_3int(self__476)
    t2619 = inline3993
    var t2620 int
    var inline3991 int = vec_len__Vec_3int(other__477)
    t2620 = inline3991
    var t2621 bool = t2619 != t2620
    if t2621 {
        return false
    } else {
        var index__478 int = 0
        Loop_loop2623:
        for {
            var t2624 int
            var inline3989 int = vec_len__Vec_3int(self__476)
            t2624 = inline3989
            var t2625 bool = index__478 < t2624
            if t2625 {
                var t2627 int = vec_get__Vec_3int(self__476, index__478)
                var t2628 int = vec_get__Vec_3int(other__477, index__478)
                var t2629 bool
                var inline3987 bool = t2627 == t2628
                t2629 = inline3987
                if t2629 {
                    var compound_old538 int = index__478
                    var compound_value539 int = 1
                    var t2630 int = compound_old538 + compound_value539
                    index__478 = t2630
                    continue
                } else {
                    return false
                }
            } else {
                break Loop_loop2623
            }
        }
        return true
    }
}

func _goml_m_trait__impl_i_Hash_i_Vec_l_isize_r__i_hash(self__479 *_goml_vec_int) uint64 {
    var value__480 uint64 = 14695981039346656037
    var index__481 int = 0
    Loop_loop2635:
    for {
        var t2636 int
        var inline3997 int = vec_len__Vec_3int(self__479)
        t2636 = inline3997
        var t2637 bool = index__481 < t2636
        if t2637 {
            var t2638 uint64 = value__480 * 1099511628211
            var t2639 int = vec_get__Vec_3int(self__479, index__481)
            var t2640 uint64
            var inline3995 uint64 = _goml_runtime_core_int_hash(t2639)
            t2640 = inline3995
            var t2641 uint64 = t2638 + t2640
            value__480 = t2641
            var compound_old543 int = index__481
            var compound_value544 int = 1
            var t2642 int = compound_old543 + compound_value544
            index__481 = t2642
            continue
        } else {
            break Loop_loop2635
        }
    }
    return value__480
}

func _goml_m_trait__impl_i_PartialEq_i__o_isize_c_string_q__i_eq(self Tuple2_3int_6string, other Tuple2_3int_6string) bool {
    var t2648 int = self._0
    var t2649 int = other._0
    var t2650 bool
    var inline4001 bool = t2648 == t2649
    t2650 = inline4001
    if t2650 {
        var t2653 string = self._1
        var t2654 string = other._1
        var t2655 bool
        var inline3999 bool = t2653 == t2654
        t2655 = inline3999
        if t2655 {
            return true
        } else {
            return false
        }
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i__o_isize_c_string_q__i_hash(self Tuple2_3int_6string) uint64 {
    var _structural_hash_0 uint64 = 14695981039346656037
    var t2658 uint64 = _structural_hash_0 * 1099511628211
    var t2659 int = self._0
    var t2660 uint64
    var inline4005 uint64 = _goml_runtime_core_int_hash(t2659)
    t2660 = inline4005
    var _structural_hash_1 uint64 = t2658 + t2660
    var t2661 uint64 = _structural_hash_1 * 1099511628211
    var t2662 string = self._1
    var t2663 uint64
    var inline4003 uint64 = _goml_runtime_core_string_hash(t2662)
    t2663 = inline4003
    var _structural_hash_2 uint64 = t2661 + t2663
    return _structural_hash_2
}

func _goml_m_trait__impl_i_PartialEq_i__l_isize_x3b_2_r__i_eq(self [2]int, other [2]int) bool {
    var t2668 int = array_get__Array_2_3int(self, 0)
    var t2669 int = array_get__Array_2_3int(other, 0)
    var t2670 bool
    var inline4009 bool = t2668 == t2669
    t2670 = inline4009
    if t2670 {
        var t2673 int = array_get__Array_2_3int(self, 1)
        var t2674 int = array_get__Array_2_3int(other, 1)
        var t2675 bool
        var inline4007 bool = t2673 == t2674
        t2675 = inline4007
        if t2675 {
            return true
        } else {
            return false
        }
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i__l_isize_x3b_2_r__i_hash(self [2]int) uint64 {
    var _structural_hash_0 uint64 = 14695981039346656037
    var t2678 uint64 = _structural_hash_0 * 1099511628211
    var t2679 int = array_get__Array_2_3int(self, 0)
    var t2680 uint64
    var inline4013 uint64 = _goml_runtime_core_int_hash(t2679)
    t2680 = inline4013
    var _structural_hash_1 uint64 = t2678 + t2680
    var t2681 uint64 = _structural_hash_1 * 1099511628211
    var t2682 int = array_get__Array_2_3int(self, 1)
    var t2683 uint64
    var inline4011 uint64 = _goml_runtime_core_int_hash(t2682)
    t2683 = inline4011
    var _structural_hash_2 uint64 = t2681 + t2683
    return _structural_hash_2
}

func main() {
    main0()
}
