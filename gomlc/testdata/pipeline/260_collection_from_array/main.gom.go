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

func _goml_m_trait__impl_i_PartialEq_i_LoggedKey_i_eq(self__0 LoggedKey, other__1 LoggedKey) bool {
    var t821 *ref_string_x = self__0.log
    var t822 *ref_string_x = self__0.log
    var t823 string
    var inline1024 string = ref_get__Ref_6string(t822)
    t823 = inline1024
    var t824 string = t823 + "E"
    ref_set__Ref_6string(t821, t824)
    var t825 int = self__0.id
    var t826 int = other__1.id
    var t827 bool = t825 == t826
    return t827
}

func _goml_m_trait__impl_i_Hash_i_LoggedKey_i_hash(self__2 LoggedKey) uint64 {
    var t830 *ref_string_x = self__2.log
    var t831 *ref_string_x = self__2.log
    var t832 string
    var inline1028 string = ref_get__Ref_6string(t831)
    t832 = inline1028
    var t833 string = t832 + "H"
    ref_set__Ref_6string(t830, t833)
    var t834 int = self__2.id
    var t835 uint64 = uint64(int(t834))
    return t835
}

func logged_key(log__3 *ref_string_x, label__4 string, id__5 int) LoggedKey {
    var t838 string
    var inline1032 string = ref_get__Ref_6string(log__3)
    t838 = inline1032
    var t839 string = t838 + label__4
    ref_set__Ref_6string(log__3, t839)
    var t840 LoggedKey = LoggedKey{
        id: id__5,
        log: log__3,
    }
    return t840
}

func logged_value(log__6 *ref_string_x, label__7 string, value__8 int) int {
    var t843 string
    var inline1036 string = ref_get__Ref_6string(log__6)
    t843 = inline1036
    var t844 string = t843 + label__7
    ref_set__Ref_6string(log__6, t844)
    return value__8
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
    var t846 [3]int = [3]int{1, 2, 3}
    var values__10 *_goml_vec_int = make_vec__9(t846)
    var t847 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(values__10)
    var t848 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t847)
    var t849 string = "" + t848
    var t850 string = t849 + ":"
    var t851 int = vec_get__Vec_3int(values__10, 0)
    var t852 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t851)
    var t853 string = t850 + t852
    var t854 string = t853 + ":"
    var t855 int = vec_get__Vec_3int(values__10, 2)
    var t856 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t855)
    var t857 string = t854 + t856
    println__T_string(t857)
    var empty__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t858 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(empty__11)
    println__T_isize(t858)
    var inferred_empty__12 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t859 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inferred_empty__12)
    println__T_isize(t859)
    var t860 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(1)
    var t861 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(2)
    var source__13 [2]*ref_int_x = [2]*ref_int_x{t860, t861}
    var copied__14 *_goml_vec_Ref_3int = func(values [2]*ref_int_x) *_goml_vec_Ref_3int {
        var storage struct {
            vector _goml_vec_Ref_3int
            values [2]*ref_int_x
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(source__13)
    var t862 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t862, 5)
    var place_root804 [2]*ref_int_x = source__13
    var index805 int = 0
    array_get__Array_2_8Ref_3int(place_root804, index805)
    var value807 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(9)
    var t863 [2]*ref_int_x = array_set__Array_2_8Ref_3int(place_root804, index805, value807)
    source__13 = t863
    var t865 *ref_int_x = vec_get__Vec_8Ref_3int(copied__14, 0)
    var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t865)
    var t867 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t866)
    var t868 string = "" + t867
    var t869 string = t868 + ":"
    var t870 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    var t871 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t870)
    var t872 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t871)
    var t873 string = t869 + t872
    println__T_string(t873)
    var log__15 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var t874 LoggedKey = logged_key(log__15, "A", 1)
    var t875 int = logged_value(log__15, "a", 10)
    var t876 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t874,
        _1: t875,
    }
    var t877 LoggedKey
    var inline1083 string = "B"
    var inline1084 int = 1
    var inline1085 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline1086 string = inline1085 + inline1083
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline1086)
    var inline1088 LoggedKey = LoggedKey{
        id: inline1084,
        log: log__15,
    }
    t877 = inline1088
    var t878 int
    var inline1077 string = "b"
    var inline1078 int = 20
    var inline1079 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline1080 string = inline1079 + inline1077
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline1080)
    t878 = inline1078
    var t879 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t877,
        _1: t878,
    }
    var table__16 *hashmap_LoggedKey_int_x = &hashmap_LoggedKey_int_x{
        buckets: make(map[uint64][]hashmap_LoggedKey_int_x_entry, 2),
        hashes: make([]uint64, 0, 2),
        len: 0,
    }
    hashmap_set__HashMap_9LoggedKey_3int(table__16, t876._0, t876._1)
    hashmap_set__HashMap_9LoggedKey_3int(table__16, t879._0, t879._1)
    var t881 string
    var inline1075 string = ref_get__Ref_6string(log__15)
    t881 = inline1075
    var inline1072 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t881)
    _goml_runtime_core_string_println(inline1072)
    var t882 LoggedKey = LoggedKey{
        id: 1,
        log: log__15,
    }
    var mtmp811 Option__isize
    var inline1070 Option__isize = hashmap_get__HashMap_9LoggedKey_3int(table__16, t882)
    mtmp811 = inline1070
    var jp884 int
    switch mtmp811._tag {
    case 0:
        jp884 = 0
    case 1:
        var x812 int = mtmp811._v1_0
        jp884 = x812
    default:
        panic("non-exhaustive match")
    }
    var inline1067 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp884)
    _goml_runtime_core_string_println(inline1067)
    var make_map__18 func([2]Tuple2_6string_3int) *hashmap_string_int_x = func(values [2]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }
    var t885 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "a",
        _1: 1,
    }
    var t886 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "b",
        _1: 2,
    }
    var t887 [2]Tuple2_6string_3int = [2]Tuple2_6string_3int{t885, t886}
    var words__19 *hashmap_string_int_x = make_map__18(t887)
    var t888 int
    var inline1065 int = hashmap_len__HashMap_6string_3int(words__19)
    t888 = inline1065
    var inline1062 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t888)
    _goml_runtime_core_string_println(inline1062)
    var no_words__20 *hashmap_string_int_x
    var inline1060 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    no_words__20 = inline1060
    var t889 int
    var inline1058 int = hashmap_len__HashMap_6string_3int(no_words__20)
    t889 = inline1058
    var inline1055 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t889)
    _goml_runtime_core_string_println(inline1055)
    var inferred_no_words__21 *hashmap_string_int_x
    var inline1053 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    inferred_no_words__21 = inline1053
    var t890 int
    var inline1051 int = hashmap_len__HashMap_6string_3int(inferred_no_words__21)
    t890 = inline1051
    var inline1048 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t890)
    _goml_runtime_core_string_println(inline1048)
    var t891 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "a",
    }
    var t892 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 2,
        _1: "b",
    }
    var t893 [2]Tuple2_3int_6string = [2]Tuple2_3int_6string{t891, t892}
    var pairs__22 *_goml_vec_Tuple2_3int_6string = func(values [2]Tuple2_3int_6string) *_goml_vec_Tuple2_3int_6string {
        var storage struct {
            vector _goml_vec_Tuple2_3int_6string
            values [2]Tuple2_3int_6string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t893)
    var t894 [2]int = [2]int{1, 2}
    var t895 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t894)
    var t896 [2]int = [2]int{3, 4}
    var t897 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t896)
    var t898 [2]*_goml_vec_int = [2]*_goml_vec_int{t895, t897}
    var nested__23 *_goml_vec_Vec_3int = func(values [2]*_goml_vec_int) *_goml_vec_Vec_3int {
        var storage struct {
            vector _goml_vec_Vec_3int
            values [2]*_goml_vec_int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t898)
    var t899 [2]int = [2]int{5, 6}
    var t900 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t899)
    var t901 Tuple2_6string_8Vec_3int = Tuple2_6string_8Vec_3int{
        _0: "values",
        _1: t900,
    }
    var nested_map__24 *hashmap_string_Vec_3int_x = &hashmap_string_Vec_3int_x{
        indices: make(map[string]int, 1),
        entries: make([]hashmap_string_Vec_3int_x_entry, 0, 1),
        len: 0,
    }
    hashmap_set__HashMap_6string_8Vec_3int(nested_map__24, t901._0, t901._1)
    var mtmp817 _goml_m_Option____Vec_l_isize_r_
    var inline1045 string = "values"
    var inline1046 _goml_m_Option____Vec_l_isize_r_ = hashmap_get__HashMap_6string_8Vec_3int(nested_map__24, inline1045)
    mtmp817 = inline1046
    var jp904 int
    switch mtmp817._tag {
    case 0:
        jp904 = 0
    case 1:
        var x818 *_goml_vec_int = mtmp817._v1_0
        var t917 int = vec_get__Vec_3int(x818, 0)
        jp904 = t917
    default:
        panic("non-exhaustive match")
    }
    var t905 Tuple2_3int_6string = vec_get__Vec_19Tuple2_3int_6string(pairs__22, 1)
    var t906 string = t905._1
    var t907 string = "" + t906
    var t908 string = t907 + ":"
    var t909 *_goml_vec_int = vec_get__Vec_8Vec_3int(nested__23, 1)
    var t910 int = vec_get__Vec_3int(t909, 0)
    var t911 string
    var inline1043 string = __goml_builtin_int_to_string(t910)
    t911 = inline1043
    var t912 string = t908 + t911
    var t913 string = t912 + ":"
    var t914 string
    var inline1041 string = __goml_builtin_int_to_string(jp904)
    t914 = inline1041
    var t915 string = t913 + t914
    var inline1038 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t915)
    _goml_runtime_core_string_println(inline1038)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__685 *ref_string_x) string {
    var t920 string = ref_get__Ref_6string(self__685)
    return t920
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__686 *ref_string_x, value__687 string) struct{} {
    ref_set__Ref_6string(self__686, value__687)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t924 string
    t924 = value__1
    _goml_runtime_core_string_println(t924)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__526 *_goml_vec_int) int {
    var t928 int = vec_len__Vec_3int(self__526)
    return t928
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1091 int64 = int64(int(self__285))
    var inline1092 string = signed_decimal_string(inline1091)
    return inline1092
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t934 *_goml_vec_int = vec_new__Vec_3int()
    return t934
}

func println__T_isize(value__1 int) struct{} {
    var t936 string
    var inline1094 string = __goml_builtin_int_to_string(value__1)
    t936 = inline1094
    _goml_runtime_core_string_println(t936)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t940 *ref_int_x = ref__Ref_3int(value__684)
    return t940
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t945 int = ref_get__Ref_3int(self__685)
    return t945
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__684 string) *ref_string_x {
    var t948 *ref_string_x = ref__Ref_6string(value__684)
    return t948
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t965 int64 = int64(int(value__222))
    var inline1096 bool = t965 < 0
    if inline1096 {
        var inline1097 uint64 = uint64(int64(t965))
        var inline1098 uint64 = 0 - inline1097
        var inline1099 string = decimal_string(inline1098)
        var inline1100 string = "-" + inline1099
        return inline1100
    } else {
        var inline1101 uint64 = uint64(int64(t965))
        var inline1102 string = decimal_string(inline1101)
        return inline1102
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1104 int64 = int64(int(self__404))
    var inline1105 string = signed_decimal_string(inline1104)
    return inline1105
}

func signed_decimal_string(value__214 int64) string {
    var t974 bool = value__214 < 0
    if t974 {
        var t975 uint64 = uint64(int64(value__214))
        var t976 uint64 = 0 - t975
        var t977 string = decimal_string(t976)
        var t978 string = "-" + t977
        return t978
    } else {
        var t979 uint64 = uint64(int64(value__214))
        var t980 string = decimal_string(t979)
        return t980
    }
}

func decimal_string(value__208 uint64) string {
    var t1003 bool = value__208 == 0
    if t1003 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop996:
        for {
            var t997 bool = remaining__210 > 0
            if t997 {
                var t998_rhs uint64 = 10
                var t998 uint64 = remaining__210 % t998_rhs
                var t999 uint8 = uint8(uint64(t998))
                var t1000 uint8 = t999 + 48
                vec_push__Vec_5uint8(reversed__209, t1000)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1001 uint64 = compound_old353 / compound_value354
                remaining__210 = t1001
                continue
            } else {
                break Loop_loop996
            }
        }
        var t985 int
        var inline1115 int = vec_len__Vec_5uint8(reversed__209)
        t985 = inline1115
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t985)
        var offset__212 int = 0
        Loop_loop987:
        for {
            var t988 int
            var inline1113 int = vec_len__Vec_5uint8(reversed__209)
            t988 = inline1113
            var t989 bool = offset__212 < t988
            if t989 {
                var t990 int
                var inline1111 int = vec_len__Vec_5uint8(reversed__209)
                t990 = inline1111
                var t991 int = t990 - offset__212
                var t992 int = t991 - 1
                var t993 uint8 = vec_get__Vec_5uint8(reversed__209, t992)
                vec_push__Vec_5uint8(bytes__211, t993)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t994 int = compound_old358 + compound_value359
                offset__212 = t994
                continue
            } else {
                break Loop_loop987
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
