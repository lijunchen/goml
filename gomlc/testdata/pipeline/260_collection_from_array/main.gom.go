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

func hashmap_new__HashMap_9LoggedKey_3int() *hashmap_LoggedKey_int_x {
    return &hashmap_LoggedKey_int_x{
        buckets: make(map[uint64][]hashmap_LoggedKey_int_x_entry),
        len: 0,
        hashes: nil,
    }
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

func hashmap_new__HashMap_6string_8Vec_3int() *hashmap_string_Vec_3int_x {
    return &hashmap_string_Vec_3int_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
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
    var inline1022 string = ref_get__Ref_6string(t822)
    t823 = inline1022
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
    var inline1026 string = ref_get__Ref_6string(t831)
    t832 = inline1026
    var t833 string = t832 + "H"
    ref_set__Ref_6string(t830, t833)
    var t834 int = self__2.id
    var t835 uint64 = uint64(int(t834))
    return t835
}

func logged_key(log__3 *ref_string_x, label__4 string, id__5 int) LoggedKey {
    var t838 string
    var inline1030 string = ref_get__Ref_6string(log__3)
    t838 = inline1030
    var t839 string = t838 + label__4
    ref_set__Ref_6string(log__3, t839)
    var t840 LoggedKey = LoggedKey{
        id: id__5,
        log: log__3,
    }
    return t840
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
    var t858 [0]int = [0]int{}
    var empty__11 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t858)
    var t859 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(empty__11)
    println__T_isize(t859)
    var t860 [0]int = [0]int{}
    var inferred_empty__12 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t860)
    var t861 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inferred_empty__12)
    println__T_isize(t861)
    var t862 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(1)
    var t863 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(2)
    var source__13 [2]*ref_int_x = [2]*ref_int_x{t862, t863}
    var copied__14 *_goml_vec_Ref_3int = func(values [2]*ref_int_x) *_goml_vec_Ref_3int {
        var storage struct {
            vector _goml_vec_Ref_3int
            values [2]*ref_int_x
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(source__13)
    var t864 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t864, 5)
    var place_root804 [2]*ref_int_x = source__13
    var index805 int = 0
    array_get__Array_2_8Ref_3int(place_root804, index805)
    var value807 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(9)
    var t865 [2]*ref_int_x = array_set__Array_2_8Ref_3int(place_root804, index805, value807)
    source__13 = t865
    var t867 *ref_int_x = vec_get__Vec_8Ref_3int(copied__14, 0)
    var t868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t867)
    var t869 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t868)
    var t870 string = "" + t869
    var t871 string = t870 + ":"
    var t872 *ref_int_x = array_get__Array_2_8Ref_3int(source__13, 0)
    var t873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t872)
    var t874 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t873)
    var t875 string = t871 + t874
    println__T_string(t875)
    var log__15 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var t876 LoggedKey = logged_key(log__15, "A", 1)
    var t877 int
    var inline1084 string = "a"
    var inline1085 int = 10
    var inline1086 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline1087 string = inline1086 + inline1084
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline1087)
    t877 = inline1085
    var t878 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t876,
        _1: t877,
    }
    var t879 LoggedKey
    var inline1077 string = "B"
    var inline1078 int = 1
    var inline1079 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline1080 string = inline1079 + inline1077
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline1080)
    var inline1082 LoggedKey = LoggedKey{
        id: inline1078,
        log: log__15,
    }
    t879 = inline1082
    var t880 int
    var inline1071 string = "b"
    var inline1072 int = 20
    var inline1073 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline1074 string = inline1073 + inline1071
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline1074)
    t880 = inline1072
    var t881 Tuple2_9LoggedKey_3int = Tuple2_9LoggedKey_3int{
        _0: t879,
        _1: t880,
    }
    var t882 [2]Tuple2_9LoggedKey_3int = [2]Tuple2_9LoggedKey_3int{t878, t881}
    var table__16 *hashmap_LoggedKey_int_x = func(values [2]Tuple2_9LoggedKey_3int) *hashmap_LoggedKey_int_x {
        var result *hashmap_LoggedKey_int_x = hashmap_new__HashMap_9LoggedKey_3int()
        for _, entry := range values {
            hashmap_set__HashMap_9LoggedKey_3int(result, entry._0, entry._1)
        }
        return result
    }(t882)
    var t883 string
    var inline1069 string = ref_get__Ref_6string(log__15)
    t883 = inline1069
    var inline1066 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t883)
    _goml_runtime_core_string_println(inline1066)
    var t884 LoggedKey = LoggedKey{
        id: 1,
        log: log__15,
    }
    var mtmp811 Option__isize
    var inline1064 Option__isize = hashmap_get__HashMap_9LoggedKey_3int(table__16, t884)
    mtmp811 = inline1064
    var jp886 int
    switch mtmp811._tag {
    case 0:
        jp886 = 0
    case 1:
        var x812 int = mtmp811._v1_0
        jp886 = x812
    default:
        panic("non-exhaustive match")
    }
    var inline1061 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp886)
    _goml_runtime_core_string_println(inline1061)
    var make_map__18 func([2]Tuple2_6string_3int) *hashmap_string_int_x = func(values [2]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }
    var t887 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "a",
        _1: 1,
    }
    var t888 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "b",
        _1: 2,
    }
    var t889 [2]Tuple2_6string_3int = [2]Tuple2_6string_3int{t887, t888}
    var words__19 *hashmap_string_int_x = make_map__18(t889)
    var t890 int
    var inline1059 int = hashmap_len__HashMap_6string_3int(words__19)
    t890 = inline1059
    var inline1056 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t890)
    _goml_runtime_core_string_println(inline1056)
    var t891 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var no_words__20 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t891)
    var t892 int
    var inline1054 int = hashmap_len__HashMap_6string_3int(no_words__20)
    t892 = inline1054
    var inline1051 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t892)
    _goml_runtime_core_string_println(inline1051)
    var t893 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var inferred_no_words__21 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t893)
    var t894 int
    var inline1049 int = hashmap_len__HashMap_6string_3int(inferred_no_words__21)
    t894 = inline1049
    var inline1046 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t894)
    _goml_runtime_core_string_println(inline1046)
    var t895 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 1,
        _1: "a",
    }
    var t896 Tuple2_3int_6string = Tuple2_3int_6string{
        _0: 2,
        _1: "b",
    }
    var t897 [2]Tuple2_3int_6string = [2]Tuple2_3int_6string{t895, t896}
    var pairs__22 *_goml_vec_Tuple2_3int_6string = func(values [2]Tuple2_3int_6string) *_goml_vec_Tuple2_3int_6string {
        var storage struct {
            vector _goml_vec_Tuple2_3int_6string
            values [2]Tuple2_3int_6string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t897)
    var t898 [2]int = [2]int{1, 2}
    var t899 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t898)
    var t900 [2]int = [2]int{3, 4}
    var t901 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t900)
    var t902 [2]*_goml_vec_int = [2]*_goml_vec_int{t899, t901}
    var nested__23 *_goml_vec_Vec_3int = func(values [2]*_goml_vec_int) *_goml_vec_Vec_3int {
        var storage struct {
            vector _goml_vec_Vec_3int
            values [2]*_goml_vec_int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t902)
    var t903 [2]int = [2]int{5, 6}
    var t904 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t903)
    var t905 Tuple2_6string_8Vec_3int = Tuple2_6string_8Vec_3int{
        _0: "values",
        _1: t904,
    }
    var t906 [1]Tuple2_6string_8Vec_3int = [1]Tuple2_6string_8Vec_3int{t905}
    var nested_map__24 *hashmap_string_Vec_3int_x = func(values [1]Tuple2_6string_8Vec_3int) *hashmap_string_Vec_3int_x {
        var result *hashmap_string_Vec_3int_x = hashmap_new__HashMap_6string_8Vec_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_8Vec_3int(result, entry._0, entry._1)
        }
        return result
    }(t906)
    var mtmp817 _goml_m_Option____Vec_l_isize_r_
    var inline1043 string = "values"
    var inline1044 _goml_m_Option____Vec_l_isize_r_ = hashmap_get__HashMap_6string_8Vec_3int(nested_map__24, inline1043)
    mtmp817 = inline1044
    var jp908 int
    switch mtmp817._tag {
    case 0:
        jp908 = 0
    case 1:
        var x818 *_goml_vec_int = mtmp817._v1_0
        var t921 int = vec_get__Vec_3int(x818, 0)
        jp908 = t921
    default:
        panic("non-exhaustive match")
    }
    var t909 Tuple2_3int_6string = vec_get__Vec_19Tuple2_3int_6string(pairs__22, 1)
    var t910 string = t909._1
    var t911 string = "" + t910
    var t912 string = t911 + ":"
    var t913 *_goml_vec_int = vec_get__Vec_8Vec_3int(nested__23, 1)
    var t914 int = vec_get__Vec_3int(t913, 0)
    var t915 string
    var inline1041 string = __goml_builtin_int_to_string(t914)
    t915 = inline1041
    var t916 string = t912 + t915
    var t917 string = t916 + ":"
    var t918 string
    var inline1039 string = __goml_builtin_int_to_string(jp908)
    t918 = inline1039
    var t919 string = t917 + t918
    var inline1036 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t919)
    _goml_runtime_core_string_println(inline1036)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__685 *ref_string_x) string {
    var t924 string = ref_get__Ref_6string(self__685)
    return t924
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__686 *ref_string_x, value__687 string) struct{} {
    ref_set__Ref_6string(self__686, value__687)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t928 string
    t928 = value__1
    _goml_runtime_core_string_println(t928)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__526 *_goml_vec_int) int {
    var t932 int = vec_len__Vec_3int(self__526)
    return t932
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1091 int64 = int64(int(self__285))
    var inline1092 string = signed_decimal_string(inline1091)
    return inline1092
}

func println__T_isize(value__1 int) struct{} {
    var t937 string
    var inline1094 string = __goml_builtin_int_to_string(value__1)
    t937 = inline1094
    _goml_runtime_core_string_println(t937)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t941 *ref_int_x = ref__Ref_3int(value__684)
    return t941
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t946 int = ref_get__Ref_3int(self__685)
    return t946
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__684 string) *ref_string_x {
    var t949 *ref_string_x = ref__Ref_6string(value__684)
    return t949
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t963 int64 = int64(int(value__222))
    var inline1096 bool = t963 < 0
    if inline1096 {
        var inline1097 uint64 = uint64(int64(t963))
        var inline1098 uint64 = 0 - inline1097
        var inline1099 string = decimal_string(inline1098)
        var inline1100 string = "-" + inline1099
        return inline1100
    } else {
        var inline1101 uint64 = uint64(int64(t963))
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
    var t972 bool = value__214 < 0
    if t972 {
        var t973 uint64 = uint64(int64(value__214))
        var t974 uint64 = 0 - t973
        var t975 string = decimal_string(t974)
        var t976 string = "-" + t975
        return t976
    } else {
        var t977 uint64 = uint64(int64(value__214))
        var t978 string = decimal_string(t977)
        return t978
    }
}

func decimal_string(value__208 uint64) string {
    var t1001 bool = value__208 == 0
    if t1001 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop994:
        for {
            var t995 bool = remaining__210 > 0
            if t995 {
                var t996_rhs uint64 = 10
                var t996 uint64 = remaining__210 % t996_rhs
                var t997 uint8 = uint8(uint64(t996))
                var t998 uint8 = t997 + 48
                vec_push__Vec_5uint8(reversed__209, t998)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t999 uint64 = compound_old353 / compound_value354
                remaining__210 = t999
                continue
            } else {
                break Loop_loop994
            }
        }
        var t983 int
        var inline1115 int = vec_len__Vec_5uint8(reversed__209)
        t983 = inline1115
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t983)
        var offset__212 int = 0
        Loop_loop985:
        for {
            var t986 int
            var inline1113 int = vec_len__Vec_5uint8(reversed__209)
            t986 = inline1113
            var t987 bool = offset__212 < t986
            if t987 {
                var t988 int
                var inline1111 int = vec_len__Vec_5uint8(reversed__209)
                t988 = inline1111
                var t989 int = t988 - offset__212
                var t990 int = t989 - 1
                var t991 uint8 = vec_get__Vec_5uint8(reversed__209, t990)
                vec_push__Vec_5uint8(bytes__211, t991)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t992 int = compound_old358 + compound_value359
                offset__212 = t992
                continue
            } else {
                break Loop_loop985
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
