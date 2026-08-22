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

type hashmap_uint_string_x_entry struct {
    active bool
    key uint
    value string
}

type hashmap_uint_string_x struct {
    indices map[uint]int
    entries []hashmap_uint_string_x_entry
    len int
}

func hashmap_new__HashMap_4uint_6string() *hashmap_uint_string_x {
    return &hashmap_uint_string_x{
        indices: make(map[uint]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero string
        return zero, false
    }
    var entry hashmap_uint_string_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_4uint_6string(m, key)
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

func hashmap_set__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint, value string) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_uint_string_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_uint_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_uint_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
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

type Ordering int32

type Option__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var left__3 uint = 19
    var right__4_source int = 2
    var right__4 uint = uint(int(right__4_source))
    var result__5 uint
    var inline927 uint = left__3 + right__4
    var inline928 uint = inline927 * 2
    result__5 = inline928
    var inline924 string = _goml_m_trait__impl_i_ToString_i_usize_i_to__string(result__5)
    _goml_runtime_core_string_println(inline924)
    var t813 string
    var inline922 string = __goml_builtin_uint_to_string(result__5)
    t813 = inline922
    var inline919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t813)
    _goml_runtime_core_string_println(inline919)
    var t814 string
    switch result__5 {
    case 0:
        t814 = "zero"
    case 42:
        t814 = "answer"
    default:
        t814 = "other"
    }
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline915)
    var t815 bool = result__5 > left__3
    var inline912 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t815)
    _goml_runtime_core_string_println(inline912)
    var t816_rhs uint = 15
    var t816 uint = result__5 & t816_rhs
    var t817 uint64 = uint64(uint(t816))
    var inline909 string = _goml_m_trait__impl_i_ToString_i_u64_i_to__string(t817)
    _goml_runtime_core_string_println(inline909)
    var values__6 *hashmap_uint_string_x
    var inline907 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline907
    var inline904 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline904)
    var mtmp802 Option__string
    var inline901 uint = 42
    var inline902 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline901)
    mtmp802 = inline902
    switch mtmp802._tag {
    case 0:
        var inline894 string = "missing"
        var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline894)
        _goml_runtime_core_string_println(inline895)
        return struct{}{}
    case 1:
        var x803 string = mtmp802._v1_0
        var inline898 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x803)
        _goml_runtime_core_string_println(inline898)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_usize_i_to__string(self__704 uint) string {
    var inline933 uint64 = uint64(uint(self__704))
    var inline934 string = decimal_string(inline933)
    return inline934
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_uint_to_string(value__227 uint) string {
    var t848 uint64 = uint64(uint(value__227))
    var t849 string = decimal_string(t848)
    return t849
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t852 string = _goml_runtime_core_bool_to_string(self__401)
    return t852
}

func _goml_m_trait__impl_i_ToString_i_u64_i_to__string(self__412 uint64) string {
    var inline940 string = decimal_string(self__412)
    return inline940
}

func decimal_string(value__208 uint64) string {
    var t878 bool = value__208 == 0
    if t878 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop871:
        for {
            var t872 bool = remaining__210 > 0
            if t872 {
                var t873_rhs uint64 = 10
                var t873 uint64 = remaining__210 % t873_rhs
                var t874 uint8 = uint8(uint64(t873))
                var t875 uint8 = t874 + 48
                vec_push__Vec_5uint8(reversed__209, t875)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t876 uint64 = compound_old353 / compound_value354
                remaining__210 = t876
                continue
            } else {
                break Loop_loop871
            }
        }
        var t860 int
        var inline950 int = vec_len__Vec_5uint8(reversed__209)
        t860 = inline950
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t860)
        var offset__212 int = 0
        Loop_loop862:
        for {
            var t863 int
            var inline948 int = vec_len__Vec_5uint8(reversed__209)
            t863 = inline948
            var t864 bool = offset__212 < t863
            if t864 {
                var t865 int
                var inline946 int = vec_len__Vec_5uint8(reversed__209)
                t865 = inline946
                var t866 int = t865 - offset__212
                var t867 int = t866 - 1
                var t868 uint8 = vec_get__Vec_5uint8(reversed__209, t867)
                vec_push__Vec_5uint8(bytes__211, t868)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t869 int = compound_old358 + compound_value359
                offset__212 = t869
                continue
            } else {
                break Loop_loop862
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
