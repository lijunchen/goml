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

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    indices map[string]int
    entries []hashmap_string_int32_x_entry
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int32
        return zero, false
    }
    var entry hashmap_string_int32_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Option__i32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__i32{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int32_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int32_x_entry{
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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func fetch(flag__0 bool) Option__i32 {
    var m__1 *hashmap_string_int32_x
    var inline894 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline894
    if flag__0 {
        var inline887 string = "a"
        var inline888 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline887, inline888)
    } else {}
    var mtmp798 Option__i32
    var inline891 string = "a"
    var inline892 Option__i32 = hashmap_get__HashMap_6string_5int32(m__1, inline891)
    mtmp798 = inline892
    var jp807 int32
    switch mtmp798._tag {
    case 0:
        return Option__i32{
            _tag: 0,
        }
    case 1:
        var x799 int32 = mtmp798._v1_0
        jp807 = x799
        var t808 int32 = jp807 + 1
        var t809 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t808,
        }
        return t809
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t817 Option__i32 = fetch(true)
    var t818 string
    switch t817._tag {
    case 0:
        t818 = "none"
    case 1:
        var inline909 int32 = t817._v1_0
        var inline911 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline909)
        var inline912 string = "some=" + inline911
        t818 = inline912
    default:
        panic("non-exhaustive match")
    }
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
    _goml_runtime_core_string_println(inline906)
    var t819 Option__i32 = fetch(false)
    var t820 string
    switch t819._tag {
    case 0:
        t820 = "none"
    case 1:
        var inline901 int32 = t819._v1_0
        var inline903 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline901)
        var inline904 string = "some=" + inline903
        t820 = inline904
    default:
        panic("non-exhaustive match")
    }
    var inline898 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline898)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__286 int32) string {
    var inline914 int64 = int64(int32(self__286))
    var inline915 string = signed_decimal_string(inline914)
    return inline915
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t845 bool = value__214 < 0
    if t845 {
        var t846 uint64 = uint64(int64(value__214))
        var t847 uint64 = 0 - t846
        var t848 string = decimal_string(t847)
        var t849 string = "-" + t848
        return t849
    } else {
        var t850 uint64 = uint64(int64(value__214))
        var t851 string = decimal_string(t850)
        return t851
    }
}

func decimal_string(value__208 uint64) string {
    var t874 bool = value__208 == 0
    if t874 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop867:
        for {
            var t868 bool = remaining__210 > 0
            if t868 {
                var t869_rhs uint64 = 10
                var t869 uint64 = remaining__210 % t869_rhs
                var t870 uint8 = uint8(uint64(t869))
                var t871 uint8 = t870 + 48
                vec_push__Vec_5uint8(reversed__209, t871)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t872 uint64 = compound_old353 / compound_value354
                remaining__210 = t872
                continue
            } else {
                break Loop_loop867
            }
        }
        var t856 int
        var inline934 int = vec_len__Vec_5uint8(reversed__209)
        t856 = inline934
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t856)
        var offset__212 int = 0
        Loop_loop858:
        for {
            var t859 int
            var inline932 int = vec_len__Vec_5uint8(reversed__209)
            t859 = inline932
            var t860 bool = offset__212 < t859
            if t860 {
                var t861 int
                var inline930 int = vec_len__Vec_5uint8(reversed__209)
                t861 = inline930
                var t862 int = t861 - offset__212
                var t863 int = t862 - 1
                var t864 uint8 = vec_get__Vec_5uint8(reversed__209, t863)
                vec_push__Vec_5uint8(bytes__211, t864)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t865 int = compound_old358 + compound_value359
                offset__212 = t865
                continue
            } else {
                break Loop_loop858
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
