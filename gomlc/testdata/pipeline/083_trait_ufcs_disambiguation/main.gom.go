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

type S struct {}

type Ordering int32

func _goml_m_trait__impl_i_A_i_S_i_pick(self__0 S) int32 {
    return 10
}

func _goml_m_trait__impl_i_B_i_S_i_pick(self__1 S) int32 {
    return 20
}

func main0() struct{} {
    var t803 S = S{}
    var t804 int32
    var inline879 int32 = _goml_m_trait__impl_i_A_i_S_i_pick(t803)
    t804 = inline879
    var t805 string
    var inline877 string = __goml_builtin_int32_to_string(t804)
    t805 = inline877
    var inline874 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline874)
    var t806 S = S{}
    var t807 int32
    var inline872 int32 = _goml_m_trait__impl_i_B_i_S_i_pick(t806)
    t807 = inline872
    var t808 string
    var inline870 string = __goml_builtin_int32_to_string(t807)
    t808 = inline870
    var inline867 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline867)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t825 int64 = int64(int32(value__225))
    var inline887 bool = t825 < 0
    if inline887 {
        var inline888 uint64 = uint64(int64(t825))
        var inline889 uint64 = 0 - inline888
        var inline890 string = decimal_string(inline889)
        var inline891 string = "-" + inline890
        return inline891
    } else {
        var inline892 uint64 = uint64(int64(t825))
        var inline893 string = decimal_string(inline892)
        return inline893
    }
}

func decimal_string(value__208 uint64) string {
    var t860 bool = value__208 == 0
    if t860 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop853:
        for {
            var t854 bool = remaining__210 > 0
            if t854 {
                var t855_rhs uint64 = 10
                var t855 uint64 = remaining__210 % t855_rhs
                var t856 uint8 = uint8(uint64(t855))
                var t857 uint8 = t856 + 48
                vec_push__Vec_5uint8(reversed__209, t857)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t858 uint64 = compound_old353 / compound_value354
                remaining__210 = t858
                continue
            } else {
                break Loop_loop853
            }
        }
        var t842 int
        var inline903 int = vec_len__Vec_5uint8(reversed__209)
        t842 = inline903
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t842)
        var offset__212 int = 0
        Loop_loop844:
        for {
            var t845 int
            var inline901 int = vec_len__Vec_5uint8(reversed__209)
            t845 = inline901
            var t846 bool = offset__212 < t845
            if t846 {
                var t847 int
                var inline899 int = vec_len__Vec_5uint8(reversed__209)
                t847 = inline899
                var t848 int = t847 - offset__212
                var t849 int = t848 - 1
                var t850 uint8 = vec_get__Vec_5uint8(reversed__209, t849)
                vec_push__Vec_5uint8(bytes__211, t850)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t851 int = compound_old358 + compound_value359
                offset__212 = t851
                continue
            } else {
                break Loop_loop844
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
