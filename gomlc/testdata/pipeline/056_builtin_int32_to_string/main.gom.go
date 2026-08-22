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

type Ordering int32

func main0() struct{} {
    var value__0 int32 = 42
    var text__1 string
    var inline853 string = __goml_builtin_int32_to_string(value__0)
    text__1 = inline853
    var inline850 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__1)
    _goml_runtime_core_string_println(inline850)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t806 int64 = int64(int32(value__225))
    var inline859 bool = t806 < 0
    if inline859 {
        var inline860 uint64 = uint64(int64(t806))
        var inline861 uint64 = 0 - inline860
        var inline862 string = decimal_string(inline861)
        var inline863 string = "-" + inline862
        return inline863
    } else {
        var inline864 uint64 = uint64(int64(t806))
        var inline865 string = decimal_string(inline864)
        return inline865
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t843 bool = value__208 == 0
    if t843 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop836:
        for {
            var t837 bool = remaining__210 > 0
            if t837 {
                var t838_rhs uint64 = 10
                var t838 uint64 = remaining__210 % t838_rhs
                var t839 uint8 = uint8(uint64(t838))
                var t840 uint8 = t839 + 48
                vec_push__Vec_5uint8(reversed__209, t840)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t841 uint64 = compound_old353 / compound_value354
                remaining__210 = t841
                continue
            } else {
                break Loop_loop836
            }
        }
        var t825 int
        var inline875 int = vec_len__Vec_5uint8(reversed__209)
        t825 = inline875
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t825)
        var offset__212 int = 0
        Loop_loop827:
        for {
            var t828 int
            var inline873 int = vec_len__Vec_5uint8(reversed__209)
            t828 = inline873
            var t829 bool = offset__212 < t828
            if t829 {
                var t830 int
                var inline871 int = vec_len__Vec_5uint8(reversed__209)
                t830 = inline871
                var t831 int = t830 - offset__212
                var t832 int = t831 - 1
                var t833 uint8 = vec_get__Vec_5uint8(reversed__209, t832)
                vec_push__Vec_5uint8(bytes__211, t833)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t834 int = compound_old358 + compound_value359
                offset__212 = t834
                continue
            } else {
                break Loop_loop827
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
