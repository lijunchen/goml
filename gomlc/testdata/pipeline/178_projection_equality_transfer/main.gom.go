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

type LeftSource struct {
    value int32
}

type RightSource struct {
    value int32
}

type Ordering int32

func _goml_m_trait__impl_i_Mark_i_i32_i_marked(self__0 int32) string {
    var t799 string
    var inline871 string = __goml_builtin_int32_to_string(self__0)
    t799 = inline871
    var t800 string = "m" + t799
    return t800
}

func _goml_m_trait__impl_i_Source_i_LeftSource_i_get(self__1 LeftSource) int32 {
    var t803 int32 = self__1.value
    return t803
}

func _goml_m_trait__impl_i_Source_i_RightSource_i_get(self__2 RightSource) int32 {
    var t806 int32 = self__2.value
    return t806
}

func main0() struct{} {
    var t808 LeftSource = LeftSource{
        value: 3,
    }
    var t809 RightSource = RightSource{
        value: 4,
    }
    var t810 string
    var inline876 int32 = _goml_m_trait__impl_i_Source_i_LeftSource_i_get(t808)
    var inline877 string = _goml_m_trait__impl_i_Mark_i_i32_i_marked(inline876)
    var inline878 string = inline877 + ":"
    var inline879 int32 = _goml_m_trait__impl_i_Source_i_RightSource_i_get(t809)
    var inline880 string = _goml_m_trait__impl_i_Mark_i_i32_i_marked(inline879)
    var inline881 string = inline878 + inline880
    t810 = inline881
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t827 int64 = int64(int32(value__225))
    var inline897 bool = t827 < 0
    if inline897 {
        var inline898 uint64 = uint64(int64(t827))
        var inline899 uint64 = 0 - inline898
        var inline900 string = decimal_string(inline899)
        var inline901 string = "-" + inline900
        return inline901
    } else {
        var inline902 uint64 = uint64(int64(t827))
        var inline903 string = decimal_string(inline902)
        return inline903
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t864 bool = value__208 == 0
    if t864 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop857:
        for {
            var t858 bool = remaining__210 > 0
            if t858 {
                var t859_rhs uint64 = 10
                var t859 uint64 = remaining__210 % t859_rhs
                var t860 uint8 = uint8(uint64(t859))
                var t861 uint8 = t860 + 48
                vec_push__Vec_5uint8(reversed__209, t861)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t862 uint64 = compound_old353 / compound_value354
                remaining__210 = t862
                continue
            } else {
                break Loop_loop857
            }
        }
        var t846 int
        var inline913 int = vec_len__Vec_5uint8(reversed__209)
        t846 = inline913
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t846)
        var offset__212 int = 0
        Loop_loop848:
        for {
            var t849 int
            var inline911 int = vec_len__Vec_5uint8(reversed__209)
            t849 = inline911
            var t850 bool = offset__212 < t849
            if t850 {
                var t851 int
                var inline909 int = vec_len__Vec_5uint8(reversed__209)
                t851 = inline909
                var t852 int = t851 - offset__212
                var t853 int = t852 - 1
                var t854 uint8 = vec_get__Vec_5uint8(reversed__209, t853)
                vec_push__Vec_5uint8(bytes__211, t854)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t855 int = compound_old358 + compound_value359
                offset__212 = t855
                continue
            } else {
                break Loop_loop848
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
