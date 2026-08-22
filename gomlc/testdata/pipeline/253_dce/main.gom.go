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
    var counter__8 *ref_int_x
    var inline881 int = 0
    var inline882 *ref_int_x = ref__Ref_3int(inline881)
    counter__8 = inline882
    var inline877 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(counter__8)
    var inline878 int = inline877 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(counter__8, inline878)
    var t802 int
    var inline875 int = ref_get__Ref_3int(counter__8)
    t802 = inline875
    var inline872 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t802)
    _goml_runtime_core_string_println(inline872)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t806 int = ref_get__Ref_3int(self__685)
    return t806
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline886 int64 = int64(int(self__404))
    var inline887 string = signed_decimal_string(inline886)
    return inline887
}

func signed_decimal_string(value__214 int64) string {
    var t826 bool = value__214 < 0
    if t826 {
        var t827 uint64 = uint64(int64(value__214))
        var t828 uint64 = 0 - t827
        var t829 string = decimal_string(t828)
        var t830 string = "-" + t829
        return t830
    } else {
        var t831 uint64 = uint64(int64(value__214))
        var t832 string = decimal_string(t831)
        return t832
    }
}

func decimal_string(value__208 uint64) string {
    var t855 bool = value__208 == 0
    if t855 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop848:
        for {
            var t849 bool = remaining__210 > 0
            if t849 {
                var t850_rhs uint64 = 10
                var t850 uint64 = remaining__210 % t850_rhs
                var t851 uint8 = uint8(uint64(t850))
                var t852 uint8 = t851 + 48
                vec_push__Vec_5uint8(reversed__209, t852)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t853 uint64 = compound_old353 / compound_value354
                remaining__210 = t853
                continue
            } else {
                break Loop_loop848
            }
        }
        var t837 int
        var inline905 int = vec_len__Vec_5uint8(reversed__209)
        t837 = inline905
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t837)
        var offset__212 int = 0
        Loop_loop839:
        for {
            var t840 int
            var inline903 int = vec_len__Vec_5uint8(reversed__209)
            t840 = inline903
            var t841 bool = offset__212 < t840
            if t841 {
                var t842 int
                var inline901 int = vec_len__Vec_5uint8(reversed__209)
                t842 = inline901
                var t843 int = t842 - offset__212
                var t844 int = t843 - 1
                var t845 uint8 = vec_get__Vec_5uint8(reversed__209, t844)
                vec_push__Vec_5uint8(bytes__211, t845)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t846 int = compound_old358 + compound_value359
                offset__212 = t846
                continue
            } else {
                break Loop_loop839
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
