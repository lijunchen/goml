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
    var i__0 *ref_int_x
    var inline898 int = 0
    var inline899 *ref_int_x = ref__Ref_3int(inline898)
    i__0 = inline899
    Loop_loop803:
    for {
        var t804 int
        var inline892 int = ref_get__Ref_3int(i__0)
        t804 = inline892
        var t805 bool = t804 < 10
        if t805 {
            var t810 int
            var inline890 int = ref_get__Ref_3int(i__0)
            t810 = inline890
            var t811 bool = t810 == 5
            if t811 {
                break Loop_loop803
            } else {
                var t807 int
                var inline888 int = ref_get__Ref_3int(i__0)
                t807 = inline888
                var inline885 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t807)
                _goml_runtime_core_string_println(inline885)
                var t808 int
                var inline883 int = ref_get__Ref_3int(i__0)
                t808 = inline883
                var t809 int = t808 + 1
                ref_set__Ref_3int(i__0, t809)
                continue
            }
        } else {
            break Loop_loop803
        }
    }
    var inline894 string = "done"
    var inline895 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline894)
    _goml_runtime_core_string_println(inline895)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline904 int64 = int64(int(self__404))
    var inline905 string = signed_decimal_string(inline904)
    return inline905
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func signed_decimal_string(value__214 int64) string {
    var t839 bool = value__214 < 0
    if t839 {
        var t840 uint64 = uint64(int64(value__214))
        var t841 uint64 = 0 - t840
        var t842 string = decimal_string(t841)
        var t843 string = "-" + t842
        return t843
    } else {
        var t844 uint64 = uint64(int64(value__214))
        var t845 string = decimal_string(t844)
        return t845
    }
}

func decimal_string(value__208 uint64) string {
    var t868 bool = value__208 == 0
    if t868 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop861:
        for {
            var t862 bool = remaining__210 > 0
            if t862 {
                var t863_rhs uint64 = 10
                var t863 uint64 = remaining__210 % t863_rhs
                var t864 uint8 = uint8(uint64(t863))
                var t865 uint8 = t864 + 48
                vec_push__Vec_5uint8(reversed__209, t865)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t866 uint64 = compound_old353 / compound_value354
                remaining__210 = t866
                continue
            } else {
                break Loop_loop861
            }
        }
        var t850 int
        var inline923 int = vec_len__Vec_5uint8(reversed__209)
        t850 = inline923
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t850)
        var offset__212 int = 0
        Loop_loop852:
        for {
            var t853 int
            var inline921 int = vec_len__Vec_5uint8(reversed__209)
            t853 = inline921
            var t854 bool = offset__212 < t853
            if t854 {
                var t855 int
                var inline919 int = vec_len__Vec_5uint8(reversed__209)
                t855 = inline919
                var t856 int = t855 - offset__212
                var t857 int = t856 - 1
                var t858 uint8 = vec_get__Vec_5uint8(reversed__209, t857)
                vec_push__Vec_5uint8(bytes__211, t858)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t859 int = compound_old358 + compound_value359
                offset__212 = t859
                continue
            } else {
                break Loop_loop852
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
