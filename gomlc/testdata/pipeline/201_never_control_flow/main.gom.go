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

func continue_branch() struct{} {
    var count__2 *ref_int_x
    var inline913 int = 0
    var inline914 *ref_int_x = ref__Ref_3int(inline913)
    count__2 = inline914
    Loop_loop809:
    for {
        var t810 int
        var inline911 int = ref_get__Ref_3int(count__2)
        t810 = inline911
        var t811 bool = t810 < 2
        if t811 {
            var t812 int
            var inline909 int = ref_get__Ref_3int(count__2)
            t812 = inline909
            var t813 int = t812 + 1
            ref_set__Ref_3int(count__2, t813)
            var t817 int
            var inline905 int = ref_get__Ref_3int(count__2)
            t817 = inline905
            var t818 bool = t817 == 1
            var jp815 int
            if t818 {
                continue
            } else {
                jp815 = 7
                var inline902 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp815)
                _goml_runtime_core_string_println(inline902)
                continue
            }
        } else {
            break Loop_loop809
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp823 int
    if stop__4 {
        return struct{}{}
    } else {
        jp823 = 9
        var inline916 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp823)
        _goml_runtime_core_string_println(inline916)
        return struct{}{}
    }
}

func main0() struct{} {
    var t825 int32
    var inline931 bool = false
    var inline933 int32
    if inline931 {
        t825 = 10
        var inline928 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t825)
        _goml_runtime_core_string_println(inline928)
        var t826 int32
        var inline922 bool = true
        var inline924 int32
        if inline922 {
            t826 = 10
            var inline919 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t826)
            _goml_runtime_core_string_println(inline919)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline924 = 20
            var inline926 int32 = inline924 + 1
            t826 = inline926
            var inline919 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t826)
            _goml_runtime_core_string_println(inline919)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline933 = 20
        var inline935 int32 = inline933 + 1
        t825 = inline935
        var inline928 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t825)
        _goml_runtime_core_string_println(inline928)
        var t826 int32
        var inline922 bool = true
        var inline924 int32
        if inline922 {
            t826 = 10
            var inline919 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t826)
            _goml_runtime_core_string_println(inline919)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline924 = 20
            var inline926 int32 = inline924 + 1
            t826 = inline926
            var inline919 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t826)
            _goml_runtime_core_string_println(inline919)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline941 int64 = int64(int(self__404))
    var inline942 string = signed_decimal_string(inline941)
    return inline942
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline944 int64 = int64(int32(self__407))
    var inline945 string = signed_decimal_string(inline944)
    return inline945
}

func signed_decimal_string(value__214 int64) string {
    var t860 bool = value__214 < 0
    if t860 {
        var t861 uint64 = uint64(int64(value__214))
        var t862 uint64 = 0 - t861
        var t863 string = decimal_string(t862)
        var t864 string = "-" + t863
        return t864
    } else {
        var t865 uint64 = uint64(int64(value__214))
        var t866 string = decimal_string(t865)
        return t866
    }
}

func decimal_string(value__208 uint64) string {
    var t889 bool = value__208 == 0
    if t889 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop882:
        for {
            var t883 bool = remaining__210 > 0
            if t883 {
                var t884_rhs uint64 = 10
                var t884 uint64 = remaining__210 % t884_rhs
                var t885 uint8 = uint8(uint64(t884))
                var t886 uint8 = t885 + 48
                vec_push__Vec_5uint8(reversed__209, t886)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t887 uint64 = compound_old353 / compound_value354
                remaining__210 = t887
                continue
            } else {
                break Loop_loop882
            }
        }
        var t871 int
        var inline971 int = vec_len__Vec_5uint8(reversed__209)
        t871 = inline971
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t871)
        var offset__212 int = 0
        Loop_loop873:
        for {
            var t874 int
            var inline969 int = vec_len__Vec_5uint8(reversed__209)
            t874 = inline969
            var t875 bool = offset__212 < t874
            if t875 {
                var t876 int
                var inline967 int = vec_len__Vec_5uint8(reversed__209)
                t876 = inline967
                var t877 int = t876 - offset__212
                var t878 int = t877 - 1
                var t879 uint8 = vec_get__Vec_5uint8(reversed__209, t878)
                vec_push__Vec_5uint8(bytes__211, t879)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t880 int = compound_old358 + compound_value359
                offset__212 = t880
                continue
            } else {
                break Loop_loop873
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
