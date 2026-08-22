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

type closure_env_main_0 struct {}

type Ordering int32

type Maybe__isize struct {
    _tag int32
    _v1_0 int
}

type Maybe__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var t801 closure_env_main_0 = closure_env_main_0{}
    var t802 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t801, p0)
    }
    var commute_field903 string
    var inline874 int = 3
    var inline876 string = t802(inline874)
    commute_field903 = inline876
    var inline871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field903)
    _goml_runtime_core_string_println(inline871)
    return struct{}{}
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t820 int64 = int64(int(value__222))
    var inline883 bool = t820 < 0
    if inline883 {
        var inline884 uint64 = uint64(int64(t820))
        var inline885 uint64 = 0 - inline884
        var inline886 string = decimal_string(inline885)
        var inline887 string = "-" + inline886
        return inline887
    } else {
        var inline888 uint64 = uint64(int64(t820))
        var inline889 string = decimal_string(inline888)
        return inline889
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t857 bool = value__208 == 0
    if t857 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop850:
        for {
            var t851 bool = remaining__210 > 0
            if t851 {
                var t852_rhs uint64 = 10
                var t852 uint64 = remaining__210 % t852_rhs
                var t853 uint8 = uint8(uint64(t852))
                var t854 uint8 = t853 + 48
                vec_push__Vec_5uint8(reversed__209, t854)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t855 uint64 = compound_old353 / compound_value354
                remaining__210 = t855
                continue
            } else {
                break Loop_loop850
            }
        }
        var t839 int
        var inline899 int = vec_len__Vec_5uint8(reversed__209)
        t839 = inline899
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t839)
        var offset__212 int = 0
        Loop_loop841:
        for {
            var t842 int
            var inline897 int = vec_len__Vec_5uint8(reversed__209)
            t842 = inline897
            var t843 bool = offset__212 < t842
            if t843 {
                var t844 int
                var inline895 int = vec_len__Vec_5uint8(reversed__209)
                t844 = inline895
                var t845 int = t844 - offset__212
                var t846 int = t845 - 1
                var t847 uint8 = vec_get__Vec_5uint8(reversed__209, t846)
                vec_push__Vec_5uint8(bytes__211, t847)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t848 int = compound_old358 + compound_value359
                offset__212 = t848
                continue
            } else {
                break Loop_loop841
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env799 closure_env_main_0, item__4 int) string {
    var inline901 string = __goml_builtin_int_to_string(item__4)
    return inline901
}

func main() {
    main0()
}
