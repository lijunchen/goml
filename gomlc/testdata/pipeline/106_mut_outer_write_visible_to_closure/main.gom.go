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

type closure_env_get_0 struct {
    x_0 *ref_int_x
}

type Ordering int32

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var t800 closure_env_get_0 = closure_env_get_0{
        x_0: x__0,
    }
    var get__1 func() int = func() int {
        return _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(t800)
    }
    ref_set__Ref_3int(x__0, 41)
    var t801 int = get__1()
    var t802 string
    var inline867 string = __goml_builtin_int_to_string(t801)
    t802 = inline867
    var inline864 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline864)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t813 int64 = int64(int(value__222))
    var inline873 bool = t813 < 0
    if inline873 {
        var inline874 uint64 = uint64(int64(t813))
        var inline875 uint64 = 0 - inline874
        var inline876 string = decimal_string(inline875)
        var inline877 string = "-" + inline876
        return inline877
    } else {
        var inline878 uint64 = uint64(int64(t813))
        var inline879 string = decimal_string(inline878)
        return inline879
    }
}

func decimal_string(value__208 uint64) string {
    var t848 bool = value__208 == 0
    if t848 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop841:
        for {
            var t842 bool = remaining__210 > 0
            if t842 {
                var t843_rhs uint64 = 10
                var t843 uint64 = remaining__210 % t843_rhs
                var t844 uint8 = uint8(uint64(t843))
                var t845 uint8 = t844 + 48
                vec_push__Vec_5uint8(reversed__209, t845)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t846 uint64 = compound_old353 / compound_value354
                remaining__210 = t846
                continue
            } else {
                break Loop_loop841
            }
        }
        var t830 int
        var inline889 int = vec_len__Vec_5uint8(reversed__209)
        t830 = inline889
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t830)
        var offset__212 int = 0
        Loop_loop832:
        for {
            var t833 int
            var inline887 int = vec_len__Vec_5uint8(reversed__209)
            t833 = inline887
            var t834 bool = offset__212 < t833
            if t834 {
                var t835 int
                var inline885 int = vec_len__Vec_5uint8(reversed__209)
                t835 = inline885
                var t836 int = t835 - offset__212
                var t837 int = t836 - 1
                var t838 uint8 = vec_get__Vec_5uint8(reversed__209, t837)
                vec_push__Vec_5uint8(bytes__211, t838)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t839 int = compound_old358 + compound_value359
                offset__212 = t839
                continue
            } else {
                break Loop_loop832
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__get__0_i_closure__env__get__0_i_apply(env798 closure_env_get_0) int {
    var x__0 *ref_int_x = env798.x_0
    var t862 int = ref_get__Ref_3int(x__0)
    return t862
}

func main() {
    main0()
}
