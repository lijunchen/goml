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

type closure_env_inc_0 struct {
    x_0 *ref_int_x
}

type closure_env_get_1 struct {
    x_0 *ref_int_x
}

type Ordering int32

func main0() struct{} {
    var x__0 *ref_int_x = ref__Ref_3int(0)
    var t802 closure_env_inc_0 = closure_env_inc_0{
        x_0: x__0,
    }
    var inc__1 func() int = func() int {
        return _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(t802)
    }
    var t803 closure_env_get_1 = closure_env_get_1{
        x_0: x__0,
    }
    var get__2 func() int = func() int {
        return _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(t803)
    }
    inc__1()
    var t804 int = get__2()
    var t805 string
    var inline875 string = __goml_builtin_int_to_string(t804)
    t805 = inline875
    var inline872 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline872)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t816 int64 = int64(int(value__222))
    var inline881 bool = t816 < 0
    if inline881 {
        var inline882 uint64 = uint64(int64(t816))
        var inline883 uint64 = 0 - inline882
        var inline884 string = decimal_string(inline883)
        var inline885 string = "-" + inline884
        return inline885
    } else {
        var inline886 uint64 = uint64(int64(t816))
        var inline887 string = decimal_string(inline886)
        return inline887
    }
}

func decimal_string(value__208 uint64) string {
    var t851 bool = value__208 == 0
    if t851 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop844:
        for {
            var t845 bool = remaining__210 > 0
            if t845 {
                var t846_rhs uint64 = 10
                var t846 uint64 = remaining__210 % t846_rhs
                var t847 uint8 = uint8(uint64(t846))
                var t848 uint8 = t847 + 48
                vec_push__Vec_5uint8(reversed__209, t848)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t849 uint64 = compound_old353 / compound_value354
                remaining__210 = t849
                continue
            } else {
                break Loop_loop844
            }
        }
        var t833 int
        var inline897 int = vec_len__Vec_5uint8(reversed__209)
        t833 = inline897
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t833)
        var offset__212 int = 0
        Loop_loop835:
        for {
            var t836 int
            var inline895 int = vec_len__Vec_5uint8(reversed__209)
            t836 = inline895
            var t837 bool = offset__212 < t836
            if t837 {
                var t838 int
                var inline893 int = vec_len__Vec_5uint8(reversed__209)
                t838 = inline893
                var t839 int = t838 - offset__212
                var t840 int = t839 - 1
                var t841 uint8 = vec_get__Vec_5uint8(reversed__209, t840)
                vec_push__Vec_5uint8(bytes__211, t841)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t842 int = compound_old358 + compound_value359
                offset__212 = t842
                continue
            } else {
                break Loop_loop835
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__inc__0_i_closure__env__inc__0_i_apply(env799 closure_env_inc_0) int {
    var x__0 *ref_int_x = env799.x_0
    var t865 int = ref_get__Ref_3int(x__0)
    var t866 int = t865 + 1
    ref_set__Ref_3int(x__0, t866)
    var t867 int = ref_get__Ref_3int(x__0)
    return t867
}

func _goml_m_inherent_i_closure__env__get__1_i_closure__env__get__1_i_apply(env800 closure_env_get_1) int {
    var x__0 *ref_int_x = env800.x_0
    var t870 int = ref_get__Ref_3int(x__0)
    return t870
}

func main() {
    main0()
}
