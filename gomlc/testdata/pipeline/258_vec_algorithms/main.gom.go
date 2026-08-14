package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_strings "strings"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_concat(values *_goml_vec_string) string {
    return _goml_strings.Join(values.items, "")
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: _goml_slices.Grow([]int{}, int(capacity)),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

func vec_truncate__Vec_3int(vec *_goml_vec_int, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_set__Vec_6string(vec *_goml_vec_string, index int, value string) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_Tuple2_3int_3int struct {
    items []Tuple2_3int_3int
}

func vec_new__Vec_16Tuple2_3int_3int() *_goml_vec_Tuple2_3int_3int {
    return &_goml_vec_Tuple2_3int_3int{
        items: nil,
    }
}

func vec_with_capacity__Vec_16Tuple2_3int_3int(capacity int) *_goml_vec_Tuple2_3int_3int {
    return &_goml_vec_Tuple2_3int_3int{
        items: _goml_slices.Grow([]Tuple2_3int_3int{}, int(capacity)),
    }
}

func vec_push__Vec_16Tuple2_3int_3int(vec *_goml_vec_Tuple2_3int_3int, elem Tuple2_3int_3int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_16Tuple2_3int_3int(vec *_goml_vec_Tuple2_3int_3int, index int) Tuple2_3int_3int {
    return vec.items[index]
}

func vec_set__Vec_16Tuple2_3int_3int(vec *_goml_vec_Tuple2_3int_3int, index int, value Tuple2_3int_3int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_16Tuple2_3int_3int(vec *_goml_vec_Tuple2_3int_3int) int {
    return int(len(vec.items))
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

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_main_4 struct {}

type closure_env_main_5 struct {}

type closure_env_main_6 struct {}

type closure_env_main_7 struct {}

type closure_env_main_8 struct {}

type closure_env_main_9 struct {}

type closure_env_inherent_Vec_Vec_T_dedup_T_int_10 struct {}

type closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 struct {
    compare_0 func(string, string) Ordering
}

type closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12 struct {
    compare_0 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering
}

type closure_env_goml_builtin_range_13 struct {
    current_0 *ref_int_x
    end_1 int
}

type FrozenVec__int struct {
    values *_goml_vec_int
}

type Ordering int32

const (
    Less Ordering = 0
    Equal Ordering = 1
    Greater Ordering = 2
)

type Option__Ordering interface {
    isOption__Ordering()
}

type Option__Ordering_None struct {}

func (_ Option__Ordering_None) isOption__Ordering() {}

type Option__Ordering_Some struct {
    _0 Ordering
}

func (_ Option__Ordering_Some) isOption__Ordering() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(self__24 string, other__25 string) Ordering {
    var t546 bool = self__24 < other__25
    if t546 {
        return Less
    } else {
        var t549 bool = self__24 > other__25
        if t549 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__32 int, other__33 int) Ordering {
    var t604 bool = self__32 < other__33
    if t604 {
        return Less
    } else {
        var t607 bool = self__32 > other__33
        if t607 {
            return Greater
        } else {
            return Equal
        }
    }
}

func main0() struct{} {
    var vec_literal__52 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 5)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 9)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__52, 6)
    var t943 closure_env_main_0 = closure_env_main_0{}
    var t944 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t943, p0)
    }
    var t945 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(vec_literal__52, t944)
    var t946 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t945, -1)
    var t947 string = _goml_m_inherent_i_int_i_int_i_to__string(t946)
    println__T_string(t947)
    var t948 closure_env_main_1 = closure_env_main_1{}
    var t949 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t948, p0)
    }
    var t950 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(vec_literal__52, t949)
    var t951 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t950, -1)
    var t952 string = _goml_m_inherent_i_int_i_int_i_to__string(t951)
    println__T_string(t952)
    var t953 closure_env_main_2 = closure_env_main_2{}
    var t954 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t953, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(vec_literal__52, t954)
    var t955 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(vec_literal__52, ",")
    println__T_string(t955)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(vec_literal__52)
    var t956 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(vec_literal__52, ",")
    println__T_string(t956)
    var vec_literal__404 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__404, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__404, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__404, 4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__404, 4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__404, 5)
    var t957 closure_env_main_3 = closure_env_main_3{}
    var t958 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t957, p0)
    }
    var t959 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(vec_literal__404, t958)
    var t960 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t959, -1)
    var t961 string = _goml_m_inherent_i_int_i_int_i_to__string(t960)
    println__T_string(t961)
    var t962 closure_env_main_4 = closure_env_main_4{}
    var t963 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t962, p0)
    }
    var t964 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(vec_literal__404, t963)
    var t965 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t964, -1)
    var t966 string = _goml_m_inherent_i_int_i_int_i_to__string(t965)
    println__T_string(t966)
    var t967 closure_env_main_5 = closure_env_main_5{}
    var t968 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t967, p0, p1)
    }
    var t969 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(vec_literal__404, t968)
    var t970 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t969, 0)
    var t971 string = _goml_m_inherent_i_int_i_int_i_to__string(t970)
    println__T_string(t971)
    var t972 closure_env_main_6 = closure_env_main_6{}
    var t973 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t972, p0, p1)
    }
    var t974 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(vec_literal__404, t973)
    var t975 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t974, 0)
    var t976 string = _goml_m_inherent_i_int_i_int_i_to__string(t975)
    println__T_string(t976)
    var vec_literal__804 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vec_literal__804, "beta")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vec_literal__804, "alpha")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(vec_literal__804, "gamma")
    var t977 closure_env_main_7 = closure_env_main_7{}
    var t978 func(string, string) Ordering = func(p0 string, p1 string) Ordering {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t977, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(vec_literal__804, t978)
    var t979 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(vec_literal__804, "|")
    println__T_string(t979)
    var vec_literal__968 *_goml_vec_Tuple2_3int_3int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int_c_int_q_()
    var t980 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int_c_int_q_(vec_literal__968, t980)
    var t981 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 1,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int_c_int_q_(vec_literal__968, t981)
    var t982 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 0,
        _1: 9,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int_c_int_q_(vec_literal__968, t982)
    var t983 closure_env_main_8 = closure_env_main_8{}
    var t984 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) Ordering {
        return _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(t983, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T___o_int_c_int_q_(vec_literal__968, t984)
    var t985 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int_c_int_q_(vec_literal__968)
    var t986 string = _goml_m_inherent_i_int_i_int_i_to__string(t985)
    println__T_string(t986)
    var vec_literal__1149 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1149, 7)
    var inline2347 int = 8
    vec_push__Vec_3int(vec_literal__1149, inline2347)
    var inline2344 int = 9
    vec_push__Vec_3int(vec_literal__1149, inline2344)
    var view__18 []int
    var inline2340 int = 0
    var inline2341 int = 2
    var inline2342 []int = vec_literal__1149.items[inline2340:inline2341]
    view__18 = inline2342
    var t987 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 8)
    var t988 string
    var inline2338 string = _goml_runtime_core_bool_to_string(t987)
    t988 = inline2338
    var inline2335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t988)
    _goml_runtime_core_string_println(inline2335)
    var t989 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 9)
    var t990 string
    var inline2333 string = _goml_runtime_core_bool_to_string(t989)
    t990 = inline2333
    var inline2330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t990)
    _goml_runtime_core_string_println(inline2330)
    var vec_literal__1280 *_goml_vec_int
    var inline2328 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1280 = inline2328
    var inline2325 int = 1
    vec_push__Vec_3int(vec_literal__1280, inline2325)
    var inline2322 int = 2
    vec_push__Vec_3int(vec_literal__1280, inline2322)
    var frozen__19 FrozenVec__int
    var inline2319 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(vec_literal__1280)
    var inline2320 FrozenVec__int = FrozenVec__int{
        values: inline2319,
    }
    frozen__19 = inline2320
    var t991 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 1)
    var t992 string
    var inline2317 string = _goml_runtime_core_bool_to_string(t991)
    t992 = inline2317
    var inline2314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t992)
    _goml_runtime_core_string_println(inline2314)
    var t993 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 3)
    var t994 string
    var inline2312 string = _goml_runtime_core_bool_to_string(t993)
    t994 = inline2312
    var inline2309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t994)
    _goml_runtime_core_string_println(inline2309)
    var vec_literal__1418 *_goml_vec_int
    var inline2307 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1418 = inline2307
    var t995 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(vec_literal__1418, ",")
    var inline2304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t995)
    _goml_runtime_core_string_println(inline2304)
    var t996 closure_env_main_9 = closure_env_main_9{}
    var t997 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(t996, p0, p1)
    }
    var t998 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(vec_literal__1418, t997)
    var t999 bool
    var inline2301 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__int(t998)
    var inline2302 bool = !inline2301
    t999 = inline2302
    var t1000 string
    var inline2299 string = _goml_runtime_core_bool_to_string(t999)
    t1000 = inline2299
    var inline2296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1000)
    _goml_runtime_core_string_println(inline2296)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t1388 *_goml_vec_int = vec_new__Vec_3int()
    return t1388
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t1392 string
    t1392 = value__1
    _goml_runtime_core_string_println(t1392)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(self__319 *_goml_vec_int, predicate__320 func(int) bool) Option__int {
    var t1396 int
    var inline2825 int = vec_len__Vec_3int(self__319)
    t1396 = inline2825
    var t1397 FnIterator__int
    var inline2819 int = 0
    var inline2820 *ref_int_x = ref__Ref_3int(inline2819)
    var inline2821 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2820,
        end_1: t1396,
    }
    var inline2822 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2821)
    }
    var inline2823 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2822)
    t1397 = inline2823
    var for_iter257 FnIterator__int
    for_iter257 = t1397
    Loop_loop1399:
    for {
        var for_next258 Option__int
        var inline2815 func() Option__int = for_iter257.next_fn
        var inline2816 Option__int = inline2815()
        for_next258 = inline2816
        switch for_next258.(type) {
        case Option__int_None:
            break Loop_loop1399
        case Option__int_Some:
            var x259 int = for_next258.(Option__int_Some)._0
            var t1402 int = vec_get__Vec_3int(self__319, x259)
            var t1403 bool = predicate__320(t1402)
            if t1403 {
                var t1404 Option__int = Option__int_Some{
                    _0: x259,
                }
                return t1404
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return Option__int_None{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(self__458 Option__int, fallback__459 int) int {
    switch self__458.(type) {
    case Option__int_None:
        return fallback__459
    case Option__int_Some:
        var x387 int = self__458.(Option__int_Some)._0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t1411 string = _goml_runtime_core_int_to_string(self__32)
    return t1411
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(self__335 *_goml_vec_int, compare__336 func(int, int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__335, compare__336)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(self__378 *_goml_vec_int, separator__379 string) string {
    var t1416 int
    var inline2865 int = vec_len__Vec_3int(self__378)
    t1416 = inline2865
    var parts__380 *_goml_vec_string
    var inline2863 *_goml_vec_string = vec_with_capacity__Vec_6string(t1416)
    parts__380 = inline2863
    var t1417 int
    var inline2861 int = vec_len__Vec_3int(self__378)
    t1417 = inline2861
    var t1418 FnIterator__int
    var inline2855 int = 0
    var inline2856 *ref_int_x = ref__Ref_3int(inline2855)
    var inline2857 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2856,
        end_1: t1417,
    }
    var inline2858 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2857)
    }
    var inline2859 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2858)
    t1418 = inline2859
    var for_iter349 FnIterator__int
    for_iter349 = t1418
    Loop_loop1433:
    for {
        var for_next350 Option__int
        var inline2831 func() Option__int = for_iter349.next_fn
        var inline2832 Option__int = inline2831()
        for_next350 = inline2832
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop1433
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t1435 int = vec_get__Vec_3int(self__378, x351)
            var t1436 string
            var inline2829 string = _goml_runtime_core_int_to_string(t1435)
            t1436 = inline2829
            vec_push__Vec_6string(parts__380, t1436)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1420 int
    var inline2852 int = vec_len__Vec_6string(parts__380)
    t1420 = inline2852
    var t1421 int = t1420 * 2
    var result__382 *_goml_vec_string
    var inline2850 *_goml_vec_string = vec_with_capacity__Vec_6string(t1421)
    result__382 = inline2850
    var t1422 int
    var inline2848 int = vec_len__Vec_6string(parts__380)
    t1422 = inline2848
    var t1423 FnIterator__int
    var inline2842 int = 0
    var inline2843 *ref_int_x = ref__Ref_3int(inline2842)
    var inline2844 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2843,
        end_1: t1422,
    }
    var inline2845 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2844)
    }
    var inline2846 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2845)
    t1423 = inline2846
    var for_iter353 FnIterator__int
    for_iter353 = t1423
    Loop_loop1426:
    for {
        var for_next354 Option__int
        var inline2838 func() Option__int = for_iter353.next_fn
        var inline2839 Option__int = inline2838()
        for_next354 = inline2839
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop1426
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t1431 bool = x355 > 0
            if t1431 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1429 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1429)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1425 string = _goml_runtime_core_string_concat(result__382)
    return t1425
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(self__387 *_goml_vec_int) struct{} {
    var t1439 closure_env_inherent_Vec_Vec_T_dedup_T_int_10 = closure_env_inherent_Vec_Vec_T_dedup_T_int_10{}
    var t1440 func(int, int) bool = func(p0 int, p1 int) bool {
        return _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(t1439, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__387, t1440)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(self__345 *_goml_vec_int, compare__346 func(int) int) Option__int {
    var low__347 int = 0
    var high__348 int
    var inline2869 int = vec_len__Vec_3int(self__345)
    high__348 = inline2869
    Loop_loop1455:
    for {
        var t1456 bool = low__347 < high__348
        if t1456 {
            var t1457 int = high__348 - low__347
            var t1458 int = t1457 / 2
            var middle__349 int = low__347 + t1458
            var t1460 int = vec_get__Vec_3int(self__345, middle__349)
            var t1461 int = compare__346(t1460)
            var t1462 bool = t1461 < 0
            if t1462 {
                var t1463 int = middle__349 + 1
                low__347 = t1463
                continue
            } else {
                high__348 = middle__349
                continue
            }
        } else {
            break Loop_loop1455
        }
    }
    var t1450 int
    var inline2867 int = vec_len__Vec_3int(self__345)
    t1450 = inline2867
    var t1451 bool = low__347 < t1450
    var jp1448 bool
    if t1451 {
        var t1452 int = vec_get__Vec_3int(self__345, low__347)
        var t1453 int = compare__346(t1452)
        var t1454 bool = t1453 == 0
        jp1448 = t1454
    } else {
        jp1448 = false
    }
    if jp1448 {
        var t1449 Option__int = Option__int_Some{
            _0: low__347,
        }
        return t1449
    } else {
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(self__353 *_goml_vec_int, compare__354 func(int, int) int) Option__int {
    var t1468 bool
    var inline2883 int = vec_len__Vec_3int(self__353)
    var inline2884 bool = inline2883 == 0
    t1468 = inline2884
    if t1468 {
        return Option__int_None{}
    } else {
        var best__355 int = vec_get__Vec_3int(self__353, 0)
        var t1469 int
        var inline2881 int = vec_len__Vec_3int(self__353)
        t1469 = inline2881
        var t1470 FnIterator__int
        var inline2875 int = 1
        var inline2876 *ref_int_x = ref__Ref_3int(inline2875)
        var inline2877 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2876,
            end_1: t1469,
        }
        var inline2878 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2877)
        }
        var inline2879 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2878)
        t1470 = inline2879
        var for_iter326 FnIterator__int
        for_iter326 = t1470
        Loop_loop1473:
        for {
            var for_next327 Option__int
            var inline2871 func() Option__int = for_iter326.next_fn
            var inline2872 Option__int = inline2871()
            for_next327 = inline2872
            switch for_next327.(type) {
            case Option__int_None:
                break Loop_loop1473
            case Option__int_Some:
                var x328 int = for_next327.(Option__int_Some)._0
                var value__357 int = vec_get__Vec_3int(self__353, x328)
                var t1476 int = compare__354(value__357, best__355)
                var t1477 bool = t1476 < 0
                if t1477 {
                    best__355 = value__357
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1472 Option__int = Option__int_Some{
            _0: best__355,
        }
        return t1472
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(self__358 *_goml_vec_int, compare__359 func(int, int) int) Option__int {
    var t1482 bool
    var inline2898 int = vec_len__Vec_3int(self__358)
    var inline2899 bool = inline2898 == 0
    t1482 = inline2899
    if t1482 {
        return Option__int_None{}
    } else {
        var best__360 int = vec_get__Vec_3int(self__358, 0)
        var t1483 int
        var inline2896 int = vec_len__Vec_3int(self__358)
        t1483 = inline2896
        var t1484 FnIterator__int
        var inline2890 int = 1
        var inline2891 *ref_int_x = ref__Ref_3int(inline2890)
        var inline2892 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2891,
            end_1: t1483,
        }
        var inline2893 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2892)
        }
        var inline2894 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2893)
        t1484 = inline2894
        var for_iter331 FnIterator__int
        for_iter331 = t1484
        Loop_loop1487:
        for {
            var for_next332 Option__int
            var inline2886 func() Option__int = for_iter331.next_fn
            var inline2887 Option__int = inline2886()
            for_next332 = inline2887
            switch for_next332.(type) {
            case Option__int_None:
                break Loop_loop1487
            case Option__int_Some:
                var x333 int = for_next332.(Option__int_Some)._0
                var value__362 int = vec_get__Vec_3int(self__358, x333)
                var t1490 int = compare__359(value__362, best__360)
                var t1491 bool = t1490 > 0
                if t1491 {
                    best__360 = value__362
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1486 Option__int = Option__int_Some{
            _0: best__360,
        }
        return t1486
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var t1494 *_goml_vec_string = vec_new__Vec_6string()
    return t1494
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__258 *_goml_vec_string, elem__259 string) struct{} {
    vec_push__Vec_6string(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(self__337 *_goml_vec_string, compare__338 func(string, string) Ordering) struct{} {
    var t1501 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11{
        compare_0: compare__338,
    }
    var t1502 func(string, string) int = func(p0 string, p1 string) int {
        return _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(t1501, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__337, t1502)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t1506 int
    var inline2943 int = vec_len__Vec_6string(self__378)
    t1506 = inline2943
    var parts__380 *_goml_vec_string
    var inline2941 *_goml_vec_string = vec_with_capacity__Vec_6string(t1506)
    parts__380 = inline2941
    var t1507 int
    var inline2939 int = vec_len__Vec_6string(self__378)
    t1507 = inline2939
    var t1508 FnIterator__int
    var inline2933 int = 0
    var inline2934 *ref_int_x = ref__Ref_3int(inline2933)
    var inline2935 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2934,
        end_1: t1507,
    }
    var inline2936 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2935)
    }
    var inline2937 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2936)
    t1508 = inline2937
    var for_iter349 FnIterator__int
    for_iter349 = t1508
    Loop_loop1523:
    for {
        var for_next350 Option__int
        var inline2909 func() Option__int = for_iter349.next_fn
        var inline2910 Option__int = inline2909()
        for_next350 = inline2910
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop1523
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t1525 string = vec_get__Vec_6string(self__378, x351)
            var t1526 string
            t1526 = t1525
            vec_push__Vec_6string(parts__380, t1526)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1510 int
    var inline2930 int = vec_len__Vec_6string(parts__380)
    t1510 = inline2930
    var t1511 int = t1510 * 2
    var result__382 *_goml_vec_string
    var inline2928 *_goml_vec_string = vec_with_capacity__Vec_6string(t1511)
    result__382 = inline2928
    var t1512 int
    var inline2926 int = vec_len__Vec_6string(parts__380)
    t1512 = inline2926
    var t1513 FnIterator__int
    var inline2920 int = 0
    var inline2921 *ref_int_x = ref__Ref_3int(inline2920)
    var inline2922 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2921,
        end_1: t1512,
    }
    var inline2923 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2922)
    }
    var inline2924 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2923)
    t1513 = inline2924
    var for_iter353 FnIterator__int
    for_iter353 = t1513
    Loop_loop1516:
    for {
        var for_next354 Option__int
        var inline2916 func() Option__int = for_iter353.next_fn
        var inline2917 Option__int = inline2916()
        for_next354 = inline2917
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop1516
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t1521 bool = x355 > 0
            if t1521 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1519 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1519)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1515 string = _goml_runtime_core_string_concat(result__382)
    return t1515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___o_int_c_int_q_() *_goml_vec_Tuple2_3int_3int {
    var t1530 *_goml_vec_Tuple2_3int_3int = vec_new__Vec_16Tuple2_3int_3int()
    return t1530
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___o_int_c_int_q_(self__258 *_goml_vec_Tuple2_3int_3int, elem__259 Tuple2_3int_3int) struct{} {
    vec_push__Vec_16Tuple2_3int_3int(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T___o_int_c_int_q_(self__337 *_goml_vec_Tuple2_3int_3int, compare__338 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering) struct{} {
    var t1537 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12{
        compare_0: compare__338,
    }
    var t1538 func(Tuple2_3int_3int, Tuple2_3int_3int) int = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) int {
        return _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(t1537, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_int_c_int_q_(self__337, t1538)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_int_c_int_q_(self__273 *_goml_vec_Tuple2_3int_3int) int {
    var t1542 int = vec_len__Vec_16Tuple2_3int_3int(self__273)
    return t1542
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(self__390 []int, expected__391 int) bool {
    var index__392 int = 0
    Loop_loop1549:
    for {
        var t1550 int
        var inline2952 int = len(self__390)
        t1550 = inline2952
        var t1551 bool = index__392 < t1550
        if t1551 {
            var t1555 int = self__390[index__392]
            var t1556 bool
            var inline2950 bool = t1555 == expected__391
            t1556 = inline2950
            if t1556 {
                return true
            } else {
                var compound_old364 int = index__392
                var compound_value365 int = 1
                var t1553 int = compound_old364 + compound_value365
                index__392 = t1553
                continue
            }
        } else {
            break Loop_loop1549
        }
    }
    return false
}

func _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(self__393 FrozenVec__int, expected__394 int) bool {
    var index__395 int = 0
    Loop_loop1567:
    for {
        var t1568 int
        var inline2959 *_goml_vec_int = self__393.values
        var inline2960 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(inline2959)
        t1568 = inline2960
        var t1569 bool = index__395 < t1568
        if t1569 {
            var t1573 int
            var inline2956 *_goml_vec_int = self__393.values
            var inline2957 int = vec_get__Vec_3int(inline2956, index__395)
            t1573 = inline2957
            var t1574 bool
            var inline2954 bool = t1573 == expected__394
            t1574 = inline2954
            if t1574 {
                return true
            } else {
                var compound_old369 int = index__395
                var compound_value370 int = 1
                var t1571 int = compound_old369 + compound_value370
                index__395 = t1571
                continue
            }
        } else {
            break Loop_loop1567
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__273 *_goml_vec_int) int {
    var t1588 int = vec_len__Vec_3int(self__273)
    return t1588
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__322 *_goml_vec_int, compare__323 func(int, int) int) struct{} {
    var length__324 int
    var inline2993 int = vec_len__Vec_3int(self__322)
    length__324 = inline2993
    var t1656 bool = length__324 < 2
    if t1656 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_int
        var inline2991 *_goml_vec_int = vec_with_capacity__Vec_3int(length__324)
        buffer__325 = inline2991
        var t1597 FnIterator__int
        var inline2985 int = 0
        var inline2986 *ref_int_x = ref__Ref_3int(inline2985)
        var inline2987 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2986,
            end_1: length__324,
        }
        var inline2988 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2987)
        }
        var inline2989 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2988)
        t1597 = inline2989
        var for_iter262 FnIterator__int
        for_iter262 = t1597
        Loop_loop1652:
        for {
            var for_next263 Option__int
            var inline2967 func() Option__int = for_iter262.next_fn
            var inline2968 Option__int = inline2967()
            for_next263 = inline2968
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1652
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1654 int = vec_get__Vec_3int(self__322, x264)
                vec_push__Vec_3int(buffer__325, t1654)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1600:
        for {
            var t1601 bool = width__327 < length__324
            if t1601 {
                var left__328 int = 0
                Loop_loop1613:
                for {
                    var t1614 bool = left__328 < length__324
                    if t1614 {
                        var t1615 int = left__328 + width__327
                        var middle__329 int
                        var inline2972 bool = t1615 < length__324
                        if inline2972 {
                            middle__329 = t1615
                        } else {
                            middle__329 = length__324
                        }
                        var t1616 int = middle__329 + width__327
                        var right__330 int
                        var inline2970 bool = t1616 < length__324
                        if inline2970 {
                            right__330 = t1616
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1634:
                        for {
                            var t1650 bool = first__331 < middle__329
                            var jp1636 bool
                            if t1650 {
                                var t1651 bool = second__332 < right__330
                                jp1636 = t1651
                            } else {
                                jp1636 = false
                            }
                            if jp1636 {
                                var t1640 int = vec_get__Vec_3int(self__322, first__331)
                                var t1641 int = vec_get__Vec_3int(self__322, second__332)
                                var t1642 int = compare__323(t1640, t1641)
                                var t1643 bool = t1642 <= 0
                                if t1643 {
                                    var index267 int = output__333
                                    vec_get__Vec_3int(buffer__325, index267)
                                    var value269 int = vec_get__Vec_3int(self__322, first__331)
                                    vec_set__Vec_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1645 int = compound_old271 + compound_value272
                                    first__331 = t1645
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_3int(buffer__325, index275)
                                    var value277 int = vec_get__Vec_3int(self__322, second__332)
                                    vec_set__Vec_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1648 int = compound_old279 + compound_value280
                                    second__332 = t1648
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1638 int = compound_old283 + compound_value284
                                output__333 = t1638
                                continue
                            } else {
                                break Loop_loop1634
                            }
                        }
                        Loop_loop1627:
                        for {
                            var t1628 bool = first__331 < middle__329
                            if t1628 {
                                var index288 int = output__333
                                vec_get__Vec_3int(buffer__325, index288)
                                var value290 int = vec_get__Vec_3int(self__322, first__331)
                                vec_set__Vec_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1630 int = compound_old292 + compound_value293
                                first__331 = t1630
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1632 int = compound_old295 + compound_value296
                                output__333 = t1632
                                continue
                            } else {
                                break Loop_loop1627
                            }
                        }
                        Loop_loop1620:
                        for {
                            var t1621 bool = second__332 < right__330
                            if t1621 {
                                var index300 int = output__333
                                vec_get__Vec_3int(buffer__325, index300)
                                var value302 int = vec_get__Vec_3int(self__322, second__332)
                                vec_set__Vec_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1623 int = compound_old304 + compound_value305
                                second__332 = t1623
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1625 int = compound_old307 + compound_value308
                                output__333 = t1625
                                continue
                            } else {
                                break Loop_loop1620
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1613
                    }
                }
                var t1603 FnIterator__int
                var inline2978 int = 0
                var inline2979 *ref_int_x = ref__Ref_3int(inline2978)
                var inline2980 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline2979,
                    end_1: length__324,
                }
                var inline2981 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2980)
                }
                var inline2982 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2981)
                t1603 = inline2982
                var for_iter313 FnIterator__int
                for_iter313 = t1603
                Loop_loop1610:
                for {
                    var for_next314 Option__int
                    var inline2974 func() Option__int = for_iter313.next_fn
                    var inline2975 Option__int = inline2974()
                    for_next314 = inline2975
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1610
                    case Option__int_Some:
                        var x315 int = for_next314.(Option__int_Some)._0
                        vec_get__Vec_3int(self__322, x315)
                        var value319 int = vec_get__Vec_3int(buffer__325, x315)
                        vec_set__Vec_3int(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1607 int = length__324 / 2
                var t1608 bool = width__327 > t1607
                var jp1606 int
                if t1608 {
                    jp1606 = length__324
                } else {
                    var t1609 int = width__327 * 2
                    jp1606 = t1609
                }
                width__327 = jp1606
                continue
            } else {
                break Loop_loop1600
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__371 *_goml_vec_int, equal__372 func(int, int) bool) struct{} {
    var t1685 int
    var inline3009 int = vec_len__Vec_3int(self__371)
    t1685 = inline3009
    var t1686 bool = t1685 < 2
    if t1686 {
        return struct{}{}
    } else {
        var output__373 int = 1
        var t1671 int
        var inline3007 int = vec_len__Vec_3int(self__371)
        t1671 = inline3007
        var t1672 FnIterator__int
        var inline3001 int = 1
        var inline3002 *ref_int_x = ref__Ref_3int(inline3001)
        var inline3003 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3002,
            end_1: t1671,
        }
        var inline3004 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3003)
        }
        var inline3005 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3004)
        t1672 = inline3005
        var for_iter337 FnIterator__int
        for_iter337 = t1672
        Loop_loop1675:
        for {
            var for_next338 Option__int
            var inline2995 func() Option__int = for_iter337.next_fn
            var inline2996 Option__int = inline2995()
            for_next338 = inline2996
            switch for_next338.(type) {
            case Option__int_None:
                break Loop_loop1675
            case Option__int_Some:
                var x339 int = for_next338.(Option__int_Some)._0
                var value__375 int = vec_get__Vec_3int(self__371, x339)
                var t1678 int = output__373 - 1
                var t1679 int = vec_get__Vec_3int(self__371, t1678)
                var t1680 bool = equal__372(t1679, value__375)
                var t1681 bool = !t1680
                if t1681 {
                    var index341 int = output__373
                    vec_get__Vec_3int(self__371, index341)
                    vec_set__Vec_3int(self__371, index341, value__375)
                    var compound_old345 int = output__373
                    var compound_value346 int = 1
                    var t1683 int = compound_old345 + compound_value346
                    output__373 = t1683
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        vec_truncate__Vec_3int(self__371, output__373)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__264 *_goml_vec_int) *_goml_vec_int {
    var t1704 int
    var inline3017 int = vec_len__Vec_3int(self__264)
    t1704 = inline3017
    var result__265 *_goml_vec_int
    var inline3015 *_goml_vec_int = vec_with_capacity__Vec_3int(t1704)
    result__265 = inline3015
    var index__266 int = 0
    Loop_loop1706:
    for {
        var t1707 int
        var inline3013 int = vec_len__Vec_3int(self__264)
        t1707 = inline3013
        var t1708 bool = index__266 < t1707
        if t1708 {
            var t1709 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t1709)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t1710 int = compound_old196 + compound_value197
            index__266 = t1710
            continue
        } else {
            break Loop_loop1706
        }
    }
    return result__265
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__int(self__456 Option__int) bool {
    switch self__456.(type) {
    case Option__int_None:
        return false
    case Option__int_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t1726 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t1726
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__322 *_goml_vec_string, compare__323 func(string, string) int) struct{} {
    var length__324 int
    var inline3049 int = vec_len__Vec_6string(self__322)
    length__324 = inline3049
    var t1798 bool = length__324 < 2
    if t1798 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_string
        var inline3047 *_goml_vec_string = vec_with_capacity__Vec_6string(length__324)
        buffer__325 = inline3047
        var t1739 FnIterator__int
        var inline3041 int = 0
        var inline3042 *ref_int_x = ref__Ref_3int(inline3041)
        var inline3043 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3042,
            end_1: length__324,
        }
        var inline3044 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3043)
        }
        var inline3045 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3044)
        t1739 = inline3045
        var for_iter262 FnIterator__int
        for_iter262 = t1739
        Loop_loop1794:
        for {
            var for_next263 Option__int
            var inline3023 func() Option__int = for_iter262.next_fn
            var inline3024 Option__int = inline3023()
            for_next263 = inline3024
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1794
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1796 string = vec_get__Vec_6string(self__322, x264)
                vec_push__Vec_6string(buffer__325, t1796)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1742:
        for {
            var t1743 bool = width__327 < length__324
            if t1743 {
                var left__328 int = 0
                Loop_loop1755:
                for {
                    var t1756 bool = left__328 < length__324
                    if t1756 {
                        var t1757 int = left__328 + width__327
                        var middle__329 int
                        var inline3028 bool = t1757 < length__324
                        if inline3028 {
                            middle__329 = t1757
                        } else {
                            middle__329 = length__324
                        }
                        var t1758 int = middle__329 + width__327
                        var right__330 int
                        var inline3026 bool = t1758 < length__324
                        if inline3026 {
                            right__330 = t1758
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1776:
                        for {
                            var t1792 bool = first__331 < middle__329
                            var jp1778 bool
                            if t1792 {
                                var t1793 bool = second__332 < right__330
                                jp1778 = t1793
                            } else {
                                jp1778 = false
                            }
                            if jp1778 {
                                var t1782 string = vec_get__Vec_6string(self__322, first__331)
                                var t1783 string = vec_get__Vec_6string(self__322, second__332)
                                var t1784 int = compare__323(t1782, t1783)
                                var t1785 bool = t1784 <= 0
                                if t1785 {
                                    var index267 int = output__333
                                    vec_get__Vec_6string(buffer__325, index267)
                                    var value269 string = vec_get__Vec_6string(self__322, first__331)
                                    vec_set__Vec_6string(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1787 int = compound_old271 + compound_value272
                                    first__331 = t1787
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_6string(buffer__325, index275)
                                    var value277 string = vec_get__Vec_6string(self__322, second__332)
                                    vec_set__Vec_6string(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1790 int = compound_old279 + compound_value280
                                    second__332 = t1790
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1780 int = compound_old283 + compound_value284
                                output__333 = t1780
                                continue
                            } else {
                                break Loop_loop1776
                            }
                        }
                        Loop_loop1769:
                        for {
                            var t1770 bool = first__331 < middle__329
                            if t1770 {
                                var index288 int = output__333
                                vec_get__Vec_6string(buffer__325, index288)
                                var value290 string = vec_get__Vec_6string(self__322, first__331)
                                vec_set__Vec_6string(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1772 int = compound_old292 + compound_value293
                                first__331 = t1772
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1774 int = compound_old295 + compound_value296
                                output__333 = t1774
                                continue
                            } else {
                                break Loop_loop1769
                            }
                        }
                        Loop_loop1762:
                        for {
                            var t1763 bool = second__332 < right__330
                            if t1763 {
                                var index300 int = output__333
                                vec_get__Vec_6string(buffer__325, index300)
                                var value302 string = vec_get__Vec_6string(self__322, second__332)
                                vec_set__Vec_6string(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1765 int = compound_old304 + compound_value305
                                second__332 = t1765
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1767 int = compound_old307 + compound_value308
                                output__333 = t1767
                                continue
                            } else {
                                break Loop_loop1762
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1755
                    }
                }
                var t1745 FnIterator__int
                var inline3034 int = 0
                var inline3035 *ref_int_x = ref__Ref_3int(inline3034)
                var inline3036 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3035,
                    end_1: length__324,
                }
                var inline3037 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3036)
                }
                var inline3038 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3037)
                t1745 = inline3038
                var for_iter313 FnIterator__int
                for_iter313 = t1745
                Loop_loop1752:
                for {
                    var for_next314 Option__int
                    var inline3030 func() Option__int = for_iter313.next_fn
                    var inline3031 Option__int = inline3030()
                    for_next314 = inline3031
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1752
                    case Option__int_Some:
                        var x315 int = for_next314.(Option__int_Some)._0
                        vec_get__Vec_6string(self__322, x315)
                        var value319 string = vec_get__Vec_6string(buffer__325, x315)
                        vec_set__Vec_6string(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1749 int = length__324 / 2
                var t1750 bool = width__327 > t1749
                var jp1748 int
                if t1750 {
                    jp1748 = length__324
                } else {
                    var t1751 int = width__327 * 2
                    jp1748 = t1751
                }
                width__327 = jp1748
                continue
            } else {
                break Loop_loop1742
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_int_c_int_q_(self__322 *_goml_vec_Tuple2_3int_3int, compare__323 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    var length__324 int
    var inline3079 int = vec_len__Vec_16Tuple2_3int_3int(self__322)
    length__324 = inline3079
    var t1860 bool = length__324 < 2
    if t1860 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_Tuple2_3int_3int
        var inline3077 *_goml_vec_Tuple2_3int_3int = vec_with_capacity__Vec_16Tuple2_3int_3int(length__324)
        buffer__325 = inline3077
        var t1801 FnIterator__int
        var inline3071 int = 0
        var inline3072 *ref_int_x = ref__Ref_3int(inline3071)
        var inline3073 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3072,
            end_1: length__324,
        }
        var inline3074 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3073)
        }
        var inline3075 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3074)
        t1801 = inline3075
        var for_iter262 FnIterator__int
        for_iter262 = t1801
        Loop_loop1856:
        for {
            var for_next263 Option__int
            var inline3053 func() Option__int = for_iter262.next_fn
            var inline3054 Option__int = inline3053()
            for_next263 = inline3054
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1856
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1858 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, x264)
                vec_push__Vec_16Tuple2_3int_3int(buffer__325, t1858)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1804:
        for {
            var t1805 bool = width__327 < length__324
            if t1805 {
                var left__328 int = 0
                Loop_loop1817:
                for {
                    var t1818 bool = left__328 < length__324
                    if t1818 {
                        var t1819 int = left__328 + width__327
                        var middle__329 int
                        var inline3058 bool = t1819 < length__324
                        if inline3058 {
                            middle__329 = t1819
                        } else {
                            middle__329 = length__324
                        }
                        var t1820 int = middle__329 + width__327
                        var right__330 int
                        var inline3056 bool = t1820 < length__324
                        if inline3056 {
                            right__330 = t1820
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1838:
                        for {
                            var t1854 bool = first__331 < middle__329
                            var jp1840 bool
                            if t1854 {
                                var t1855 bool = second__332 < right__330
                                jp1840 = t1855
                            } else {
                                jp1840 = false
                            }
                            if jp1840 {
                                var t1844 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                var t1845 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                var t1846 int = compare__323(t1844, t1845)
                                var t1847 bool = t1846 <= 0
                                if t1847 {
                                    var index267 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index267)
                                    var value269 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1849 int = compound_old271 + compound_value272
                                    first__331 = t1849
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index275)
                                    var value277 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1852 int = compound_old279 + compound_value280
                                    second__332 = t1852
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1842 int = compound_old283 + compound_value284
                                output__333 = t1842
                                continue
                            } else {
                                break Loop_loop1838
                            }
                        }
                        Loop_loop1831:
                        for {
                            var t1832 bool = first__331 < middle__329
                            if t1832 {
                                var index288 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index288)
                                var value290 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1834 int = compound_old292 + compound_value293
                                first__331 = t1834
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1836 int = compound_old295 + compound_value296
                                output__333 = t1836
                                continue
                            } else {
                                break Loop_loop1831
                            }
                        }
                        Loop_loop1824:
                        for {
                            var t1825 bool = second__332 < right__330
                            if t1825 {
                                var index300 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index300)
                                var value302 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1827 int = compound_old304 + compound_value305
                                second__332 = t1827
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1829 int = compound_old307 + compound_value308
                                output__333 = t1829
                                continue
                            } else {
                                break Loop_loop1824
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1817
                    }
                }
                var t1807 FnIterator__int
                var inline3064 int = 0
                var inline3065 *ref_int_x = ref__Ref_3int(inline3064)
                var inline3066 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3065,
                    end_1: length__324,
                }
                var inline3067 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3066)
                }
                var inline3068 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3067)
                t1807 = inline3068
                var for_iter313 FnIterator__int
                for_iter313 = t1807
                Loop_loop1814:
                for {
                    var for_next314 Option__int
                    var inline3060 func() Option__int = for_iter313.next_fn
                    var inline3061 Option__int = inline3060()
                    for_next314 = inline3061
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1814
                    case Option__int_Some:
                        var x315 int = for_next314.(Option__int_Some)._0
                        vec_get__Vec_16Tuple2_3int_3int(self__322, x315)
                        var value319 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(buffer__325, x315)
                        vec_set__Vec_16Tuple2_3int_3int(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1811 int = length__324 / 2
                var t1812 bool = width__327 > t1811
                var jp1810 int
                if t1812 {
                    jp1810 = length__324
                } else {
                    var t1813 int = width__327 * 2
                    jp1810 = t1813
                }
                width__327 = jp1810
                continue
            } else {
                break Loop_loop1804
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env451 closure_env_main_0, value__1 int) bool {
    var t1869 bool = value__1 == 5
    return t1869
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env452 closure_env_main_1, value__2 int) bool {
    var t1872 bool = value__2 == 99
    return t1872
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env453 closure_env_main_2, left__3 int, right__4 int) int {
    var t1875 int = left__3 - right__4
    return t1875
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env454 closure_env_main_3, value__6 int) int {
    var t1878 int = value__6 - 4
    return t1878
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env455 closure_env_main_4, value__7 int) int {
    var t1881 int = value__7 - 3
    return t1881
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env456 closure_env_main_5, left__8 int, right__9 int) int {
    var t1884 int = left__8 - right__9
    return t1884
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env457 closure_env_main_6, left__10 int, right__11 int) int {
    var t1887 int = left__10 - right__11
    return t1887
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env458 closure_env_main_7, left__13 string, right__14 string) Ordering {
    var inline3081 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(left__13, right__14)
    return inline3081
}

func _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(env459 closure_env_main_8, left__16 Tuple2_3int_3int, right__17 Tuple2_3int_3int) Ordering {
    var t1893 int = left__16._0
    var t1894 int = right__17._0
    var inline3083 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(t1893, t1894)
    return inline3083
}

func _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(env460 closure_env_main_9, left__21 int, right__22 int) int {
    var t1898 int = left__21 - right__22
    return t1898
}

func _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(env461 closure_env_inherent_Vec_Vec_T_dedup_T_int_10, left__388 int, right__389 int) bool {
    var inline3085 bool = left__388 == right__389
    return inline3085
}

func _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(env462 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11, left__339 string, right__340 string) int {
    var compare__338 func(string, string) Ordering = env462.compare_0
    var t1904 Ordering = compare__338(left__339, right__340)
    switch t1904 {
    case Less:
        return -1
    case Equal:
        return 0
    case Greater:
        return 1
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(env463 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12, left__339 Tuple2_3int_3int, right__340 Tuple2_3int_3int) int {
    var compare__338 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = env463.compare_0
    var t1908 Ordering = compare__338(left__339, right__340)
    switch t1908 {
    case Less:
        return -1
    case Equal:
        return 0
    case Greater:
        return 1
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(env464 closure_env_goml_builtin_range_13) Option__int {
    var current__496 *ref_int_x = env464.current_0
    var end__495 int = env464.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t1914 bool = value__497 < end__495
    if t1914 {
        var t1915 int = value__497 + 1
        ref_set__Ref_3int(current__496, t1915)
        var t1916 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t1916
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
