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
    var t522 bool = self__24 < other__25
    if t522 {
        return Less
    } else {
        var t525 bool = self__24 > other__25
        if t525 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__32 int, other__33 int) Ordering {
    var t580 bool = self__32 < other__33
    if t580 {
        return Less
    } else {
        var t583 bool = self__32 > other__33
        if t583 {
            return Greater
        } else {
            return Equal
        }
    }
}

func main0() struct{} {
    var t919 [8]int = [8]int{3, 1, 4, 1, 5, 9, 2, 6}
    var values__0 *_goml_vec_int = func(values [8]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t919)
    var t920 closure_env_main_0 = closure_env_main_0{}
    var t921 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t920, p0)
    }
    var t922 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(values__0, t921)
    var t923 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t922, -1)
    var t924 string = _goml_m_inherent_i_int_i_int_i_to__string(t923)
    println__T_string(t924)
    var t925 closure_env_main_1 = closure_env_main_1{}
    var t926 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t925, p0)
    }
    var t927 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(values__0, t926)
    var t928 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t927, -1)
    var t929 string = _goml_m_inherent_i_int_i_int_i_to__string(t928)
    println__T_string(t929)
    var t930 closure_env_main_2 = closure_env_main_2{}
    var t931 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t930, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(values__0, t931)
    var t932 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(values__0, ",")
    println__T_string(t932)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(values__0)
    var t933 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(values__0, ",")
    println__T_string(t933)
    var t934 [5]int = [5]int{1, 2, 4, 4, 5}
    var ordered__5 *_goml_vec_int = func(values [5]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t934)
    var t935 closure_env_main_3 = closure_env_main_3{}
    var t936 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t935, p0)
    }
    var t937 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(ordered__5, t936)
    var t938 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t937, -1)
    var t939 string = _goml_m_inherent_i_int_i_int_i_to__string(t938)
    println__T_string(t939)
    var t940 closure_env_main_4 = closure_env_main_4{}
    var t941 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t940, p0)
    }
    var t942 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(ordered__5, t941)
    var t943 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t942, -1)
    var t944 string = _goml_m_inherent_i_int_i_int_i_to__string(t943)
    println__T_string(t944)
    var t945 closure_env_main_5 = closure_env_main_5{}
    var t946 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t945, p0, p1)
    }
    var t947 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(ordered__5, t946)
    var t948 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t947, 0)
    var t949 string = _goml_m_inherent_i_int_i_int_i_to__string(t948)
    println__T_string(t949)
    var t950 closure_env_main_6 = closure_env_main_6{}
    var t951 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t950, p0, p1)
    }
    var t952 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(ordered__5, t951)
    var t953 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t952, 0)
    var t954 string = _goml_m_inherent_i_int_i_int_i_to__string(t953)
    println__T_string(t954)
    var t955 [3]string = [3]string{"beta", "alpha", "gamma"}
    var names__12 *_goml_vec_string = func(values [3]string) *_goml_vec_string {
        return &_goml_vec_string{
            items: values[0:len(values)],
        }
    }(t955)
    var t956 closure_env_main_7 = closure_env_main_7{}
    var t957 func(string, string) Ordering = func(p0 string, p1 string) Ordering {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t956, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(names__12, t957)
    var t958 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(names__12, "|")
    println__T_string(t958)
    var t959 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t960 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 1,
    }
    var t961 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 0,
        _1: 9,
    }
    var t962 [3]Tuple2_3int_3int = [3]Tuple2_3int_3int{t959, t960, t961}
    var pairs__15 *_goml_vec_Tuple2_3int_3int = func(values [3]Tuple2_3int_3int) *_goml_vec_Tuple2_3int_3int {
        return &_goml_vec_Tuple2_3int_3int{
            items: values[0:len(values)],
        }
    }(t962)
    var t963 closure_env_main_8 = closure_env_main_8{}
    var t964 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) Ordering {
        return _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(t963, p0, p1)
    }
    var inline2317 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12{
        compare_0: t964,
    }
    var inline2318 func(Tuple2_3int_3int, Tuple2_3int_3int) int = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) int {
        return _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(inline2317, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T___o_int_c_int_q_(pairs__15, inline2318)
    var t965 int
    var inline2315 int = vec_len__Vec_16Tuple2_3int_3int(pairs__15)
    t965 = inline2315
    var t966 string
    var inline2313 string = _goml_runtime_core_int_to_string(t965)
    t966 = inline2313
    var inline2310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t966)
    _goml_runtime_core_string_println(inline2310)
    var t967 [3]int = [3]int{7, 8, 9}
    var t968 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t967)
    var view__18 []int
    var inline2306 int = 0
    var inline2307 int = 2
    var inline2308 []int = t968.items[inline2306:inline2307]
    view__18 = inline2308
    var t969 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 8)
    var t970 string
    var inline2304 string = _goml_runtime_core_bool_to_string(t969)
    t970 = inline2304
    var inline2301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t970)
    _goml_runtime_core_string_println(inline2301)
    var t971 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 9)
    var t972 string
    var inline2299 string = _goml_runtime_core_bool_to_string(t971)
    t972 = inline2299
    var inline2296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t972)
    _goml_runtime_core_string_println(inline2296)
    var t973 [2]int = [2]int{1, 2}
    var t974 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t973)
    var frozen__19 FrozenVec__int
    var inline2293 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(t974)
    var inline2294 FrozenVec__int = FrozenVec__int{
        values: inline2293,
    }
    frozen__19 = inline2294
    var t975 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 1)
    var t976 string
    var inline2291 string = _goml_runtime_core_bool_to_string(t975)
    t976 = inline2291
    var inline2288 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t976)
    _goml_runtime_core_string_println(inline2288)
    var t977 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 3)
    var t978 string
    var inline2286 string = _goml_runtime_core_bool_to_string(t977)
    t978 = inline2286
    var inline2283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t978)
    _goml_runtime_core_string_println(inline2283)
    var t979 [0]int = [0]int{}
    var empty__20 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t979)
    var t980 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(empty__20, ",")
    var inline2280 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t980)
    _goml_runtime_core_string_println(inline2280)
    var t981 closure_env_main_9 = closure_env_main_9{}
    var t982 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(t981, p0, p1)
    }
    var t983 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(empty__20, t982)
    var t984 bool
    var inline2277 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__int(t983)
    var inline2278 bool = !inline2277
    t984 = inline2278
    var t985 string
    var inline2275 string = _goml_runtime_core_bool_to_string(t984)
    t985 = inline2275
    var inline2272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t985)
    _goml_runtime_core_string_println(inline2272)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t1372 string
    t1372 = value__1
    _goml_runtime_core_string_println(t1372)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(self__319 *_goml_vec_int, predicate__320 func(int) bool) Option__int {
    var t1376 int
    var inline2796 int = vec_len__Vec_3int(self__319)
    t1376 = inline2796
    var t1377 FnIterator__int
    var inline2790 int = 0
    var inline2791 *ref_int_x = ref__Ref_3int(inline2790)
    var inline2792 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2791,
        end_1: t1376,
    }
    var inline2793 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2792)
    }
    var inline2794 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2793)
    t1377 = inline2794
    var for_iter257 FnIterator__int
    for_iter257 = t1377
    Loop_loop1379:
    for {
        var for_next258 Option__int
        var inline2786 func() Option__int = for_iter257.next_fn
        var inline2787 Option__int = inline2786()
        for_next258 = inline2787
        switch for_next258.(type) {
        case Option__int_None:
            break Loop_loop1379
        case Option__int_Some:
            var x259 int = for_next258.(Option__int_Some)._0
            var t1382 int = vec_get__Vec_3int(self__319, x259)
            var t1383 bool = predicate__320(t1382)
            if t1383 {
                var t1384 Option__int = Option__int_Some{
                    _0: x259,
                }
                return t1384
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
    var t1391 string = _goml_runtime_core_int_to_string(self__32)
    return t1391
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(self__335 *_goml_vec_int, compare__336 func(int, int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__335, compare__336)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(self__378 *_goml_vec_int, separator__379 string) string {
    var t1396 int
    var inline2836 int = vec_len__Vec_3int(self__378)
    t1396 = inline2836
    var parts__380 *_goml_vec_string
    var inline2834 *_goml_vec_string = vec_with_capacity__Vec_6string(t1396)
    parts__380 = inline2834
    var t1397 int
    var inline2832 int = vec_len__Vec_3int(self__378)
    t1397 = inline2832
    var t1398 FnIterator__int
    var inline2826 int = 0
    var inline2827 *ref_int_x = ref__Ref_3int(inline2826)
    var inline2828 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2827,
        end_1: t1397,
    }
    var inline2829 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2828)
    }
    var inline2830 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2829)
    t1398 = inline2830
    var for_iter349 FnIterator__int
    for_iter349 = t1398
    Loop_loop1413:
    for {
        var for_next350 Option__int
        var inline2802 func() Option__int = for_iter349.next_fn
        var inline2803 Option__int = inline2802()
        for_next350 = inline2803
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop1413
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t1415 int = vec_get__Vec_3int(self__378, x351)
            var t1416 string
            var inline2800 string = _goml_runtime_core_int_to_string(t1415)
            t1416 = inline2800
            vec_push__Vec_6string(parts__380, t1416)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1400 int
    var inline2823 int = vec_len__Vec_6string(parts__380)
    t1400 = inline2823
    var t1401 int = t1400 * 2
    var result__382 *_goml_vec_string
    var inline2821 *_goml_vec_string = vec_with_capacity__Vec_6string(t1401)
    result__382 = inline2821
    var t1402 int
    var inline2819 int = vec_len__Vec_6string(parts__380)
    t1402 = inline2819
    var t1403 FnIterator__int
    var inline2813 int = 0
    var inline2814 *ref_int_x = ref__Ref_3int(inline2813)
    var inline2815 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2814,
        end_1: t1402,
    }
    var inline2816 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2815)
    }
    var inline2817 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2816)
    t1403 = inline2817
    var for_iter353 FnIterator__int
    for_iter353 = t1403
    Loop_loop1406:
    for {
        var for_next354 Option__int
        var inline2809 func() Option__int = for_iter353.next_fn
        var inline2810 Option__int = inline2809()
        for_next354 = inline2810
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop1406
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t1411 bool = x355 > 0
            if t1411 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1409 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1409)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1405 string = _goml_runtime_core_string_concat(result__382)
    return t1405
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(self__387 *_goml_vec_int) struct{} {
    var t1419 closure_env_inherent_Vec_Vec_T_dedup_T_int_10 = closure_env_inherent_Vec_Vec_T_dedup_T_int_10{}
    var t1420 func(int, int) bool = func(p0 int, p1 int) bool {
        return _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(t1419, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__387, t1420)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(self__345 *_goml_vec_int, compare__346 func(int) int) Option__int {
    var low__347 int = 0
    var high__348 int
    var inline2840 int = vec_len__Vec_3int(self__345)
    high__348 = inline2840
    Loop_loop1435:
    for {
        var t1436 bool = low__347 < high__348
        if t1436 {
            var t1437 int = high__348 - low__347
            var t1438 int = t1437 / 2
            var middle__349 int = low__347 + t1438
            var t1440 int = vec_get__Vec_3int(self__345, middle__349)
            var t1441 int = compare__346(t1440)
            var t1442 bool = t1441 < 0
            if t1442 {
                var t1443 int = middle__349 + 1
                low__347 = t1443
                continue
            } else {
                high__348 = middle__349
                continue
            }
        } else {
            break Loop_loop1435
        }
    }
    var t1430 int
    var inline2838 int = vec_len__Vec_3int(self__345)
    t1430 = inline2838
    var t1431 bool = low__347 < t1430
    var jp1428 bool
    if t1431 {
        var t1432 int = vec_get__Vec_3int(self__345, low__347)
        var t1433 int = compare__346(t1432)
        var t1434 bool = t1433 == 0
        jp1428 = t1434
    } else {
        jp1428 = false
    }
    if jp1428 {
        var t1429 Option__int = Option__int_Some{
            _0: low__347,
        }
        return t1429
    } else {
        return Option__int_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(self__353 *_goml_vec_int, compare__354 func(int, int) int) Option__int {
    var t1448 bool
    var inline2854 int = vec_len__Vec_3int(self__353)
    var inline2855 bool = inline2854 == 0
    t1448 = inline2855
    if t1448 {
        return Option__int_None{}
    } else {
        var best__355 int = vec_get__Vec_3int(self__353, 0)
        var t1449 int
        var inline2852 int = vec_len__Vec_3int(self__353)
        t1449 = inline2852
        var t1450 FnIterator__int
        var inline2846 int = 1
        var inline2847 *ref_int_x = ref__Ref_3int(inline2846)
        var inline2848 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2847,
            end_1: t1449,
        }
        var inline2849 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2848)
        }
        var inline2850 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2849)
        t1450 = inline2850
        var for_iter326 FnIterator__int
        for_iter326 = t1450
        Loop_loop1453:
        for {
            var for_next327 Option__int
            var inline2842 func() Option__int = for_iter326.next_fn
            var inline2843 Option__int = inline2842()
            for_next327 = inline2843
            switch for_next327.(type) {
            case Option__int_None:
                break Loop_loop1453
            case Option__int_Some:
                var x328 int = for_next327.(Option__int_Some)._0
                var value__357 int = vec_get__Vec_3int(self__353, x328)
                var t1456 int = compare__354(value__357, best__355)
                var t1457 bool = t1456 < 0
                if t1457 {
                    best__355 = value__357
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1452 Option__int = Option__int_Some{
            _0: best__355,
        }
        return t1452
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(self__358 *_goml_vec_int, compare__359 func(int, int) int) Option__int {
    var t1462 bool
    var inline2869 int = vec_len__Vec_3int(self__358)
    var inline2870 bool = inline2869 == 0
    t1462 = inline2870
    if t1462 {
        return Option__int_None{}
    } else {
        var best__360 int = vec_get__Vec_3int(self__358, 0)
        var t1463 int
        var inline2867 int = vec_len__Vec_3int(self__358)
        t1463 = inline2867
        var t1464 FnIterator__int
        var inline2861 int = 1
        var inline2862 *ref_int_x = ref__Ref_3int(inline2861)
        var inline2863 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2862,
            end_1: t1463,
        }
        var inline2864 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2863)
        }
        var inline2865 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2864)
        t1464 = inline2865
        var for_iter331 FnIterator__int
        for_iter331 = t1464
        Loop_loop1467:
        for {
            var for_next332 Option__int
            var inline2857 func() Option__int = for_iter331.next_fn
            var inline2858 Option__int = inline2857()
            for_next332 = inline2858
            switch for_next332.(type) {
            case Option__int_None:
                break Loop_loop1467
            case Option__int_Some:
                var x333 int = for_next332.(Option__int_Some)._0
                var value__362 int = vec_get__Vec_3int(self__358, x333)
                var t1470 int = compare__359(value__362, best__360)
                var t1471 bool = t1470 > 0
                if t1471 {
                    best__360 = value__362
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1466 Option__int = Option__int_Some{
            _0: best__360,
        }
        return t1466
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(self__337 *_goml_vec_string, compare__338 func(string, string) Ordering) struct{} {
    var t1476 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11{
        compare_0: compare__338,
    }
    var t1477 func(string, string) int = func(p0 string, p1 string) int {
        return _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(t1476, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__337, t1477)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t1481 int
    var inline2914 int = vec_len__Vec_6string(self__378)
    t1481 = inline2914
    var parts__380 *_goml_vec_string
    var inline2912 *_goml_vec_string = vec_with_capacity__Vec_6string(t1481)
    parts__380 = inline2912
    var t1482 int
    var inline2910 int = vec_len__Vec_6string(self__378)
    t1482 = inline2910
    var t1483 FnIterator__int
    var inline2904 int = 0
    var inline2905 *ref_int_x = ref__Ref_3int(inline2904)
    var inline2906 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2905,
        end_1: t1482,
    }
    var inline2907 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2906)
    }
    var inline2908 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2907)
    t1483 = inline2908
    var for_iter349 FnIterator__int
    for_iter349 = t1483
    Loop_loop1498:
    for {
        var for_next350 Option__int
        var inline2880 func() Option__int = for_iter349.next_fn
        var inline2881 Option__int = inline2880()
        for_next350 = inline2881
        switch for_next350.(type) {
        case Option__int_None:
            break Loop_loop1498
        case Option__int_Some:
            var x351 int = for_next350.(Option__int_Some)._0
            var t1500 string = vec_get__Vec_6string(self__378, x351)
            var t1501 string
            t1501 = t1500
            vec_push__Vec_6string(parts__380, t1501)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1485 int
    var inline2901 int = vec_len__Vec_6string(parts__380)
    t1485 = inline2901
    var t1486 int = t1485 * 2
    var result__382 *_goml_vec_string
    var inline2899 *_goml_vec_string = vec_with_capacity__Vec_6string(t1486)
    result__382 = inline2899
    var t1487 int
    var inline2897 int = vec_len__Vec_6string(parts__380)
    t1487 = inline2897
    var t1488 FnIterator__int
    var inline2891 int = 0
    var inline2892 *ref_int_x = ref__Ref_3int(inline2891)
    var inline2893 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2892,
        end_1: t1487,
    }
    var inline2894 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2893)
    }
    var inline2895 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2894)
    t1488 = inline2895
    var for_iter353 FnIterator__int
    for_iter353 = t1488
    Loop_loop1491:
    for {
        var for_next354 Option__int
        var inline2887 func() Option__int = for_iter353.next_fn
        var inline2888 Option__int = inline2887()
        for_next354 = inline2888
        switch for_next354.(type) {
        case Option__int_None:
            break Loop_loop1491
        case Option__int_Some:
            var x355 int = for_next354.(Option__int_Some)._0
            var t1496 bool = x355 > 0
            if t1496 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1494 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1494)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1490 string = _goml_runtime_core_string_concat(result__382)
    return t1490
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(self__390 []int, expected__391 int) bool {
    var index__392 int = 0
    Loop_loop1519:
    for {
        var t1520 int
        var inline2923 int = len(self__390)
        t1520 = inline2923
        var t1521 bool = index__392 < t1520
        if t1521 {
            var t1525 int = self__390[index__392]
            var t1526 bool
            var inline2921 bool = t1525 == expected__391
            t1526 = inline2921
            if t1526 {
                return true
            } else {
                var compound_old364 int = index__392
                var compound_value365 int = 1
                var t1523 int = compound_old364 + compound_value365
                index__392 = t1523
                continue
            }
        } else {
            break Loop_loop1519
        }
    }
    return false
}

func _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(self__393 FrozenVec__int, expected__394 int) bool {
    var index__395 int = 0
    Loop_loop1537:
    for {
        var t1538 int
        var inline2930 *_goml_vec_int = self__393.values
        var inline2931 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(inline2930)
        t1538 = inline2931
        var t1539 bool = index__395 < t1538
        if t1539 {
            var t1543 int
            var inline2927 *_goml_vec_int = self__393.values
            var inline2928 int = vec_get__Vec_3int(inline2927, index__395)
            t1543 = inline2928
            var t1544 bool
            var inline2925 bool = t1543 == expected__394
            t1544 = inline2925
            if t1544 {
                return true
            } else {
                var compound_old369 int = index__395
                var compound_value370 int = 1
                var t1541 int = compound_old369 + compound_value370
                index__395 = t1541
                continue
            }
        } else {
            break Loop_loop1537
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__273 *_goml_vec_int) int {
    var t1558 int = vec_len__Vec_3int(self__273)
    return t1558
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__322 *_goml_vec_int, compare__323 func(int, int) int) struct{} {
    var length__324 int
    var inline2964 int = vec_len__Vec_3int(self__322)
    length__324 = inline2964
    var t1626 bool = length__324 < 2
    if t1626 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_int
        var inline2962 *_goml_vec_int = vec_with_capacity__Vec_3int(length__324)
        buffer__325 = inline2962
        var t1567 FnIterator__int
        var inline2956 int = 0
        var inline2957 *ref_int_x = ref__Ref_3int(inline2956)
        var inline2958 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2957,
            end_1: length__324,
        }
        var inline2959 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2958)
        }
        var inline2960 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2959)
        t1567 = inline2960
        var for_iter262 FnIterator__int
        for_iter262 = t1567
        Loop_loop1622:
        for {
            var for_next263 Option__int
            var inline2938 func() Option__int = for_iter262.next_fn
            var inline2939 Option__int = inline2938()
            for_next263 = inline2939
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1622
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1624 int = vec_get__Vec_3int(self__322, x264)
                vec_push__Vec_3int(buffer__325, t1624)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1570:
        for {
            var t1571 bool = width__327 < length__324
            if t1571 {
                var left__328 int = 0
                Loop_loop1583:
                for {
                    var t1584 bool = left__328 < length__324
                    if t1584 {
                        var t1585 int = left__328 + width__327
                        var middle__329 int
                        var inline2943 bool = t1585 < length__324
                        if inline2943 {
                            middle__329 = t1585
                        } else {
                            middle__329 = length__324
                        }
                        var t1586 int = middle__329 + width__327
                        var right__330 int
                        var inline2941 bool = t1586 < length__324
                        if inline2941 {
                            right__330 = t1586
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1604:
                        for {
                            var t1620 bool = first__331 < middle__329
                            var jp1606 bool
                            if t1620 {
                                var t1621 bool = second__332 < right__330
                                jp1606 = t1621
                            } else {
                                jp1606 = false
                            }
                            if jp1606 {
                                var t1610 int = vec_get__Vec_3int(self__322, first__331)
                                var t1611 int = vec_get__Vec_3int(self__322, second__332)
                                var t1612 int = compare__323(t1610, t1611)
                                var t1613 bool = t1612 <= 0
                                if t1613 {
                                    var index267 int = output__333
                                    vec_get__Vec_3int(buffer__325, index267)
                                    var value269 int = vec_get__Vec_3int(self__322, first__331)
                                    vec_set__Vec_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1615 int = compound_old271 + compound_value272
                                    first__331 = t1615
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_3int(buffer__325, index275)
                                    var value277 int = vec_get__Vec_3int(self__322, second__332)
                                    vec_set__Vec_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1618 int = compound_old279 + compound_value280
                                    second__332 = t1618
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1608 int = compound_old283 + compound_value284
                                output__333 = t1608
                                continue
                            } else {
                                break Loop_loop1604
                            }
                        }
                        Loop_loop1597:
                        for {
                            var t1598 bool = first__331 < middle__329
                            if t1598 {
                                var index288 int = output__333
                                vec_get__Vec_3int(buffer__325, index288)
                                var value290 int = vec_get__Vec_3int(self__322, first__331)
                                vec_set__Vec_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1600 int = compound_old292 + compound_value293
                                first__331 = t1600
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1602 int = compound_old295 + compound_value296
                                output__333 = t1602
                                continue
                            } else {
                                break Loop_loop1597
                            }
                        }
                        Loop_loop1590:
                        for {
                            var t1591 bool = second__332 < right__330
                            if t1591 {
                                var index300 int = output__333
                                vec_get__Vec_3int(buffer__325, index300)
                                var value302 int = vec_get__Vec_3int(self__322, second__332)
                                vec_set__Vec_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1593 int = compound_old304 + compound_value305
                                second__332 = t1593
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1595 int = compound_old307 + compound_value308
                                output__333 = t1595
                                continue
                            } else {
                                break Loop_loop1590
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1583
                    }
                }
                var t1573 FnIterator__int
                var inline2949 int = 0
                var inline2950 *ref_int_x = ref__Ref_3int(inline2949)
                var inline2951 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline2950,
                    end_1: length__324,
                }
                var inline2952 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2951)
                }
                var inline2953 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2952)
                t1573 = inline2953
                var for_iter313 FnIterator__int
                for_iter313 = t1573
                Loop_loop1580:
                for {
                    var for_next314 Option__int
                    var inline2945 func() Option__int = for_iter313.next_fn
                    var inline2946 Option__int = inline2945()
                    for_next314 = inline2946
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1580
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
                var t1577 int = length__324 / 2
                var t1578 bool = width__327 > t1577
                var jp1576 int
                if t1578 {
                    jp1576 = length__324
                } else {
                    var t1579 int = width__327 * 2
                    jp1576 = t1579
                }
                width__327 = jp1576
                continue
            } else {
                break Loop_loop1570
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__371 *_goml_vec_int, equal__372 func(int, int) bool) struct{} {
    var t1657 int
    var inline2980 int = vec_len__Vec_3int(self__371)
    t1657 = inline2980
    var t1658 bool = t1657 < 2
    if t1658 {
        return struct{}{}
    } else {
        var output__373 int = 1
        var t1643 int
        var inline2978 int = vec_len__Vec_3int(self__371)
        t1643 = inline2978
        var t1644 FnIterator__int
        var inline2972 int = 1
        var inline2973 *ref_int_x = ref__Ref_3int(inline2972)
        var inline2974 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2973,
            end_1: t1643,
        }
        var inline2975 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2974)
        }
        var inline2976 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2975)
        t1644 = inline2976
        var for_iter337 FnIterator__int
        for_iter337 = t1644
        Loop_loop1647:
        for {
            var for_next338 Option__int
            var inline2966 func() Option__int = for_iter337.next_fn
            var inline2967 Option__int = inline2966()
            for_next338 = inline2967
            switch for_next338.(type) {
            case Option__int_None:
                break Loop_loop1647
            case Option__int_Some:
                var x339 int = for_next338.(Option__int_Some)._0
                var value__375 int = vec_get__Vec_3int(self__371, x339)
                var t1650 int = output__373 - 1
                var t1651 int = vec_get__Vec_3int(self__371, t1650)
                var t1652 bool = equal__372(t1651, value__375)
                var t1653 bool = !t1652
                if t1653 {
                    var index341 int = output__373
                    vec_get__Vec_3int(self__371, index341)
                    vec_set__Vec_3int(self__371, index341, value__375)
                    var compound_old345 int = output__373
                    var compound_value346 int = 1
                    var t1655 int = compound_old345 + compound_value346
                    output__373 = t1655
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T___o_int_c_int_q_(self__335 *_goml_vec_Tuple2_3int_3int, compare__336 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_int_c_int_q_(self__335, compare__336)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__264 *_goml_vec_int) *_goml_vec_int {
    var t1676 int
    var inline2988 int = vec_len__Vec_3int(self__264)
    t1676 = inline2988
    var result__265 *_goml_vec_int
    var inline2986 *_goml_vec_int = vec_with_capacity__Vec_3int(t1676)
    result__265 = inline2986
    var index__266 int = 0
    Loop_loop1678:
    for {
        var t1679 int
        var inline2984 int = vec_len__Vec_3int(self__264)
        t1679 = inline2984
        var t1680 bool = index__266 < t1679
        if t1680 {
            var t1681 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t1681)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t1682 int = compound_old196 + compound_value197
            index__266 = t1682
            continue
        } else {
            break Loop_loop1678
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
    var t1698 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t1698
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__322 *_goml_vec_string, compare__323 func(string, string) int) struct{} {
    var length__324 int
    var inline3020 int = vec_len__Vec_6string(self__322)
    length__324 = inline3020
    var t1772 bool = length__324 < 2
    if t1772 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_string
        var inline3018 *_goml_vec_string = vec_with_capacity__Vec_6string(length__324)
        buffer__325 = inline3018
        var t1713 FnIterator__int
        var inline3012 int = 0
        var inline3013 *ref_int_x = ref__Ref_3int(inline3012)
        var inline3014 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3013,
            end_1: length__324,
        }
        var inline3015 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3014)
        }
        var inline3016 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3015)
        t1713 = inline3016
        var for_iter262 FnIterator__int
        for_iter262 = t1713
        Loop_loop1768:
        for {
            var for_next263 Option__int
            var inline2994 func() Option__int = for_iter262.next_fn
            var inline2995 Option__int = inline2994()
            for_next263 = inline2995
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1768
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1770 string = vec_get__Vec_6string(self__322, x264)
                vec_push__Vec_6string(buffer__325, t1770)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1716:
        for {
            var t1717 bool = width__327 < length__324
            if t1717 {
                var left__328 int = 0
                Loop_loop1729:
                for {
                    var t1730 bool = left__328 < length__324
                    if t1730 {
                        var t1731 int = left__328 + width__327
                        var middle__329 int
                        var inline2999 bool = t1731 < length__324
                        if inline2999 {
                            middle__329 = t1731
                        } else {
                            middle__329 = length__324
                        }
                        var t1732 int = middle__329 + width__327
                        var right__330 int
                        var inline2997 bool = t1732 < length__324
                        if inline2997 {
                            right__330 = t1732
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1750:
                        for {
                            var t1766 bool = first__331 < middle__329
                            var jp1752 bool
                            if t1766 {
                                var t1767 bool = second__332 < right__330
                                jp1752 = t1767
                            } else {
                                jp1752 = false
                            }
                            if jp1752 {
                                var t1756 string = vec_get__Vec_6string(self__322, first__331)
                                var t1757 string = vec_get__Vec_6string(self__322, second__332)
                                var t1758 int = compare__323(t1756, t1757)
                                var t1759 bool = t1758 <= 0
                                if t1759 {
                                    var index267 int = output__333
                                    vec_get__Vec_6string(buffer__325, index267)
                                    var value269 string = vec_get__Vec_6string(self__322, first__331)
                                    vec_set__Vec_6string(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1761 int = compound_old271 + compound_value272
                                    first__331 = t1761
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_6string(buffer__325, index275)
                                    var value277 string = vec_get__Vec_6string(self__322, second__332)
                                    vec_set__Vec_6string(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1764 int = compound_old279 + compound_value280
                                    second__332 = t1764
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1754 int = compound_old283 + compound_value284
                                output__333 = t1754
                                continue
                            } else {
                                break Loop_loop1750
                            }
                        }
                        Loop_loop1743:
                        for {
                            var t1744 bool = first__331 < middle__329
                            if t1744 {
                                var index288 int = output__333
                                vec_get__Vec_6string(buffer__325, index288)
                                var value290 string = vec_get__Vec_6string(self__322, first__331)
                                vec_set__Vec_6string(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1746 int = compound_old292 + compound_value293
                                first__331 = t1746
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1748 int = compound_old295 + compound_value296
                                output__333 = t1748
                                continue
                            } else {
                                break Loop_loop1743
                            }
                        }
                        Loop_loop1736:
                        for {
                            var t1737 bool = second__332 < right__330
                            if t1737 {
                                var index300 int = output__333
                                vec_get__Vec_6string(buffer__325, index300)
                                var value302 string = vec_get__Vec_6string(self__322, second__332)
                                vec_set__Vec_6string(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1739 int = compound_old304 + compound_value305
                                second__332 = t1739
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1741 int = compound_old307 + compound_value308
                                output__333 = t1741
                                continue
                            } else {
                                break Loop_loop1736
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1729
                    }
                }
                var t1719 FnIterator__int
                var inline3005 int = 0
                var inline3006 *ref_int_x = ref__Ref_3int(inline3005)
                var inline3007 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3006,
                    end_1: length__324,
                }
                var inline3008 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3007)
                }
                var inline3009 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3008)
                t1719 = inline3009
                var for_iter313 FnIterator__int
                for_iter313 = t1719
                Loop_loop1726:
                for {
                    var for_next314 Option__int
                    var inline3001 func() Option__int = for_iter313.next_fn
                    var inline3002 Option__int = inline3001()
                    for_next314 = inline3002
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1726
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
                var t1723 int = length__324 / 2
                var t1724 bool = width__327 > t1723
                var jp1722 int
                if t1724 {
                    jp1722 = length__324
                } else {
                    var t1725 int = width__327 * 2
                    jp1722 = t1725
                }
                width__327 = jp1722
                continue
            } else {
                break Loop_loop1716
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_int_c_int_q_(self__322 *_goml_vec_Tuple2_3int_3int, compare__323 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    var length__324 int
    var inline3050 int = vec_len__Vec_16Tuple2_3int_3int(self__322)
    length__324 = inline3050
    var t1834 bool = length__324 < 2
    if t1834 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_Tuple2_3int_3int
        var inline3048 *_goml_vec_Tuple2_3int_3int = vec_with_capacity__Vec_16Tuple2_3int_3int(length__324)
        buffer__325 = inline3048
        var t1775 FnIterator__int
        var inline3042 int = 0
        var inline3043 *ref_int_x = ref__Ref_3int(inline3042)
        var inline3044 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3043,
            end_1: length__324,
        }
        var inline3045 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3044)
        }
        var inline3046 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3045)
        t1775 = inline3046
        var for_iter262 FnIterator__int
        for_iter262 = t1775
        Loop_loop1830:
        for {
            var for_next263 Option__int
            var inline3024 func() Option__int = for_iter262.next_fn
            var inline3025 Option__int = inline3024()
            for_next263 = inline3025
            switch for_next263.(type) {
            case Option__int_None:
                break Loop_loop1830
            case Option__int_Some:
                var x264 int = for_next263.(Option__int_Some)._0
                var t1832 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, x264)
                vec_push__Vec_16Tuple2_3int_3int(buffer__325, t1832)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1778:
        for {
            var t1779 bool = width__327 < length__324
            if t1779 {
                var left__328 int = 0
                Loop_loop1791:
                for {
                    var t1792 bool = left__328 < length__324
                    if t1792 {
                        var t1793 int = left__328 + width__327
                        var middle__329 int
                        var inline3029 bool = t1793 < length__324
                        if inline3029 {
                            middle__329 = t1793
                        } else {
                            middle__329 = length__324
                        }
                        var t1794 int = middle__329 + width__327
                        var right__330 int
                        var inline3027 bool = t1794 < length__324
                        if inline3027 {
                            right__330 = t1794
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1812:
                        for {
                            var t1828 bool = first__331 < middle__329
                            var jp1814 bool
                            if t1828 {
                                var t1829 bool = second__332 < right__330
                                jp1814 = t1829
                            } else {
                                jp1814 = false
                            }
                            if jp1814 {
                                var t1818 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                var t1819 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                var t1820 int = compare__323(t1818, t1819)
                                var t1821 bool = t1820 <= 0
                                if t1821 {
                                    var index267 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index267)
                                    var value269 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1823 int = compound_old271 + compound_value272
                                    first__331 = t1823
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index275)
                                    var value277 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1826 int = compound_old279 + compound_value280
                                    second__332 = t1826
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1816 int = compound_old283 + compound_value284
                                output__333 = t1816
                                continue
                            } else {
                                break Loop_loop1812
                            }
                        }
                        Loop_loop1805:
                        for {
                            var t1806 bool = first__331 < middle__329
                            if t1806 {
                                var index288 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index288)
                                var value290 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1808 int = compound_old292 + compound_value293
                                first__331 = t1808
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1810 int = compound_old295 + compound_value296
                                output__333 = t1810
                                continue
                            } else {
                                break Loop_loop1805
                            }
                        }
                        Loop_loop1798:
                        for {
                            var t1799 bool = second__332 < right__330
                            if t1799 {
                                var index300 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index300)
                                var value302 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1801 int = compound_old304 + compound_value305
                                second__332 = t1801
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1803 int = compound_old307 + compound_value308
                                output__333 = t1803
                                continue
                            } else {
                                break Loop_loop1798
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1791
                    }
                }
                var t1781 FnIterator__int
                var inline3035 int = 0
                var inline3036 *ref_int_x = ref__Ref_3int(inline3035)
                var inline3037 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3036,
                    end_1: length__324,
                }
                var inline3038 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3037)
                }
                var inline3039 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3038)
                t1781 = inline3039
                var for_iter313 FnIterator__int
                for_iter313 = t1781
                Loop_loop1788:
                for {
                    var for_next314 Option__int
                    var inline3031 func() Option__int = for_iter313.next_fn
                    var inline3032 Option__int = inline3031()
                    for_next314 = inline3032
                    switch for_next314.(type) {
                    case Option__int_None:
                        break Loop_loop1788
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
                var t1785 int = length__324 / 2
                var t1786 bool = width__327 > t1785
                var jp1784 int
                if t1786 {
                    jp1784 = length__324
                } else {
                    var t1787 int = width__327 * 2
                    jp1784 = t1787
                }
                width__327 = jp1784
                continue
            } else {
                break Loop_loop1778
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env427 closure_env_main_0, value__1 int) bool {
    var t1845 bool = value__1 == 5
    return t1845
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env428 closure_env_main_1, value__2 int) bool {
    var t1848 bool = value__2 == 99
    return t1848
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env429 closure_env_main_2, left__3 int, right__4 int) int {
    var t1851 int = left__3 - right__4
    return t1851
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env430 closure_env_main_3, value__6 int) int {
    var t1854 int = value__6 - 4
    return t1854
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env431 closure_env_main_4, value__7 int) int {
    var t1857 int = value__7 - 3
    return t1857
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env432 closure_env_main_5, left__8 int, right__9 int) int {
    var t1860 int = left__8 - right__9
    return t1860
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env433 closure_env_main_6, left__10 int, right__11 int) int {
    var t1863 int = left__10 - right__11
    return t1863
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env434 closure_env_main_7, left__13 string, right__14 string) Ordering {
    var inline3052 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(left__13, right__14)
    return inline3052
}

func _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(env435 closure_env_main_8, left__16 Tuple2_3int_3int, right__17 Tuple2_3int_3int) Ordering {
    var t1869 int = left__16._0
    var t1870 int = right__17._0
    var inline3054 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(t1869, t1870)
    return inline3054
}

func _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(env436 closure_env_main_9, left__21 int, right__22 int) int {
    var t1874 int = left__21 - right__22
    return t1874
}

func _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(env437 closure_env_inherent_Vec_Vec_T_dedup_T_int_10, left__388 int, right__389 int) bool {
    var inline3056 bool = left__388 == right__389
    return inline3056
}

func _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(env438 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11, left__339 string, right__340 string) int {
    var compare__338 func(string, string) Ordering = env438.compare_0
    var t1880 Ordering = compare__338(left__339, right__340)
    switch t1880 {
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

func _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(env439 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12, left__339 Tuple2_3int_3int, right__340 Tuple2_3int_3int) int {
    var compare__338 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = env439.compare_0
    var t1884 Ordering = compare__338(left__339, right__340)
    switch t1884 {
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

func _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(env440 closure_env_goml_builtin_range_13) Option__int {
    var current__496 *ref_int_x = env440.current_0
    var end__495 int = env440.end_1
    var value__497 int = ref_get__Ref_3int(current__496)
    var t1890 bool = value__497 < end__495
    if t1890 {
        var t1891 int = value__497 + 1
        ref_set__Ref_3int(current__496, t1891)
        var t1892 Option__int = Option__int_Some{
            _0: value__497,
        }
        return t1892
    } else {
        return Option__int_None{}
    }
}

func main() {
    main0()
}
