package main

import (
    _goml_fmt "fmt"
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
        items: make([]int, 0, capacity),
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
        items: make([]string, 0, capacity),
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
        items: make([]Tuple2_3int_3int, 0, capacity),
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

type Option__Ordering struct {
    _tag int32
    _v1_0 Ordering
}

type Option__int struct {
    _tag int32
    _v1_0 int
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(self__24 string, other__25 string) Ordering {
    var t525 bool = self__24 < other__25
    if t525 {
        return Less
    } else {
        var t528 bool = self__24 > other__25
        if t528 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(self__32 int, other__33 int) Ordering {
    var t583 bool = self__32 < other__33
    if t583 {
        return Less
    } else {
        var t586 bool = self__32 > other__33
        if t586 {
            return Greater
        } else {
            return Equal
        }
    }
}

func main0() struct{} {
    var t922 [8]int = [8]int{3, 1, 4, 1, 5, 9, 2, 6}
    var values__0 *_goml_vec_int = func(values [8]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [8]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t922)
    var t923 closure_env_main_0 = closure_env_main_0{}
    var t924 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t923, p0)
    }
    var t925 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(values__0, t924)
    var t926 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t925, -1)
    var t927 string = _goml_m_inherent_i_int_i_int_i_to__string(t926)
    println__T_string(t927)
    var t928 closure_env_main_1 = closure_env_main_1{}
    var t929 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t928, p0)
    }
    var t930 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(values__0, t929)
    var t931 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t930, -1)
    var t932 string = _goml_m_inherent_i_int_i_int_i_to__string(t931)
    println__T_string(t932)
    var t933 closure_env_main_2 = closure_env_main_2{}
    var t934 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t933, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(values__0, t934)
    var t935 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(values__0, ",")
    println__T_string(t935)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(values__0)
    var t936 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(values__0, ",")
    println__T_string(t936)
    var t937 [5]int = [5]int{1, 2, 4, 4, 5}
    var ordered__5 *_goml_vec_int = func(values [5]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [5]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t937)
    var t938 closure_env_main_3 = closure_env_main_3{}
    var t939 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t938, p0)
    }
    var t940 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(ordered__5, t939)
    var t941 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t940, -1)
    var t942 string = _goml_m_inherent_i_int_i_int_i_to__string(t941)
    println__T_string(t942)
    var t943 closure_env_main_4 = closure_env_main_4{}
    var t944 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t943, p0)
    }
    var t945 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(ordered__5, t944)
    var t946 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t945, -1)
    var t947 string = _goml_m_inherent_i_int_i_int_i_to__string(t946)
    println__T_string(t947)
    var t948 closure_env_main_5 = closure_env_main_5{}
    var t949 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t948, p0, p1)
    }
    var t950 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(ordered__5, t949)
    var t951 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t950, 0)
    var t952 string = _goml_m_inherent_i_int_i_int_i_to__string(t951)
    println__T_string(t952)
    var t953 closure_env_main_6 = closure_env_main_6{}
    var t954 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t953, p0, p1)
    }
    var t955 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(ordered__5, t954)
    var t956 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(t955, 0)
    var t957 string = _goml_m_inherent_i_int_i_int_i_to__string(t956)
    println__T_string(t957)
    var t958 [3]string = [3]string{"beta", "alpha", "gamma"}
    var names__12 *_goml_vec_string = func(values [3]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [3]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t958)
    var t959 closure_env_main_7 = closure_env_main_7{}
    var t960 func(string, string) Ordering = func(p0 string, p1 string) Ordering {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t959, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(names__12, t960)
    var t961 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(names__12, "|")
    println__T_string(t961)
    var t962 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t963 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 1,
    }
    var t964 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 0,
        _1: 9,
    }
    var t965 [3]Tuple2_3int_3int = [3]Tuple2_3int_3int{t962, t963, t964}
    var pairs__15 *_goml_vec_Tuple2_3int_3int = func(values [3]Tuple2_3int_3int) *_goml_vec_Tuple2_3int_3int {
        var storage struct {
            vector _goml_vec_Tuple2_3int_3int
            values [3]Tuple2_3int_3int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t965)
    var t966 closure_env_main_8 = closure_env_main_8{}
    var t967 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) Ordering {
        return _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(t966, p0, p1)
    }
    var inline2320 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12{
        compare_0: t967,
    }
    var inline2321 func(Tuple2_3int_3int, Tuple2_3int_3int) int = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) int {
        return _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(inline2320, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T___o_int_c_int_q_(pairs__15, inline2321)
    var t968 int
    var inline2318 int = vec_len__Vec_16Tuple2_3int_3int(pairs__15)
    t968 = inline2318
    var t969 string
    var inline2316 string = _goml_runtime_core_int_to_string(t968)
    t969 = inline2316
    var inline2313 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t969)
    _goml_runtime_core_string_println(inline2313)
    var t970 [3]int = [3]int{7, 8, 9}
    var t971 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t970)
    var view__18 []int
    var inline2309 int = 0
    var inline2310 int = 2
    var inline2311 []int = t971.items[inline2309:inline2310]
    view__18 = inline2311
    var t972 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 8)
    var t973 string
    var inline2307 string = _goml_runtime_core_bool_to_string(t972)
    t973 = inline2307
    var inline2304 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t973)
    _goml_runtime_core_string_println(inline2304)
    var t974 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(view__18, 9)
    var t975 string
    var inline2302 string = _goml_runtime_core_bool_to_string(t974)
    t975 = inline2302
    var inline2299 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t975)
    _goml_runtime_core_string_println(inline2299)
    var t976 [2]int = [2]int{1, 2}
    var t977 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t976)
    var frozen__19 FrozenVec__int
    var inline2296 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(t977)
    var inline2297 FrozenVec__int = FrozenVec__int{
        values: inline2296,
    }
    frozen__19 = inline2297
    var t978 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 1)
    var t979 string
    var inline2294 string = _goml_runtime_core_bool_to_string(t978)
    t979 = inline2294
    var inline2291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t979)
    _goml_runtime_core_string_println(inline2291)
    var t980 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(frozen__19, 3)
    var t981 string
    var inline2289 string = _goml_runtime_core_bool_to_string(t980)
    t981 = inline2289
    var inline2286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t981)
    _goml_runtime_core_string_println(inline2286)
    var t982 [0]int = [0]int{}
    var empty__20 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t982)
    var t983 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(empty__20, ",")
    var inline2283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t983)
    _goml_runtime_core_string_println(inline2283)
    var t984 closure_env_main_9 = closure_env_main_9{}
    var t985 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(t984, p0, p1)
    }
    var t986 Option__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(empty__20, t985)
    var t987 bool
    var inline2280 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__int(t986)
    var inline2281 bool = !inline2280
    t987 = inline2281
    var t988 string
    var inline2278 string = _goml_runtime_core_bool_to_string(t987)
    t988 = inline2278
    var inline2275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t988)
    _goml_runtime_core_string_println(inline2275)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t1375 string
    t1375 = value__1
    _goml_runtime_core_string_println(t1375)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__int(self__319 *_goml_vec_int, predicate__320 func(int) bool) Option__int {
    var t1379 int
    var inline2799 int = vec_len__Vec_3int(self__319)
    t1379 = inline2799
    var t1380 FnIterator__int
    var inline2793 int = 0
    var inline2794 *ref_int_x = ref__Ref_3int(inline2793)
    var inline2795 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2794,
        end_1: t1379,
    }
    var inline2796 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2795)
    }
    var inline2797 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2796)
    t1380 = inline2797
    var for_iter257 FnIterator__int
    for_iter257 = t1380
    Loop_loop1382:
    for {
        var for_next258 Option__int
        var inline2789 func() Option__int = for_iter257.next_fn
        var inline2790 Option__int = inline2789()
        for_next258 = inline2790
        switch for_next258._tag {
        case 0:
            break Loop_loop1382
        case 1:
            var x259 int = for_next258._v1_0
            var t1385 int = vec_get__Vec_3int(self__319, x259)
            var t1386 bool = predicate__320(t1385)
            if t1386 {
                var t1387 Option__int = Option__int{
                    _tag: 1,
                    _v1_0: x259,
                }
                return t1387
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return Option__int{
        _tag: 0,
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__int(self__467 Option__int, fallback__468 int) int {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 int = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t1394 string = _goml_runtime_core_int_to_string(self__32)
    return t1394
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__int(self__335 *_goml_vec_int, compare__336 func(int, int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__335, compare__336)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__int(self__378 *_goml_vec_int, separator__379 string) string {
    var t1399 int
    var inline2839 int = vec_len__Vec_3int(self__378)
    t1399 = inline2839
    var parts__380 *_goml_vec_string
    var inline2837 *_goml_vec_string = vec_with_capacity__Vec_6string(t1399)
    parts__380 = inline2837
    var t1400 int
    var inline2835 int = vec_len__Vec_3int(self__378)
    t1400 = inline2835
    var t1401 FnIterator__int
    var inline2829 int = 0
    var inline2830 *ref_int_x = ref__Ref_3int(inline2829)
    var inline2831 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2830,
        end_1: t1400,
    }
    var inline2832 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2831)
    }
    var inline2833 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2832)
    t1401 = inline2833
    var for_iter349 FnIterator__int
    for_iter349 = t1401
    Loop_loop1416:
    for {
        var for_next350 Option__int
        var inline2805 func() Option__int = for_iter349.next_fn
        var inline2806 Option__int = inline2805()
        for_next350 = inline2806
        switch for_next350._tag {
        case 0:
            break Loop_loop1416
        case 1:
            var x351 int = for_next350._v1_0
            var t1418 int = vec_get__Vec_3int(self__378, x351)
            var t1419 string
            var inline2803 string = _goml_runtime_core_int_to_string(t1418)
            t1419 = inline2803
            vec_push__Vec_6string(parts__380, t1419)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1403 int
    var inline2826 int = vec_len__Vec_6string(parts__380)
    t1403 = inline2826
    var t1404 int = t1403 * 2
    var result__382 *_goml_vec_string
    var inline2824 *_goml_vec_string = vec_with_capacity__Vec_6string(t1404)
    result__382 = inline2824
    var t1405 int
    var inline2822 int = vec_len__Vec_6string(parts__380)
    t1405 = inline2822
    var t1406 FnIterator__int
    var inline2816 int = 0
    var inline2817 *ref_int_x = ref__Ref_3int(inline2816)
    var inline2818 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2817,
        end_1: t1405,
    }
    var inline2819 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2818)
    }
    var inline2820 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2819)
    t1406 = inline2820
    var for_iter353 FnIterator__int
    for_iter353 = t1406
    Loop_loop1409:
    for {
        var for_next354 Option__int
        var inline2812 func() Option__int = for_iter353.next_fn
        var inline2813 Option__int = inline2812()
        for_next354 = inline2813
        switch for_next354._tag {
        case 0:
            break Loop_loop1409
        case 1:
            var x355 int = for_next354._v1_0
            var t1414 bool = x355 > 0
            if t1414 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1412 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1412)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1408 string = _goml_runtime_core_string_concat(result__382)
    return t1408
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__int(self__387 *_goml_vec_int) struct{} {
    var t1422 closure_env_inherent_Vec_Vec_T_dedup_T_int_10 = closure_env_inherent_Vec_Vec_T_dedup_T_int_10{}
    var t1423 func(int, int) bool = func(p0 int, p1 int) bool {
        return _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(t1422, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__387, t1423)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__int(self__345 *_goml_vec_int, compare__346 func(int) int) Option__int {
    var low__347 int = 0
    var high__348 int
    var inline2843 int = vec_len__Vec_3int(self__345)
    high__348 = inline2843
    Loop_loop1438:
    for {
        var t1439 bool = low__347 < high__348
        if t1439 {
            var t1440 int = high__348 - low__347
            var t1441 int = t1440 / 2
            var middle__349 int = low__347 + t1441
            var t1443 int = vec_get__Vec_3int(self__345, middle__349)
            var t1444 int = compare__346(t1443)
            var t1445 bool = t1444 < 0
            if t1445 {
                var t1446 int = middle__349 + 1
                low__347 = t1446
                continue
            } else {
                high__348 = middle__349
                continue
            }
        } else {
            break Loop_loop1438
        }
    }
    var t1433 int
    var inline2841 int = vec_len__Vec_3int(self__345)
    t1433 = inline2841
    var t1434 bool = low__347 < t1433
    var jp1431 bool
    if t1434 {
        var t1435 int = vec_get__Vec_3int(self__345, low__347)
        var t1436 int = compare__346(t1435)
        var t1437 bool = t1436 == 0
        jp1431 = t1437
    } else {
        jp1431 = false
    }
    if jp1431 {
        var t1432 Option__int = Option__int{
            _tag: 1,
            _v1_0: low__347,
        }
        return t1432
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__int(self__353 *_goml_vec_int, compare__354 func(int, int) int) Option__int {
    var t1451 bool
    var inline2857 int = vec_len__Vec_3int(self__353)
    var inline2858 bool = inline2857 == 0
    t1451 = inline2858
    if t1451 {
        return Option__int{
            _tag: 0,
        }
    } else {
        var best__355 int = vec_get__Vec_3int(self__353, 0)
        var t1452 int
        var inline2855 int = vec_len__Vec_3int(self__353)
        t1452 = inline2855
        var t1453 FnIterator__int
        var inline2849 int = 1
        var inline2850 *ref_int_x = ref__Ref_3int(inline2849)
        var inline2851 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2850,
            end_1: t1452,
        }
        var inline2852 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2851)
        }
        var inline2853 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2852)
        t1453 = inline2853
        var for_iter326 FnIterator__int
        for_iter326 = t1453
        Loop_loop1456:
        for {
            var for_next327 Option__int
            var inline2845 func() Option__int = for_iter326.next_fn
            var inline2846 Option__int = inline2845()
            for_next327 = inline2846
            switch for_next327._tag {
            case 0:
                break Loop_loop1456
            case 1:
                var x328 int = for_next327._v1_0
                var value__357 int = vec_get__Vec_3int(self__353, x328)
                var t1459 int = compare__354(value__357, best__355)
                var t1460 bool = t1459 < 0
                if t1460 {
                    best__355 = value__357
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1455 Option__int = Option__int{
            _tag: 1,
            _v1_0: best__355,
        }
        return t1455
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__int(self__358 *_goml_vec_int, compare__359 func(int, int) int) Option__int {
    var t1465 bool
    var inline2872 int = vec_len__Vec_3int(self__358)
    var inline2873 bool = inline2872 == 0
    t1465 = inline2873
    if t1465 {
        return Option__int{
            _tag: 0,
        }
    } else {
        var best__360 int = vec_get__Vec_3int(self__358, 0)
        var t1466 int
        var inline2870 int = vec_len__Vec_3int(self__358)
        t1466 = inline2870
        var t1467 FnIterator__int
        var inline2864 int = 1
        var inline2865 *ref_int_x = ref__Ref_3int(inline2864)
        var inline2866 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2865,
            end_1: t1466,
        }
        var inline2867 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2866)
        }
        var inline2868 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2867)
        t1467 = inline2868
        var for_iter331 FnIterator__int
        for_iter331 = t1467
        Loop_loop1470:
        for {
            var for_next332 Option__int
            var inline2860 func() Option__int = for_iter331.next_fn
            var inline2861 Option__int = inline2860()
            for_next332 = inline2861
            switch for_next332._tag {
            case 0:
                break Loop_loop1470
            case 1:
                var x333 int = for_next332._v1_0
                var value__362 int = vec_get__Vec_3int(self__358, x333)
                var t1473 int = compare__359(value__362, best__360)
                var t1474 bool = t1473 > 0
                if t1474 {
                    best__360 = value__362
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1469 Option__int = Option__int{
            _tag: 1,
            _v1_0: best__360,
        }
        return t1469
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(self__337 *_goml_vec_string, compare__338 func(string, string) Ordering) struct{} {
    var t1479 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11{
        compare_0: compare__338,
    }
    var t1480 func(string, string) int = func(p0 string, p1 string) int {
        return _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(t1479, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__337, t1480)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__378 *_goml_vec_string, separator__379 string) string {
    var t1484 int
    var inline2917 int = vec_len__Vec_6string(self__378)
    t1484 = inline2917
    var parts__380 *_goml_vec_string
    var inline2915 *_goml_vec_string = vec_with_capacity__Vec_6string(t1484)
    parts__380 = inline2915
    var t1485 int
    var inline2913 int = vec_len__Vec_6string(self__378)
    t1485 = inline2913
    var t1486 FnIterator__int
    var inline2907 int = 0
    var inline2908 *ref_int_x = ref__Ref_3int(inline2907)
    var inline2909 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2908,
        end_1: t1485,
    }
    var inline2910 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2909)
    }
    var inline2911 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2910)
    t1486 = inline2911
    var for_iter349 FnIterator__int
    for_iter349 = t1486
    Loop_loop1501:
    for {
        var for_next350 Option__int
        var inline2883 func() Option__int = for_iter349.next_fn
        var inline2884 Option__int = inline2883()
        for_next350 = inline2884
        switch for_next350._tag {
        case 0:
            break Loop_loop1501
        case 1:
            var x351 int = for_next350._v1_0
            var t1503 string = vec_get__Vec_6string(self__378, x351)
            var t1504 string
            t1504 = t1503
            vec_push__Vec_6string(parts__380, t1504)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1488 int
    var inline2904 int = vec_len__Vec_6string(parts__380)
    t1488 = inline2904
    var t1489 int = t1488 * 2
    var result__382 *_goml_vec_string
    var inline2902 *_goml_vec_string = vec_with_capacity__Vec_6string(t1489)
    result__382 = inline2902
    var t1490 int
    var inline2900 int = vec_len__Vec_6string(parts__380)
    t1490 = inline2900
    var t1491 FnIterator__int
    var inline2894 int = 0
    var inline2895 *ref_int_x = ref__Ref_3int(inline2894)
    var inline2896 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline2895,
        end_1: t1490,
    }
    var inline2897 func() Option__int = func() Option__int {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2896)
    }
    var inline2898 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2897)
    t1491 = inline2898
    var for_iter353 FnIterator__int
    for_iter353 = t1491
    Loop_loop1494:
    for {
        var for_next354 Option__int
        var inline2890 func() Option__int = for_iter353.next_fn
        var inline2891 Option__int = inline2890()
        for_next354 = inline2891
        switch for_next354._tag {
        case 0:
            break Loop_loop1494
        case 1:
            var x355 int = for_next354._v1_0
            var t1499 bool = x355 > 0
            if t1499 {
                vec_push__Vec_6string(result__382, separator__379)
            } else {}
            var t1497 string = vec_get__Vec_6string(parts__380, x355)
            vec_push__Vec_6string(result__382, t1497)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1493 string = _goml_runtime_core_string_concat(result__382)
    return t1493
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__int(self__390 []int, expected__391 int) bool {
    var index__392 int = 0
    Loop_loop1522:
    for {
        var t1523 int
        var inline2926 int = len(self__390)
        t1523 = inline2926
        var t1524 bool = index__392 < t1523
        if t1524 {
            var t1528 int = self__390[index__392]
            var t1529 bool
            var inline2924 bool = t1528 == expected__391
            t1529 = inline2924
            if t1529 {
                return true
            } else {
                var compound_old364 int = index__392
                var compound_value365 int = 1
                var t1526 int = compound_old364 + compound_value365
                index__392 = t1526
                continue
            }
        } else {
            break Loop_loop1522
        }
    }
    return false
}

func _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__int(self__393 FrozenVec__int, expected__394 int) bool {
    var index__395 int = 0
    Loop_loop1540:
    for {
        var t1541 int
        var inline2933 *_goml_vec_int = self__393.values
        var inline2934 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(inline2933)
        t1541 = inline2934
        var t1542 bool = index__395 < t1541
        if t1542 {
            var t1546 int
            var inline2930 *_goml_vec_int = self__393.values
            var inline2931 int = vec_get__Vec_3int(inline2930, index__395)
            t1546 = inline2931
            var t1547 bool
            var inline2928 bool = t1546 == expected__394
            t1547 = inline2928
            if t1547 {
                return true
            } else {
                var compound_old369 int = index__395
                var compound_value370 int = 1
                var t1544 int = compound_old369 + compound_value370
                index__395 = t1544
                continue
            }
        } else {
            break Loop_loop1540
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__273 *_goml_vec_int) int {
    var t1561 int = vec_len__Vec_3int(self__273)
    return t1561
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__int(self__322 *_goml_vec_int, compare__323 func(int, int) int) struct{} {
    var length__324 int
    var inline2967 int = vec_len__Vec_3int(self__322)
    length__324 = inline2967
    var t1629 bool = length__324 < 2
    if t1629 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_int
        var inline2965 *_goml_vec_int = vec_with_capacity__Vec_3int(length__324)
        buffer__325 = inline2965
        var t1570 FnIterator__int
        var inline2959 int = 0
        var inline2960 *ref_int_x = ref__Ref_3int(inline2959)
        var inline2961 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2960,
            end_1: length__324,
        }
        var inline2962 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2961)
        }
        var inline2963 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2962)
        t1570 = inline2963
        var for_iter262 FnIterator__int
        for_iter262 = t1570
        Loop_loop1625:
        for {
            var for_next263 Option__int
            var inline2941 func() Option__int = for_iter262.next_fn
            var inline2942 Option__int = inline2941()
            for_next263 = inline2942
            switch for_next263._tag {
            case 0:
                break Loop_loop1625
            case 1:
                var x264 int = for_next263._v1_0
                var t1627 int = vec_get__Vec_3int(self__322, x264)
                vec_push__Vec_3int(buffer__325, t1627)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1573:
        for {
            var t1574 bool = width__327 < length__324
            if t1574 {
                var left__328 int = 0
                Loop_loop1586:
                for {
                    var t1587 bool = left__328 < length__324
                    if t1587 {
                        var t1588 int = left__328 + width__327
                        var middle__329 int
                        var inline2946 bool = t1588 < length__324
                        if inline2946 {
                            middle__329 = t1588
                        } else {
                            middle__329 = length__324
                        }
                        var t1589 int = middle__329 + width__327
                        var right__330 int
                        var inline2944 bool = t1589 < length__324
                        if inline2944 {
                            right__330 = t1589
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1607:
                        for {
                            var t1623 bool = first__331 < middle__329
                            var jp1609 bool
                            if t1623 {
                                var t1624 bool = second__332 < right__330
                                jp1609 = t1624
                            } else {
                                jp1609 = false
                            }
                            if jp1609 {
                                var t1613 int = vec_get__Vec_3int(self__322, first__331)
                                var t1614 int = vec_get__Vec_3int(self__322, second__332)
                                var t1615 int = compare__323(t1613, t1614)
                                var t1616 bool = t1615 <= 0
                                if t1616 {
                                    var index267 int = output__333
                                    vec_get__Vec_3int(buffer__325, index267)
                                    var value269 int = vec_get__Vec_3int(self__322, first__331)
                                    vec_set__Vec_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1618 int = compound_old271 + compound_value272
                                    first__331 = t1618
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_3int(buffer__325, index275)
                                    var value277 int = vec_get__Vec_3int(self__322, second__332)
                                    vec_set__Vec_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1621 int = compound_old279 + compound_value280
                                    second__332 = t1621
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1611 int = compound_old283 + compound_value284
                                output__333 = t1611
                                continue
                            } else {
                                break Loop_loop1607
                            }
                        }
                        Loop_loop1600:
                        for {
                            var t1601 bool = first__331 < middle__329
                            if t1601 {
                                var index288 int = output__333
                                vec_get__Vec_3int(buffer__325, index288)
                                var value290 int = vec_get__Vec_3int(self__322, first__331)
                                vec_set__Vec_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1603 int = compound_old292 + compound_value293
                                first__331 = t1603
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1605 int = compound_old295 + compound_value296
                                output__333 = t1605
                                continue
                            } else {
                                break Loop_loop1600
                            }
                        }
                        Loop_loop1593:
                        for {
                            var t1594 bool = second__332 < right__330
                            if t1594 {
                                var index300 int = output__333
                                vec_get__Vec_3int(buffer__325, index300)
                                var value302 int = vec_get__Vec_3int(self__322, second__332)
                                vec_set__Vec_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1596 int = compound_old304 + compound_value305
                                second__332 = t1596
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1598 int = compound_old307 + compound_value308
                                output__333 = t1598
                                continue
                            } else {
                                break Loop_loop1593
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1586
                    }
                }
                var t1576 FnIterator__int
                var inline2952 int = 0
                var inline2953 *ref_int_x = ref__Ref_3int(inline2952)
                var inline2954 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline2953,
                    end_1: length__324,
                }
                var inline2955 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2954)
                }
                var inline2956 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2955)
                t1576 = inline2956
                var for_iter313 FnIterator__int
                for_iter313 = t1576
                Loop_loop1583:
                for {
                    var for_next314 Option__int
                    var inline2948 func() Option__int = for_iter313.next_fn
                    var inline2949 Option__int = inline2948()
                    for_next314 = inline2949
                    switch for_next314._tag {
                    case 0:
                        break Loop_loop1583
                    case 1:
                        var x315 int = for_next314._v1_0
                        vec_get__Vec_3int(self__322, x315)
                        var value319 int = vec_get__Vec_3int(buffer__325, x315)
                        vec_set__Vec_3int(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1580 int = length__324 / 2
                var t1581 bool = width__327 > t1580
                var jp1579 int
                if t1581 {
                    jp1579 = length__324
                } else {
                    var t1582 int = width__327 * 2
                    jp1579 = t1582
                }
                width__327 = jp1579
                continue
            } else {
                break Loop_loop1573
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__int(self__371 *_goml_vec_int, equal__372 func(int, int) bool) struct{} {
    var t1660 int
    var inline2983 int = vec_len__Vec_3int(self__371)
    t1660 = inline2983
    var t1661 bool = t1660 < 2
    if t1661 {
        return struct{}{}
    } else {
        var output__373 int = 1
        var t1646 int
        var inline2981 int = vec_len__Vec_3int(self__371)
        t1646 = inline2981
        var t1647 FnIterator__int
        var inline2975 int = 1
        var inline2976 *ref_int_x = ref__Ref_3int(inline2975)
        var inline2977 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline2976,
            end_1: t1646,
        }
        var inline2978 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline2977)
        }
        var inline2979 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline2978)
        t1647 = inline2979
        var for_iter337 FnIterator__int
        for_iter337 = t1647
        Loop_loop1650:
        for {
            var for_next338 Option__int
            var inline2969 func() Option__int = for_iter337.next_fn
            var inline2970 Option__int = inline2969()
            for_next338 = inline2970
            switch for_next338._tag {
            case 0:
                break Loop_loop1650
            case 1:
                var x339 int = for_next338._v1_0
                var value__375 int = vec_get__Vec_3int(self__371, x339)
                var t1653 int = output__373 - 1
                var t1654 int = vec_get__Vec_3int(self__371, t1653)
                var t1655 bool = equal__372(t1654, value__375)
                var t1656 bool = !t1655
                if t1656 {
                    var index341 int = output__373
                    vec_get__Vec_3int(self__371, index341)
                    vec_set__Vec_3int(self__371, index341, value__375)
                    var compound_old345 int = output__373
                    var compound_value346 int = 1
                    var t1658 int = compound_old345 + compound_value346
                    output__373 = t1658
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
    var t1679 int
    var inline2991 int = vec_len__Vec_3int(self__264)
    t1679 = inline2991
    var result__265 *_goml_vec_int
    var inline2989 *_goml_vec_int = vec_with_capacity__Vec_3int(t1679)
    result__265 = inline2989
    var index__266 int = 0
    Loop_loop1681:
    for {
        var t1682 int
        var inline2987 int = vec_len__Vec_3int(self__264)
        t1682 = inline2987
        var t1683 bool = index__266 < t1682
        if t1683 {
            var t1684 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t1684)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t1685 int = compound_old196 + compound_value197
            index__266 = t1685
            continue
        } else {
            break Loop_loop1681
        }
    }
    return result__265
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__int(self__465 Option__int) bool {
    switch self__465._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__254 func() Option__int) FnIterator__int {
    var t1701 FnIterator__int = FnIterator__int{
        next_fn: next_fn__254,
    }
    return t1701
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__322 *_goml_vec_string, compare__323 func(string, string) int) struct{} {
    var length__324 int
    var inline3023 int = vec_len__Vec_6string(self__322)
    length__324 = inline3023
    var t1775 bool = length__324 < 2
    if t1775 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_string
        var inline3021 *_goml_vec_string = vec_with_capacity__Vec_6string(length__324)
        buffer__325 = inline3021
        var t1716 FnIterator__int
        var inline3015 int = 0
        var inline3016 *ref_int_x = ref__Ref_3int(inline3015)
        var inline3017 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3016,
            end_1: length__324,
        }
        var inline3018 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3017)
        }
        var inline3019 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3018)
        t1716 = inline3019
        var for_iter262 FnIterator__int
        for_iter262 = t1716
        Loop_loop1771:
        for {
            var for_next263 Option__int
            var inline2997 func() Option__int = for_iter262.next_fn
            var inline2998 Option__int = inline2997()
            for_next263 = inline2998
            switch for_next263._tag {
            case 0:
                break Loop_loop1771
            case 1:
                var x264 int = for_next263._v1_0
                var t1773 string = vec_get__Vec_6string(self__322, x264)
                vec_push__Vec_6string(buffer__325, t1773)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1719:
        for {
            var t1720 bool = width__327 < length__324
            if t1720 {
                var left__328 int = 0
                Loop_loop1732:
                for {
                    var t1733 bool = left__328 < length__324
                    if t1733 {
                        var t1734 int = left__328 + width__327
                        var middle__329 int
                        var inline3002 bool = t1734 < length__324
                        if inline3002 {
                            middle__329 = t1734
                        } else {
                            middle__329 = length__324
                        }
                        var t1735 int = middle__329 + width__327
                        var right__330 int
                        var inline3000 bool = t1735 < length__324
                        if inline3000 {
                            right__330 = t1735
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1753:
                        for {
                            var t1769 bool = first__331 < middle__329
                            var jp1755 bool
                            if t1769 {
                                var t1770 bool = second__332 < right__330
                                jp1755 = t1770
                            } else {
                                jp1755 = false
                            }
                            if jp1755 {
                                var t1759 string = vec_get__Vec_6string(self__322, first__331)
                                var t1760 string = vec_get__Vec_6string(self__322, second__332)
                                var t1761 int = compare__323(t1759, t1760)
                                var t1762 bool = t1761 <= 0
                                if t1762 {
                                    var index267 int = output__333
                                    vec_get__Vec_6string(buffer__325, index267)
                                    var value269 string = vec_get__Vec_6string(self__322, first__331)
                                    vec_set__Vec_6string(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1764 int = compound_old271 + compound_value272
                                    first__331 = t1764
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_6string(buffer__325, index275)
                                    var value277 string = vec_get__Vec_6string(self__322, second__332)
                                    vec_set__Vec_6string(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1767 int = compound_old279 + compound_value280
                                    second__332 = t1767
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1757 int = compound_old283 + compound_value284
                                output__333 = t1757
                                continue
                            } else {
                                break Loop_loop1753
                            }
                        }
                        Loop_loop1746:
                        for {
                            var t1747 bool = first__331 < middle__329
                            if t1747 {
                                var index288 int = output__333
                                vec_get__Vec_6string(buffer__325, index288)
                                var value290 string = vec_get__Vec_6string(self__322, first__331)
                                vec_set__Vec_6string(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1749 int = compound_old292 + compound_value293
                                first__331 = t1749
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1751 int = compound_old295 + compound_value296
                                output__333 = t1751
                                continue
                            } else {
                                break Loop_loop1746
                            }
                        }
                        Loop_loop1739:
                        for {
                            var t1740 bool = second__332 < right__330
                            if t1740 {
                                var index300 int = output__333
                                vec_get__Vec_6string(buffer__325, index300)
                                var value302 string = vec_get__Vec_6string(self__322, second__332)
                                vec_set__Vec_6string(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1742 int = compound_old304 + compound_value305
                                second__332 = t1742
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1744 int = compound_old307 + compound_value308
                                output__333 = t1744
                                continue
                            } else {
                                break Loop_loop1739
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1732
                    }
                }
                var t1722 FnIterator__int
                var inline3008 int = 0
                var inline3009 *ref_int_x = ref__Ref_3int(inline3008)
                var inline3010 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3009,
                    end_1: length__324,
                }
                var inline3011 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3010)
                }
                var inline3012 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3011)
                t1722 = inline3012
                var for_iter313 FnIterator__int
                for_iter313 = t1722
                Loop_loop1729:
                for {
                    var for_next314 Option__int
                    var inline3004 func() Option__int = for_iter313.next_fn
                    var inline3005 Option__int = inline3004()
                    for_next314 = inline3005
                    switch for_next314._tag {
                    case 0:
                        break Loop_loop1729
                    case 1:
                        var x315 int = for_next314._v1_0
                        vec_get__Vec_6string(self__322, x315)
                        var value319 string = vec_get__Vec_6string(buffer__325, x315)
                        vec_set__Vec_6string(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1726 int = length__324 / 2
                var t1727 bool = width__327 > t1726
                var jp1725 int
                if t1727 {
                    jp1725 = length__324
                } else {
                    var t1728 int = width__327 * 2
                    jp1725 = t1728
                }
                width__327 = jp1725
                continue
            } else {
                break Loop_loop1719
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_int_c_int_q_(self__322 *_goml_vec_Tuple2_3int_3int, compare__323 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    var length__324 int
    var inline3053 int = vec_len__Vec_16Tuple2_3int_3int(self__322)
    length__324 = inline3053
    var t1837 bool = length__324 < 2
    if t1837 {
        return struct{}{}
    } else {
        var buffer__325 *_goml_vec_Tuple2_3int_3int
        var inline3051 *_goml_vec_Tuple2_3int_3int = vec_with_capacity__Vec_16Tuple2_3int_3int(length__324)
        buffer__325 = inline3051
        var t1778 FnIterator__int
        var inline3045 int = 0
        var inline3046 *ref_int_x = ref__Ref_3int(inline3045)
        var inline3047 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3046,
            end_1: length__324,
        }
        var inline3048 func() Option__int = func() Option__int {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3047)
        }
        var inline3049 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3048)
        t1778 = inline3049
        var for_iter262 FnIterator__int
        for_iter262 = t1778
        Loop_loop1833:
        for {
            var for_next263 Option__int
            var inline3027 func() Option__int = for_iter262.next_fn
            var inline3028 Option__int = inline3027()
            for_next263 = inline3028
            switch for_next263._tag {
            case 0:
                break Loop_loop1833
            case 1:
                var x264 int = for_next263._v1_0
                var t1835 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, x264)
                vec_push__Vec_16Tuple2_3int_3int(buffer__325, t1835)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__327 int = 1
        Loop_loop1781:
        for {
            var t1782 bool = width__327 < length__324
            if t1782 {
                var left__328 int = 0
                Loop_loop1794:
                for {
                    var t1795 bool = left__328 < length__324
                    if t1795 {
                        var t1796 int = left__328 + width__327
                        var middle__329 int
                        var inline3032 bool = t1796 < length__324
                        if inline3032 {
                            middle__329 = t1796
                        } else {
                            middle__329 = length__324
                        }
                        var t1797 int = middle__329 + width__327
                        var right__330 int
                        var inline3030 bool = t1797 < length__324
                        if inline3030 {
                            right__330 = t1797
                        } else {
                            right__330 = length__324
                        }
                        var first__331 int = left__328
                        var second__332 int = middle__329
                        var output__333 int = left__328
                        Loop_loop1815:
                        for {
                            var t1831 bool = first__331 < middle__329
                            var jp1817 bool
                            if t1831 {
                                var t1832 bool = second__332 < right__330
                                jp1817 = t1832
                            } else {
                                jp1817 = false
                            }
                            if jp1817 {
                                var t1821 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                var t1822 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                var t1823 int = compare__323(t1821, t1822)
                                var t1824 bool = t1823 <= 0
                                if t1824 {
                                    var index267 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index267)
                                    var value269 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index267, value269)
                                    var compound_old271 int = first__331
                                    var compound_value272 int = 1
                                    var t1826 int = compound_old271 + compound_value272
                                    first__331 = t1826
                                } else {
                                    var index275 int = output__333
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__325, index275)
                                    var value277 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__325, index275, value277)
                                    var compound_old279 int = second__332
                                    var compound_value280 int = 1
                                    var t1829 int = compound_old279 + compound_value280
                                    second__332 = t1829
                                }
                                var compound_old283 int = output__333
                                var compound_value284 int = 1
                                var t1819 int = compound_old283 + compound_value284
                                output__333 = t1819
                                continue
                            } else {
                                break Loop_loop1815
                            }
                        }
                        Loop_loop1808:
                        for {
                            var t1809 bool = first__331 < middle__329
                            if t1809 {
                                var index288 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index288)
                                var value290 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, first__331)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index288, value290)
                                var compound_old292 int = first__331
                                var compound_value293 int = 1
                                var t1811 int = compound_old292 + compound_value293
                                first__331 = t1811
                                var compound_old295 int = output__333
                                var compound_value296 int = 1
                                var t1813 int = compound_old295 + compound_value296
                                output__333 = t1813
                                continue
                            } else {
                                break Loop_loop1808
                            }
                        }
                        Loop_loop1801:
                        for {
                            var t1802 bool = second__332 < right__330
                            if t1802 {
                                var index300 int = output__333
                                vec_get__Vec_16Tuple2_3int_3int(buffer__325, index300)
                                var value302 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__322, second__332)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__325, index300, value302)
                                var compound_old304 int = second__332
                                var compound_value305 int = 1
                                var t1804 int = compound_old304 + compound_value305
                                second__332 = t1804
                                var compound_old307 int = output__333
                                var compound_value308 int = 1
                                var t1806 int = compound_old307 + compound_value308
                                output__333 = t1806
                                continue
                            } else {
                                break Loop_loop1801
                            }
                        }
                        left__328 = right__330
                        continue
                    } else {
                        break Loop_loop1794
                    }
                }
                var t1784 FnIterator__int
                var inline3038 int = 0
                var inline3039 *ref_int_x = ref__Ref_3int(inline3038)
                var inline3040 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3039,
                    end_1: length__324,
                }
                var inline3041 func() Option__int = func() Option__int {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3040)
                }
                var inline3042 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(inline3041)
                t1784 = inline3042
                var for_iter313 FnIterator__int
                for_iter313 = t1784
                Loop_loop1791:
                for {
                    var for_next314 Option__int
                    var inline3034 func() Option__int = for_iter313.next_fn
                    var inline3035 Option__int = inline3034()
                    for_next314 = inline3035
                    switch for_next314._tag {
                    case 0:
                        break Loop_loop1791
                    case 1:
                        var x315 int = for_next314._v1_0
                        vec_get__Vec_16Tuple2_3int_3int(self__322, x315)
                        var value319 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(buffer__325, x315)
                        vec_set__Vec_16Tuple2_3int_3int(self__322, x315, value319)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1788 int = length__324 / 2
                var t1789 bool = width__327 > t1788
                var jp1787 int
                if t1789 {
                    jp1787 = length__324
                } else {
                    var t1790 int = width__327 * 2
                    jp1787 = t1790
                }
                width__327 = jp1787
                continue
            } else {
                break Loop_loop1781
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env430 closure_env_main_0, value__1 int) bool {
    var t1848 bool = value__1 == 5
    return t1848
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env431 closure_env_main_1, value__2 int) bool {
    var t1851 bool = value__2 == 99
    return t1851
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env432 closure_env_main_2, left__3 int, right__4 int) int {
    var t1854 int = left__3 - right__4
    return t1854
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env433 closure_env_main_3, value__6 int) int {
    var t1857 int = value__6 - 4
    return t1857
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env434 closure_env_main_4, value__7 int) int {
    var t1860 int = value__7 - 3
    return t1860
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env435 closure_env_main_5, left__8 int, right__9 int) int {
    var t1863 int = left__8 - right__9
    return t1863
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env436 closure_env_main_6, left__10 int, right__11 int) int {
    var t1866 int = left__10 - right__11
    return t1866
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env437 closure_env_main_7, left__13 string, right__14 string) Ordering {
    var inline3055 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(left__13, right__14)
    return inline3055
}

func _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(env438 closure_env_main_8, left__16 Tuple2_3int_3int, right__17 Tuple2_3int_3int) Ordering {
    var t1872 int = left__16._0
    var t1873 int = right__17._0
    var inline3057 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_int_i_cmp(t1872, t1873)
    return inline3057
}

func _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(env439 closure_env_main_9, left__21 int, right__22 int) int {
    var t1877 int = left__21 - right__22
    return t1877
}

func _goml_m_inherent_i_closure__en_h9b77fba9cc53b3c3a0a25fec1775bf14_int__10_i_apply(env440 closure_env_inherent_Vec_Vec_T_dedup_T_int_10, left__388 int, right__389 int) bool {
    var inline3059 bool = left__388 == right__389
    return inline3059
}

func _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(env441 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11, left__339 string, right__340 string) int {
    var compare__338 func(string, string) Ordering = env441.compare_0
    var t1883 Ordering = compare__338(left__339, right__340)
    switch t1883 {
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

func _goml_m_inherent_i_closure__en_hda1611287a64cff76927f92b5317363e_int__12_i_apply(env442 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_int_int_12, left__339 Tuple2_3int_3int, right__340 Tuple2_3int_3int) int {
    var compare__338 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = env442.compare_0
    var t1887 Ordering = compare__338(left__339, right__340)
    switch t1887 {
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

func _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(env443 closure_env_goml_builtin_range_13) Option__int {
    var current__505 *ref_int_x = env443.current_0
    var end__504 int = env443.end_1
    var value__506 int = ref_get__Ref_3int(current__505)
    var t1893 bool = value__506 < end__504
    if t1893 {
        var t1894 int = value__506 + 1
        ref_set__Ref_3int(current__505, t1894)
        var t1895 Option__int = Option__int{
            _tag: 1,
            _v1_0: value__506,
        }
        return t1895
    } else {
        return Option__int{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
