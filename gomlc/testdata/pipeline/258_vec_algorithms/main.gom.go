package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

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

type Tuple2_3int_3int struct {
    _0 int
    _1 int
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

type FnIterator__isize struct {
    next_fn func() Option__isize
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

type closure_env_inherent_Vec_Vec_T_dedup_T_isize_10 struct {}

type closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 struct {
    compare_0 func(string, string) Ordering
}

type closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12 struct {
    compare_0 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering
}

type closure_env_goml_builtin_range_13 struct {
    current_0 *ref_int_x
    end_1 int
}

type FrozenVec__isize struct {
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

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(self__24 string, other__25 string) Ordering {
    var t910 bool = self__24 < other__25
    if t910 {
        return Less
    } else {
        var t913 bool = self__24 > other__25
        if t913 {
            return Greater
        } else {
            return Equal
        }
    }
}

func _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(self__32 int, other__33 int) Ordering {
    var t968 bool = self__32 < other__33
    if t968 {
        return Less
    } else {
        var t971 bool = self__32 > other__33
        if t971 {
            return Greater
        } else {
            return Equal
        }
    }
}

func main0() struct{} {
    var t1307 [8]int = [8]int{3, 1, 4, 1, 5, 9, 2, 6}
    var values__0 *_goml_vec_int = func(values [8]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [8]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1307)
    var t1308 closure_env_main_0 = closure_env_main_0{}
    var t1309 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t1308, p0)
    }
    var t1310 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__isize(values__0, t1309)
    var t1311 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1310, -1)
    var t1312 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1311)
    println__T_string(t1312)
    var t1313 closure_env_main_1 = closure_env_main_1{}
    var t1314 func(int) bool = func(p0 int) bool {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t1313, p0)
    }
    var t1315 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__isize(values__0, t1314)
    var t1316 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1315, -1)
    var t1317 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1316)
    println__T_string(t1317)
    var t1318 closure_env_main_2 = closure_env_main_2{}
    var t1319 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t1318, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__isize(values__0, t1319)
    var t1320 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(values__0, ",")
    println__T_string(t1320)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__isize(values__0)
    var t1321 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(values__0, ",")
    println__T_string(t1321)
    var t1322 [5]int = [5]int{1, 2, 4, 4, 5}
    var ordered__5 *_goml_vec_int = func(values [5]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [5]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1322)
    var t1323 closure_env_main_3 = closure_env_main_3{}
    var t1324 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t1323, p0)
    }
    var t1325 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__isize(ordered__5, t1324)
    var t1326 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1325, -1)
    var t1327 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1326)
    println__T_string(t1327)
    var t1328 closure_env_main_4 = closure_env_main_4{}
    var t1329 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t1328, p0)
    }
    var t1330 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__isize(ordered__5, t1329)
    var t1331 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1330, -1)
    var t1332 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1331)
    println__T_string(t1332)
    var t1333 closure_env_main_5 = closure_env_main_5{}
    var t1334 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t1333, p0, p1)
    }
    var t1335 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__isize(ordered__5, t1334)
    var t1336 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1335, 0)
    var t1337 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1336)
    println__T_string(t1337)
    var t1338 closure_env_main_6 = closure_env_main_6{}
    var t1339 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t1338, p0, p1)
    }
    var t1340 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__isize(ordered__5, t1339)
    var t1341 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t1340, 0)
    var t1342 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t1341)
    println__T_string(t1342)
    var t1343 [3]string = [3]string{"beta", "alpha", "gamma"}
    var names__12 *_goml_vec_string = func(values [3]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [3]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1343)
    var t1344 closure_env_main_7 = closure_env_main_7{}
    var t1345 func(string, string) Ordering = func(p0 string, p1 string) Ordering {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t1344, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(names__12, t1345)
    var t1346 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(names__12, "|")
    println__T_string(t1346)
    var t1347 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 2,
    }
    var t1348 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 1,
        _1: 1,
    }
    var t1349 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 0,
        _1: 9,
    }
    var t1350 [3]Tuple2_3int_3int = [3]Tuple2_3int_3int{t1347, t1348, t1349}
    var pairs__15 *_goml_vec_Tuple2_3int_3int = func(values [3]Tuple2_3int_3int) *_goml_vec_Tuple2_3int_3int {
        var storage struct {
            vector _goml_vec_Tuple2_3int_3int
            values [3]Tuple2_3int_3int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1350)
    var t1351 closure_env_main_8 = closure_env_main_8{}
    var t1352 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) Ordering {
        return _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(t1351, p0, p1)
    }
    var inline2778 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12{
        compare_0: t1352,
    }
    var inline2779 func(Tuple2_3int_3int, Tuple2_3int_3int) int = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) int {
        return _goml_m_inherent_i_closure__en_hc4b58ff73aa2ff3a45662a7349486da0_ize__12_i_apply(inline2778, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T___o_isize_c_isize_q_(pairs__15, inline2779)
    var t1353 int
    var inline2776 int = vec_len__Vec_16Tuple2_3int_3int(pairs__15)
    t1353 = inline2776
    var t1354 string
    var inline2774 string = __goml_builtin_int_to_string(t1353)
    t1354 = inline2774
    var inline2771 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1354)
    _goml_runtime_core_string_println(inline2771)
    var t1355 [3]int = [3]int{7, 8, 9}
    var t1356 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1355)
    var view__18 []int
    var inline2767 int = 0
    var inline2768 int = 2
    var inline2769 []int = t1356.items[inline2767:inline2768]
    view__18 = inline2769
    var t1357 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(view__18, 8)
    var t1358 string
    var inline2765 string = _goml_runtime_core_bool_to_string(t1357)
    t1358 = inline2765
    var inline2762 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1358)
    _goml_runtime_core_string_println(inline2762)
    var t1359 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(view__18, 9)
    var t1360 string
    var inline2760 string = _goml_runtime_core_bool_to_string(t1359)
    t1360 = inline2760
    var inline2757 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1360)
    _goml_runtime_core_string_println(inline2757)
    var t1361 [2]int = [2]int{1, 2}
    var t1362 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t1361)
    var frozen__19 FrozenVec__isize
    var inline2754 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(t1362)
    var inline2755 FrozenVec__isize = FrozenVec__isize{
        values: inline2754,
    }
    frozen__19 = inline2755
    var t1363 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(frozen__19, 1)
    var t1364 string
    var inline2752 string = _goml_runtime_core_bool_to_string(t1363)
    t1364 = inline2752
    var inline2749 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1364)
    _goml_runtime_core_string_println(inline2749)
    var t1365 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(frozen__19, 3)
    var t1366 string
    var inline2747 string = _goml_runtime_core_bool_to_string(t1365)
    t1366 = inline2747
    var inline2744 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1366)
    _goml_runtime_core_string_println(inline2744)
    var t1367 [0]int = [0]int{}
    var empty__20 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t1367)
    var t1368 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(empty__20, ",")
    var inline2741 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1368)
    _goml_runtime_core_string_println(inline2741)
    var t1369 closure_env_main_9 = closure_env_main_9{}
    var t1370 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(t1369, p0, p1)
    }
    var t1371 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__isize(empty__20, t1370)
    var t1372 bool
    var inline2738 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__isize(t1371)
    var inline2739 bool = !inline2738
    t1372 = inline2739
    var t1373 string
    var inline2736 string = _goml_runtime_core_bool_to_string(t1372)
    t1373 = inline2736
    var inline2733 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1373)
    _goml_runtime_core_string_println(inline2733)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t1760 string
    t1760 = value__1
    _goml_runtime_core_string_println(t1760)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__isize(self__572 *_goml_vec_int, predicate__573 func(int) bool) Option__isize {
    var t1764 int
    var inline3257 int = vec_len__Vec_3int(self__572)
    t1764 = inline3257
    var t1765 FnIterator__isize
    var inline3251 int = 0
    var inline3252 *ref_int_x = ref__Ref_3int(inline3251)
    var inline3253 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3252,
        end_1: t1764,
    }
    var inline3254 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3253)
    }
    var inline3255 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3254)
    t1765 = inline3255
    var for_iter642 FnIterator__isize
    for_iter642 = t1765
    Loop_loop1767:
    for {
        var for_next643 Option__isize
        var inline3247 func() Option__isize = for_iter642.next_fn
        var inline3248 Option__isize = inline3247()
        for_next643 = inline3248
        switch for_next643._tag {
        case 0:
            break Loop_loop1767
        case 1:
            var x644 int = for_next643._v1_0
            var t1770 int = vec_get__Vec_3int(self__572, x644)
            var t1771 bool = predicate__573(t1770)
            if t1771 {
                var t1772 Option__isize = Option__isize{
                    _tag: 1,
                    _v1_0: x644,
                }
                return t1772
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return Option__isize{
        _tag: 0,
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(self__720 Option__isize, fallback__721 int) int {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 int = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline3259 int64 = int64(int(self__285))
    var inline3260 string = signed_decimal_string(inline3259)
    return inline3260
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__isize(self__588 *_goml_vec_int, compare__589 func(int, int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__isize(self__588, compare__589)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(self__631 *_goml_vec_int, separator__632 string) string {
    var t1784 int
    var inline3300 int = vec_len__Vec_3int(self__631)
    t1784 = inline3300
    var parts__633 *_goml_vec_string
    var inline3298 *_goml_vec_string = vec_with_capacity__Vec_6string(t1784)
    parts__633 = inline3298
    var t1785 int
    var inline3296 int = vec_len__Vec_3int(self__631)
    t1785 = inline3296
    var t1786 FnIterator__isize
    var inline3290 int = 0
    var inline3291 *ref_int_x = ref__Ref_3int(inline3290)
    var inline3292 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3291,
        end_1: t1785,
    }
    var inline3293 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3292)
    }
    var inline3294 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3293)
    t1786 = inline3294
    var for_iter734 FnIterator__isize
    for_iter734 = t1786
    Loop_loop1801:
    for {
        var for_next735 Option__isize
        var inline3266 func() Option__isize = for_iter734.next_fn
        var inline3267 Option__isize = inline3266()
        for_next735 = inline3267
        switch for_next735._tag {
        case 0:
            break Loop_loop1801
        case 1:
            var x736 int = for_next735._v1_0
            var t1803 int = vec_get__Vec_3int(self__631, x736)
            var t1804 string
            var inline3264 string = __goml_builtin_int_to_string(t1803)
            t1804 = inline3264
            vec_push__Vec_6string(parts__633, t1804)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1788 int
    var inline3287 int = vec_len__Vec_6string(parts__633)
    t1788 = inline3287
    var t1789 int = t1788 * 2
    var result__635 *_goml_vec_string
    var inline3285 *_goml_vec_string = vec_with_capacity__Vec_6string(t1789)
    result__635 = inline3285
    var t1790 int
    var inline3283 int = vec_len__Vec_6string(parts__633)
    t1790 = inline3283
    var t1791 FnIterator__isize
    var inline3277 int = 0
    var inline3278 *ref_int_x = ref__Ref_3int(inline3277)
    var inline3279 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3278,
        end_1: t1790,
    }
    var inline3280 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3279)
    }
    var inline3281 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3280)
    t1791 = inline3281
    var for_iter738 FnIterator__isize
    for_iter738 = t1791
    Loop_loop1794:
    for {
        var for_next739 Option__isize
        var inline3273 func() Option__isize = for_iter738.next_fn
        var inline3274 Option__isize = inline3273()
        for_next739 = inline3274
        switch for_next739._tag {
        case 0:
            break Loop_loop1794
        case 1:
            var x740 int = for_next739._v1_0
            var t1799 bool = x740 > 0
            if t1799 {
                vec_push__Vec_6string(result__635, separator__632)
            } else {}
            var t1797 string = vec_get__Vec_6string(parts__633, x740)
            vec_push__Vec_6string(result__635, t1797)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1793 string = __goml_builtin_string_concat(result__635)
    return t1793
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__isize(self__640 *_goml_vec_int) struct{} {
    var t1807 closure_env_inherent_Vec_Vec_T_dedup_T_isize_10 = closure_env_inherent_Vec_Vec_T_dedup_T_isize_10{}
    var t1808 func(int, int) bool = func(p0 int, p1 int) bool {
        return _goml_m_inherent_i_closure__en_h5b5a2c4c0a397a14d2b6a2dc409e76d2_ize__10_i_apply(t1807, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__isize(self__640, t1808)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__isize(self__598 *_goml_vec_int, compare__599 func(int) int) Option__isize {
    var low__600 int = 0
    var high__601 int
    var inline3304 int = vec_len__Vec_3int(self__598)
    high__601 = inline3304
    Loop_loop1823:
    for {
        var t1824 bool = low__600 < high__601
        if t1824 {
            var t1825 int = high__601 - low__600
            var t1826 int = t1825 / 2
            var middle__602 int = low__600 + t1826
            var t1828 int = vec_get__Vec_3int(self__598, middle__602)
            var t1829 int = compare__599(t1828)
            var t1830 bool = t1829 < 0
            if t1830 {
                var t1831 int = middle__602 + 1
                low__600 = t1831
                continue
            } else {
                high__601 = middle__602
                continue
            }
        } else {
            break Loop_loop1823
        }
    }
    var t1818 int
    var inline3302 int = vec_len__Vec_3int(self__598)
    t1818 = inline3302
    var t1819 bool = low__600 < t1818
    var jp1816 bool
    if t1819 {
        var t1820 int = vec_get__Vec_3int(self__598, low__600)
        var t1821 int = compare__599(t1820)
        var t1822 bool = t1821 == 0
        jp1816 = t1822
    } else {
        jp1816 = false
    }
    if jp1816 {
        var t1817 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: low__600,
        }
        return t1817
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__isize(self__606 *_goml_vec_int, compare__607 func(int, int) int) Option__isize {
    var t1836 bool
    var inline3318 int = vec_len__Vec_3int(self__606)
    var inline3319 bool = inline3318 == 0
    t1836 = inline3319
    if t1836 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var best__608 int = vec_get__Vec_3int(self__606, 0)
        var t1837 int
        var inline3316 int = vec_len__Vec_3int(self__606)
        t1837 = inline3316
        var t1838 FnIterator__isize
        var inline3310 int = 1
        var inline3311 *ref_int_x = ref__Ref_3int(inline3310)
        var inline3312 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3311,
            end_1: t1837,
        }
        var inline3313 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3312)
        }
        var inline3314 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3313)
        t1838 = inline3314
        var for_iter711 FnIterator__isize
        for_iter711 = t1838
        Loop_loop1841:
        for {
            var for_next712 Option__isize
            var inline3306 func() Option__isize = for_iter711.next_fn
            var inline3307 Option__isize = inline3306()
            for_next712 = inline3307
            switch for_next712._tag {
            case 0:
                break Loop_loop1841
            case 1:
                var x713 int = for_next712._v1_0
                var value__610 int = vec_get__Vec_3int(self__606, x713)
                var t1844 int = compare__607(value__610, best__608)
                var t1845 bool = t1844 < 0
                if t1845 {
                    best__608 = value__610
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1840 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: best__608,
        }
        return t1840
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__isize(self__611 *_goml_vec_int, compare__612 func(int, int) int) Option__isize {
    var t1850 bool
    var inline3333 int = vec_len__Vec_3int(self__611)
    var inline3334 bool = inline3333 == 0
    t1850 = inline3334
    if t1850 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var best__613 int = vec_get__Vec_3int(self__611, 0)
        var t1851 int
        var inline3331 int = vec_len__Vec_3int(self__611)
        t1851 = inline3331
        var t1852 FnIterator__isize
        var inline3325 int = 1
        var inline3326 *ref_int_x = ref__Ref_3int(inline3325)
        var inline3327 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3326,
            end_1: t1851,
        }
        var inline3328 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3327)
        }
        var inline3329 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3328)
        t1852 = inline3329
        var for_iter716 FnIterator__isize
        for_iter716 = t1852
        Loop_loop1855:
        for {
            var for_next717 Option__isize
            var inline3321 func() Option__isize = for_iter716.next_fn
            var inline3322 Option__isize = inline3321()
            for_next717 = inline3322
            switch for_next717._tag {
            case 0:
                break Loop_loop1855
            case 1:
                var x718 int = for_next717._v1_0
                var value__615 int = vec_get__Vec_3int(self__611, x718)
                var t1858 int = compare__612(value__615, best__613)
                var t1859 bool = t1858 > 0
                if t1859 {
                    best__613 = value__615
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1854 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: best__613,
        }
        return t1854
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(self__590 *_goml_vec_string, compare__591 func(string, string) Ordering) struct{} {
    var t1864 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11{
        compare_0: compare__591,
    }
    var t1865 func(string, string) int = func(p0 string, p1 string) int {
        return _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(t1864, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__590, t1865)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__631 *_goml_vec_string, separator__632 string) string {
    var t1869 int
    var inline3378 int = vec_len__Vec_6string(self__631)
    t1869 = inline3378
    var parts__633 *_goml_vec_string
    var inline3376 *_goml_vec_string = vec_with_capacity__Vec_6string(t1869)
    parts__633 = inline3376
    var t1870 int
    var inline3374 int = vec_len__Vec_6string(self__631)
    t1870 = inline3374
    var t1871 FnIterator__isize
    var inline3368 int = 0
    var inline3369 *ref_int_x = ref__Ref_3int(inline3368)
    var inline3370 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3369,
        end_1: t1870,
    }
    var inline3371 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3370)
    }
    var inline3372 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3371)
    t1871 = inline3372
    var for_iter734 FnIterator__isize
    for_iter734 = t1871
    Loop_loop1886:
    for {
        var for_next735 Option__isize
        var inline3344 func() Option__isize = for_iter734.next_fn
        var inline3345 Option__isize = inline3344()
        for_next735 = inline3345
        switch for_next735._tag {
        case 0:
            break Loop_loop1886
        case 1:
            var x736 int = for_next735._v1_0
            var t1888 string = vec_get__Vec_6string(self__631, x736)
            var t1889 string
            t1889 = t1888
            vec_push__Vec_6string(parts__633, t1889)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1873 int
    var inline3365 int = vec_len__Vec_6string(parts__633)
    t1873 = inline3365
    var t1874 int = t1873 * 2
    var result__635 *_goml_vec_string
    var inline3363 *_goml_vec_string = vec_with_capacity__Vec_6string(t1874)
    result__635 = inline3363
    var t1875 int
    var inline3361 int = vec_len__Vec_6string(parts__633)
    t1875 = inline3361
    var t1876 FnIterator__isize
    var inline3355 int = 0
    var inline3356 *ref_int_x = ref__Ref_3int(inline3355)
    var inline3357 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3356,
        end_1: t1875,
    }
    var inline3358 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3357)
    }
    var inline3359 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3358)
    t1876 = inline3359
    var for_iter738 FnIterator__isize
    for_iter738 = t1876
    Loop_loop1879:
    for {
        var for_next739 Option__isize
        var inline3351 func() Option__isize = for_iter738.next_fn
        var inline3352 Option__isize = inline3351()
        for_next739 = inline3352
        switch for_next739._tag {
        case 0:
            break Loop_loop1879
        case 1:
            var x740 int = for_next739._v1_0
            var t1884 bool = x740 > 0
            if t1884 {
                vec_push__Vec_6string(result__635, separator__632)
            } else {}
            var t1882 string = vec_get__Vec_6string(parts__633, x740)
            vec_push__Vec_6string(result__635, t1882)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1878 string = __goml_builtin_string_concat(result__635)
    return t1878
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(self__643 []int, expected__644 int) bool {
    var index__645 int = 0
    Loop_loop1907:
    for {
        var t1908 int
        var inline3387 int = len(self__643)
        t1908 = inline3387
        var t1909 bool = index__645 < t1908
        if t1909 {
            var t1913 int = self__643[index__645]
            var t1914 bool
            var inline3385 bool = t1913 == expected__644
            t1914 = inline3385
            if t1914 {
                return true
            } else {
                var compound_old749 int = index__645
                var compound_value750 int = 1
                var t1911 int = compound_old749 + compound_value750
                index__645 = t1911
                continue
            }
        } else {
            break Loop_loop1907
        }
    }
    return false
}

func _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(self__646 FrozenVec__isize, expected__647 int) bool {
    var index__648 int = 0
    Loop_loop1925:
    for {
        var t1926 int
        var inline3394 *_goml_vec_int = self__646.values
        var inline3395 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inline3394)
        t1926 = inline3395
        var t1927 bool = index__648 < t1926
        if t1927 {
            var t1931 int
            var inline3391 *_goml_vec_int = self__646.values
            var inline3392 int = vec_get__Vec_3int(inline3391, index__648)
            t1931 = inline3392
            var t1932 bool
            var inline3389 bool = t1931 == expected__647
            t1932 = inline3389
            if t1932 {
                return true
            } else {
                var compound_old754 int = index__648
                var compound_value755 int = 1
                var t1929 int = compound_old754 + compound_value755
                index__648 = t1929
                continue
            }
        } else {
            break Loop_loop1925
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__526 *_goml_vec_int) int {
    var t1946 int = vec_len__Vec_3int(self__526)
    return t1946
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1955 int64 = int64(int(value__222))
    var inline3400 bool = t1955 < 0
    if inline3400 {
        var inline3401 uint64 = uint64(int64(t1955))
        var inline3402 uint64 = 0 - inline3401
        var inline3403 string = decimal_string(inline3402)
        var inline3404 string = "-" + inline3403
        return inline3404
    } else {
        var inline3405 uint64 = uint64(int64(t1955))
        var inline3406 string = decimal_string(inline3405)
        return inline3406
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__isize(self__575 *_goml_vec_int, compare__576 func(int, int) int) struct{} {
    var length__577 int
    var inline3436 int = vec_len__Vec_3int(self__575)
    length__577 = inline3436
    var t2018 bool = length__577 < 2
    if t2018 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_int
        var inline3434 *_goml_vec_int = vec_with_capacity__Vec_3int(length__577)
        buffer__578 = inline3434
        var t1959 FnIterator__isize
        var inline3428 int = 0
        var inline3429 *ref_int_x = ref__Ref_3int(inline3428)
        var inline3430 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3429,
            end_1: length__577,
        }
        var inline3431 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3430)
        }
        var inline3432 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3431)
        t1959 = inline3432
        var for_iter647 FnIterator__isize
        for_iter647 = t1959
        Loop_loop2014:
        for {
            var for_next648 Option__isize
            var inline3410 func() Option__isize = for_iter647.next_fn
            var inline3411 Option__isize = inline3410()
            for_next648 = inline3411
            switch for_next648._tag {
            case 0:
                break Loop_loop2014
            case 1:
                var x649 int = for_next648._v1_0
                var t2016 int = vec_get__Vec_3int(self__575, x649)
                vec_push__Vec_3int(buffer__578, t2016)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop1962:
        for {
            var t1963 bool = width__580 < length__577
            if t1963 {
                var left__581 int = 0
                Loop_loop1975:
                for {
                    var t1976 bool = left__581 < length__577
                    if t1976 {
                        var t1977 int = left__581 + width__580
                        var middle__582 int
                        var inline3415 bool = t1977 < length__577
                        if inline3415 {
                            middle__582 = t1977
                        } else {
                            middle__582 = length__577
                        }
                        var t1978 int = middle__582 + width__580
                        var right__583 int
                        var inline3413 bool = t1978 < length__577
                        if inline3413 {
                            right__583 = t1978
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop1996:
                        for {
                            var t2012 bool = first__584 < middle__582
                            var jp1998 bool
                            if t2012 {
                                var t2013 bool = second__585 < right__583
                                jp1998 = t2013
                            } else {
                                jp1998 = false
                            }
                            if jp1998 {
                                var t2002 int = vec_get__Vec_3int(self__575, first__584)
                                var t2003 int = vec_get__Vec_3int(self__575, second__585)
                                var t2004 int = compare__576(t2002, t2003)
                                var t2005 bool = t2004 <= 0
                                if t2005 {
                                    var index652 int = output__586
                                    vec_get__Vec_3int(buffer__578, index652)
                                    var value654 int = vec_get__Vec_3int(self__575, first__584)
                                    vec_set__Vec_3int(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2007 int = compound_old656 + compound_value657
                                    first__584 = t2007
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_3int(buffer__578, index660)
                                    var value662 int = vec_get__Vec_3int(self__575, second__585)
                                    vec_set__Vec_3int(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2010 int = compound_old664 + compound_value665
                                    second__585 = t2010
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2000 int = compound_old668 + compound_value669
                                output__586 = t2000
                                continue
                            } else {
                                break Loop_loop1996
                            }
                        }
                        Loop_loop1989:
                        for {
                            var t1990 bool = first__584 < middle__582
                            if t1990 {
                                var index673 int = output__586
                                vec_get__Vec_3int(buffer__578, index673)
                                var value675 int = vec_get__Vec_3int(self__575, first__584)
                                vec_set__Vec_3int(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t1992 int = compound_old677 + compound_value678
                                first__584 = t1992
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t1994 int = compound_old680 + compound_value681
                                output__586 = t1994
                                continue
                            } else {
                                break Loop_loop1989
                            }
                        }
                        Loop_loop1982:
                        for {
                            var t1983 bool = second__585 < right__583
                            if t1983 {
                                var index685 int = output__586
                                vec_get__Vec_3int(buffer__578, index685)
                                var value687 int = vec_get__Vec_3int(self__575, second__585)
                                vec_set__Vec_3int(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t1985 int = compound_old689 + compound_value690
                                second__585 = t1985
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t1987 int = compound_old692 + compound_value693
                                output__586 = t1987
                                continue
                            } else {
                                break Loop_loop1982
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop1975
                    }
                }
                var t1965 FnIterator__isize
                var inline3421 int = 0
                var inline3422 *ref_int_x = ref__Ref_3int(inline3421)
                var inline3423 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3422,
                    end_1: length__577,
                }
                var inline3424 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3423)
                }
                var inline3425 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3424)
                t1965 = inline3425
                var for_iter698 FnIterator__isize
                for_iter698 = t1965
                Loop_loop1972:
                for {
                    var for_next699 Option__isize
                    var inline3417 func() Option__isize = for_iter698.next_fn
                    var inline3418 Option__isize = inline3417()
                    for_next699 = inline3418
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop1972
                    case 1:
                        var x700 int = for_next699._v1_0
                        vec_get__Vec_3int(self__575, x700)
                        var value704 int = vec_get__Vec_3int(buffer__578, x700)
                        vec_set__Vec_3int(self__575, x700, value704)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t1969 int = length__577 / 2
                var t1970 bool = width__580 > t1969
                var jp1968 int
                if t1970 {
                    jp1968 = length__577
                } else {
                    var t1971 int = width__580 * 2
                    jp1968 = t1971
                }
                width__580 = jp1968
                continue
            } else {
                break Loop_loop1962
            }
        }
        return struct{}{}
    }
}

func __goml_builtin_string_concat(values__215 *_goml_vec_string) string {
    var length__216 int = 0
    var value_index__217 int = 0
    Loop_loop2046:
    for {
        var t2047 int
        var inline3443 int = vec_len__Vec_6string(values__215)
        t2047 = inline3443
        var t2048 bool = value_index__217 < t2047
        if t2048 {
            var compound_old365 int = length__216
            var t2049 string = vec_get__Vec_6string(values__215, value_index__217)
            var compound_value366 int
            var inline3441 int = _goml_runtime_core_string_len(t2049)
            compound_value366 = inline3441
            var t2050 int = compound_old365 + compound_value366
            length__216 = t2050
            var compound_old368 int = value_index__217
            var compound_value369 int = 1
            var t2052 int = compound_old368 + compound_value369
            value_index__217 = t2052
            continue
        } else {
            break Loop_loop2046
        }
    }
    var bytes__218 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(length__216)
    value_index__217 = 0
    Loop_loop2034:
    for {
        var t2035 int
        var inline3451 int = vec_len__Vec_6string(values__215)
        t2035 = inline3451
        var t2036 bool = value_index__217 < t2035
        if t2036 {
            var value__219 string = vec_get__Vec_6string(values__215, value_index__217)
            var byte_index__220 int = 0
            Loop_loop2040:
            for {
                var t2041 int
                var inline3449 int = _goml_runtime_core_string_len(value__219)
                t2041 = inline3449
                var t2042 bool = byte_index__220 < t2041
                if t2042 {
                    var t2043 uint8
                    var inline3447 uint8 = _goml_runtime_core_string_byte_get(value__219, byte_index__220)
                    t2043 = inline3447
                    vec_push__Vec_5uint8(bytes__218, t2043)
                    var compound_old374 int = byte_index__220
                    var compound_value375 int = 1
                    var t2044 int = compound_old374 + compound_value375
                    byte_index__220 = t2044
                    continue
                } else {
                    break Loop_loop2040
                }
            }
            var compound_old378 int = value_index__217
            var compound_value379 int = 1
            var t2038 int = compound_old378 + compound_value379
            value_index__217 = t2038
            continue
        } else {
            break Loop_loop2034
        }
    }
    var mtmp382 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__218)
    var x384 string = mtmp382._1
    return x384
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__isize(self__624 *_goml_vec_int, equal__625 func(int, int) bool) struct{} {
    var t2073 int
    var inline3467 int = vec_len__Vec_3int(self__624)
    t2073 = inline3467
    var t2074 bool = t2073 < 2
    if t2074 {
        return struct{}{}
    } else {
        var output__626 int = 1
        var t2059 int
        var inline3465 int = vec_len__Vec_3int(self__624)
        t2059 = inline3465
        var t2060 FnIterator__isize
        var inline3459 int = 1
        var inline3460 *ref_int_x = ref__Ref_3int(inline3459)
        var inline3461 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3460,
            end_1: t2059,
        }
        var inline3462 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3461)
        }
        var inline3463 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3462)
        t2060 = inline3463
        var for_iter722 FnIterator__isize
        for_iter722 = t2060
        Loop_loop2063:
        for {
            var for_next723 Option__isize
            var inline3453 func() Option__isize = for_iter722.next_fn
            var inline3454 Option__isize = inline3453()
            for_next723 = inline3454
            switch for_next723._tag {
            case 0:
                break Loop_loop2063
            case 1:
                var x724 int = for_next723._v1_0
                var value__628 int = vec_get__Vec_3int(self__624, x724)
                var t2066 int = output__626 - 1
                var t2067 int = vec_get__Vec_3int(self__624, t2066)
                var t2068 bool = equal__625(t2067, value__628)
                var t2069 bool = !t2068
                if t2069 {
                    var index726 int = output__626
                    vec_get__Vec_3int(self__624, index726)
                    vec_set__Vec_3int(self__624, index726, value__628)
                    var compound_old730 int = output__626
                    var compound_value731 int = 1
                    var t2071 int = compound_old730 + compound_value731
                    output__626 = t2071
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        vec_truncate__Vec_3int(self__624, output__626)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T___o_isize_c_isize_q_(self__588 *_goml_vec_Tuple2_3int_3int, compare__589 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_isize_c_isize_q_(self__588, compare__589)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(self__517 *_goml_vec_int) *_goml_vec_int {
    var t2092 int
    var inline3475 int = vec_len__Vec_3int(self__517)
    t2092 = inline3475
    var result__518 *_goml_vec_int
    var inline3473 *_goml_vec_int = vec_with_capacity__Vec_3int(t2092)
    result__518 = inline3473
    var index__519 int = 0
    Loop_loop2094:
    for {
        var t2095 int
        var inline3471 int = vec_len__Vec_3int(self__517)
        t2095 = inline3471
        var t2096 bool = index__519 < t2095
        if t2096 {
            var t2097 int = vec_get__Vec_3int(self__517, index__519)
            vec_push__Vec_3int(result__518, t2097)
            var compound_old581 int = index__519
            var compound_value582 int = 1
            var t2098 int = compound_old581 + compound_value582
            index__519 = t2098
            continue
        } else {
            break Loop_loop2094
        }
    }
    return result__518
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__isize(self__718 Option__isize) bool {
    switch self__718._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__507 func() Option__isize) FnIterator__isize {
    var t2114 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__507,
    }
    return t2114
}

func signed_decimal_string(value__214 int64) string {
    var t2119 bool = value__214 < 0
    if t2119 {
        var t2120 uint64 = uint64(int64(value__214))
        var t2121 uint64 = 0 - t2120
        var t2122 string = decimal_string(t2121)
        var t2123 string = "-" + t2122
        return t2123
    } else {
        var t2124 uint64 = uint64(int64(value__214))
        var t2125 string = decimal_string(t2124)
        return t2125
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__575 *_goml_vec_string, compare__576 func(string, string) int) struct{} {
    var length__577 int
    var inline3507 int = vec_len__Vec_6string(self__575)
    length__577 = inline3507
    var t2207 bool = length__577 < 2
    if t2207 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_string
        var inline3505 *_goml_vec_string = vec_with_capacity__Vec_6string(length__577)
        buffer__578 = inline3505
        var t2148 FnIterator__isize
        var inline3499 int = 0
        var inline3500 *ref_int_x = ref__Ref_3int(inline3499)
        var inline3501 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3500,
            end_1: length__577,
        }
        var inline3502 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3501)
        }
        var inline3503 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3502)
        t2148 = inline3503
        var for_iter647 FnIterator__isize
        for_iter647 = t2148
        Loop_loop2203:
        for {
            var for_next648 Option__isize
            var inline3481 func() Option__isize = for_iter647.next_fn
            var inline3482 Option__isize = inline3481()
            for_next648 = inline3482
            switch for_next648._tag {
            case 0:
                break Loop_loop2203
            case 1:
                var x649 int = for_next648._v1_0
                var t2205 string = vec_get__Vec_6string(self__575, x649)
                vec_push__Vec_6string(buffer__578, t2205)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop2151:
        for {
            var t2152 bool = width__580 < length__577
            if t2152 {
                var left__581 int = 0
                Loop_loop2164:
                for {
                    var t2165 bool = left__581 < length__577
                    if t2165 {
                        var t2166 int = left__581 + width__580
                        var middle__582 int
                        var inline3486 bool = t2166 < length__577
                        if inline3486 {
                            middle__582 = t2166
                        } else {
                            middle__582 = length__577
                        }
                        var t2167 int = middle__582 + width__580
                        var right__583 int
                        var inline3484 bool = t2167 < length__577
                        if inline3484 {
                            right__583 = t2167
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop2185:
                        for {
                            var t2201 bool = first__584 < middle__582
                            var jp2187 bool
                            if t2201 {
                                var t2202 bool = second__585 < right__583
                                jp2187 = t2202
                            } else {
                                jp2187 = false
                            }
                            if jp2187 {
                                var t2191 string = vec_get__Vec_6string(self__575, first__584)
                                var t2192 string = vec_get__Vec_6string(self__575, second__585)
                                var t2193 int = compare__576(t2191, t2192)
                                var t2194 bool = t2193 <= 0
                                if t2194 {
                                    var index652 int = output__586
                                    vec_get__Vec_6string(buffer__578, index652)
                                    var value654 string = vec_get__Vec_6string(self__575, first__584)
                                    vec_set__Vec_6string(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2196 int = compound_old656 + compound_value657
                                    first__584 = t2196
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_6string(buffer__578, index660)
                                    var value662 string = vec_get__Vec_6string(self__575, second__585)
                                    vec_set__Vec_6string(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2199 int = compound_old664 + compound_value665
                                    second__585 = t2199
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2189 int = compound_old668 + compound_value669
                                output__586 = t2189
                                continue
                            } else {
                                break Loop_loop2185
                            }
                        }
                        Loop_loop2178:
                        for {
                            var t2179 bool = first__584 < middle__582
                            if t2179 {
                                var index673 int = output__586
                                vec_get__Vec_6string(buffer__578, index673)
                                var value675 string = vec_get__Vec_6string(self__575, first__584)
                                vec_set__Vec_6string(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t2181 int = compound_old677 + compound_value678
                                first__584 = t2181
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t2183 int = compound_old680 + compound_value681
                                output__586 = t2183
                                continue
                            } else {
                                break Loop_loop2178
                            }
                        }
                        Loop_loop2171:
                        for {
                            var t2172 bool = second__585 < right__583
                            if t2172 {
                                var index685 int = output__586
                                vec_get__Vec_6string(buffer__578, index685)
                                var value687 string = vec_get__Vec_6string(self__575, second__585)
                                vec_set__Vec_6string(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t2174 int = compound_old689 + compound_value690
                                second__585 = t2174
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t2176 int = compound_old692 + compound_value693
                                output__586 = t2176
                                continue
                            } else {
                                break Loop_loop2171
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop2164
                    }
                }
                var t2154 FnIterator__isize
                var inline3492 int = 0
                var inline3493 *ref_int_x = ref__Ref_3int(inline3492)
                var inline3494 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3493,
                    end_1: length__577,
                }
                var inline3495 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3494)
                }
                var inline3496 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3495)
                t2154 = inline3496
                var for_iter698 FnIterator__isize
                for_iter698 = t2154
                Loop_loop2161:
                for {
                    var for_next699 Option__isize
                    var inline3488 func() Option__isize = for_iter698.next_fn
                    var inline3489 Option__isize = inline3488()
                    for_next699 = inline3489
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop2161
                    case 1:
                        var x700 int = for_next699._v1_0
                        vec_get__Vec_6string(self__575, x700)
                        var value704 string = vec_get__Vec_6string(buffer__578, x700)
                        vec_set__Vec_6string(self__575, x700, value704)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t2158 int = length__577 / 2
                var t2159 bool = width__580 > t2158
                var jp2157 int
                if t2159 {
                    jp2157 = length__577
                } else {
                    var t2160 int = width__580 * 2
                    jp2157 = t2160
                }
                width__580 = jp2157
                continue
            } else {
                break Loop_loop2151
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_isize_c_isize_q_(self__575 *_goml_vec_Tuple2_3int_3int, compare__576 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    var length__577 int
    var inline3537 int = vec_len__Vec_16Tuple2_3int_3int(self__575)
    length__577 = inline3537
    var t2269 bool = length__577 < 2
    if t2269 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_Tuple2_3int_3int
        var inline3535 *_goml_vec_Tuple2_3int_3int = vec_with_capacity__Vec_16Tuple2_3int_3int(length__577)
        buffer__578 = inline3535
        var t2210 FnIterator__isize
        var inline3529 int = 0
        var inline3530 *ref_int_x = ref__Ref_3int(inline3529)
        var inline3531 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3530,
            end_1: length__577,
        }
        var inline3532 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3531)
        }
        var inline3533 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3532)
        t2210 = inline3533
        var for_iter647 FnIterator__isize
        for_iter647 = t2210
        Loop_loop2265:
        for {
            var for_next648 Option__isize
            var inline3511 func() Option__isize = for_iter647.next_fn
            var inline3512 Option__isize = inline3511()
            for_next648 = inline3512
            switch for_next648._tag {
            case 0:
                break Loop_loop2265
            case 1:
                var x649 int = for_next648._v1_0
                var t2267 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, x649)
                vec_push__Vec_16Tuple2_3int_3int(buffer__578, t2267)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop2213:
        for {
            var t2214 bool = width__580 < length__577
            if t2214 {
                var left__581 int = 0
                Loop_loop2226:
                for {
                    var t2227 bool = left__581 < length__577
                    if t2227 {
                        var t2228 int = left__581 + width__580
                        var middle__582 int
                        var inline3516 bool = t2228 < length__577
                        if inline3516 {
                            middle__582 = t2228
                        } else {
                            middle__582 = length__577
                        }
                        var t2229 int = middle__582 + width__580
                        var right__583 int
                        var inline3514 bool = t2229 < length__577
                        if inline3514 {
                            right__583 = t2229
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop2247:
                        for {
                            var t2263 bool = first__584 < middle__582
                            var jp2249 bool
                            if t2263 {
                                var t2264 bool = second__585 < right__583
                                jp2249 = t2264
                            } else {
                                jp2249 = false
                            }
                            if jp2249 {
                                var t2253 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                var t2254 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                var t2255 int = compare__576(t2253, t2254)
                                var t2256 bool = t2255 <= 0
                                if t2256 {
                                    var index652 int = output__586
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__578, index652)
                                    var value654 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2258 int = compound_old656 + compound_value657
                                    first__584 = t2258
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__578, index660)
                                    var value662 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2261 int = compound_old664 + compound_value665
                                    second__585 = t2261
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2251 int = compound_old668 + compound_value669
                                output__586 = t2251
                                continue
                            } else {
                                break Loop_loop2247
                            }
                        }
                        Loop_loop2240:
                        for {
                            var t2241 bool = first__584 < middle__582
                            if t2241 {
                                var index673 int = output__586
                                vec_get__Vec_16Tuple2_3int_3int(buffer__578, index673)
                                var value675 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t2243 int = compound_old677 + compound_value678
                                first__584 = t2243
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t2245 int = compound_old680 + compound_value681
                                output__586 = t2245
                                continue
                            } else {
                                break Loop_loop2240
                            }
                        }
                        Loop_loop2233:
                        for {
                            var t2234 bool = second__585 < right__583
                            if t2234 {
                                var index685 int = output__586
                                vec_get__Vec_16Tuple2_3int_3int(buffer__578, index685)
                                var value687 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t2236 int = compound_old689 + compound_value690
                                second__585 = t2236
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t2238 int = compound_old692 + compound_value693
                                output__586 = t2238
                                continue
                            } else {
                                break Loop_loop2233
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop2226
                    }
                }
                var t2216 FnIterator__isize
                var inline3522 int = 0
                var inline3523 *ref_int_x = ref__Ref_3int(inline3522)
                var inline3524 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3523,
                    end_1: length__577,
                }
                var inline3525 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3524)
                }
                var inline3526 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3525)
                t2216 = inline3526
                var for_iter698 FnIterator__isize
                for_iter698 = t2216
                Loop_loop2223:
                for {
                    var for_next699 Option__isize
                    var inline3518 func() Option__isize = for_iter698.next_fn
                    var inline3519 Option__isize = inline3518()
                    for_next699 = inline3519
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop2223
                    case 1:
                        var x700 int = for_next699._v1_0
                        vec_get__Vec_16Tuple2_3int_3int(self__575, x700)
                        var value704 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(buffer__578, x700)
                        vec_set__Vec_16Tuple2_3int_3int(self__575, x700, value704)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                }
                var t2220 int = length__577 / 2
                var t2221 bool = width__580 > t2220
                var jp2219 int
                if t2221 {
                    jp2219 = length__577
                } else {
                    var t2222 int = width__580 * 2
                    jp2219 = t2222
                }
                width__580 = jp2219
                continue
            } else {
                break Loop_loop2213
            }
        }
        return struct{}{}
    }
}

func decimal_string(value__208 uint64) string {
    var t2292 bool = value__208 == 0
    if t2292 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop2285:
        for {
            var t2286 bool = remaining__210 > 0
            if t2286 {
                var t2287_rhs uint64 = 10
                var t2287 uint64 = remaining__210 % t2287_rhs
                var t2288 uint8 = uint8(uint64(t2287))
                var t2289 uint8 = t2288 + 48
                vec_push__Vec_5uint8(reversed__209, t2289)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t2290 uint64 = compound_old353 / compound_value354
                remaining__210 = t2290
                continue
            } else {
                break Loop_loop2285
            }
        }
        var t2274 int
        var inline3547 int = vec_len__Vec_5uint8(reversed__209)
        t2274 = inline3547
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t2274)
        var offset__212 int = 0
        Loop_loop2276:
        for {
            var t2277 int
            var inline3545 int = vec_len__Vec_5uint8(reversed__209)
            t2277 = inline3545
            var t2278 bool = offset__212 < t2277
            if t2278 {
                var t2279 int
                var inline3543 int = vec_len__Vec_5uint8(reversed__209)
                t2279 = inline3543
                var t2280 int = t2279 - offset__212
                var t2281 int = t2280 - 1
                var t2282 uint8 = vec_get__Vec_5uint8(reversed__209, t2281)
                vec_push__Vec_5uint8(bytes__211, t2282)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t2283 int = compound_old358 + compound_value359
                offset__212 = t2283
                continue
            } else {
                break Loop_loop2276
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env815 closure_env_main_0, value__1 int) bool {
    var t2306 bool = value__1 == 5
    return t2306
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env816 closure_env_main_1, value__2 int) bool {
    var t2309 bool = value__2 == 99
    return t2309
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env817 closure_env_main_2, left__3 int, right__4 int) int {
    var t2312 int = left__3 - right__4
    return t2312
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env818 closure_env_main_3, value__6 int) int {
    var t2315 int = value__6 - 4
    return t2315
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env819 closure_env_main_4, value__7 int) int {
    var t2318 int = value__7 - 3
    return t2318
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env820 closure_env_main_5, left__8 int, right__9 int) int {
    var t2321 int = left__8 - right__9
    return t2321
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env821 closure_env_main_6, left__10 int, right__11 int) int {
    var t2324 int = left__10 - right__11
    return t2324
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env822 closure_env_main_7, left__13 string, right__14 string) Ordering {
    var inline3549 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(left__13, right__14)
    return inline3549
}

func _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(env823 closure_env_main_8, left__16 Tuple2_3int_3int, right__17 Tuple2_3int_3int) Ordering {
    var t2330 int = left__16._0
    var t2331 int = right__17._0
    var inline3551 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(t2330, t2331)
    return inline3551
}

func _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(env824 closure_env_main_9, left__21 int, right__22 int) int {
    var t2335 int = left__21 - right__22
    return t2335
}

func _goml_m_inherent_i_closure__en_h5b5a2c4c0a397a14d2b6a2dc409e76d2_ize__10_i_apply(env825 closure_env_inherent_Vec_Vec_T_dedup_T_isize_10, left__641 int, right__642 int) bool {
    var inline3553 bool = left__641 == right__642
    return inline3553
}

func _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(env826 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11, left__592 string, right__593 string) int {
    var compare__591 func(string, string) Ordering = env826.compare_0
    var t2341 Ordering = compare__591(left__592, right__593)
    switch t2341 {
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

func _goml_m_inherent_i_closure__en_hc4b58ff73aa2ff3a45662a7349486da0_ize__12_i_apply(env827 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12, left__592 Tuple2_3int_3int, right__593 Tuple2_3int_3int) int {
    var compare__591 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering = env827.compare_0
    var t2345 Ordering = compare__591(left__592, right__593)
    switch t2345 {
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

func _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(env828 closure_env_goml_builtin_range_13) Option__isize {
    var current__758 *ref_int_x = env828.current_0
    var end__757 int = env828.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t2351 bool = value__759 < end__757
    if t2351 {
        var t2352 int = value__759 + 1
        ref_set__Ref_3int(current__758, t2352)
        var t2353 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t2353
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
