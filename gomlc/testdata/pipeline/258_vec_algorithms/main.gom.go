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

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
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
    var inline2782 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1346)
    _goml_runtime_core_string_println(inline2782)
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
    _goml_m_inherent_i_Vec_i_Vec_l_hfde7b787e35869a5469e590f5ee0e2ea_size_c_isize_q_(pairs__15, t1352)
    var t1353 int
    var inline2780 int = vec_len__Vec_16Tuple2_3int_3int(pairs__15)
    t1353 = inline2780
    var t1354 string
    var inline2778 string = __goml_builtin_int_to_string(t1353)
    t1354 = inline2778
    var inline2775 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1354)
    _goml_runtime_core_string_println(inline2775)
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
    var inline2771 int = 0
    var inline2772 int = 2
    var inline2773 []int = t1356.items[inline2771:inline2772]
    view__18 = inline2773
    var t1357 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(view__18, 8)
    var t1358 string
    var inline2769 string = _goml_runtime_core_bool_to_string(t1357)
    t1358 = inline2769
    var inline2766 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1358)
    _goml_runtime_core_string_println(inline2766)
    var t1359 bool = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(view__18, 9)
    var t1360 string
    var inline2764 string = _goml_runtime_core_bool_to_string(t1359)
    t1360 = inline2764
    var inline2761 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1360)
    _goml_runtime_core_string_println(inline2761)
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
    var inline2758 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(t1362)
    var inline2759 FrozenVec__isize = FrozenVec__isize{
        values: inline2758,
    }
    frozen__19 = inline2759
    var t1363 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(frozen__19, 1)
    var t1364 string
    var inline2756 string = _goml_runtime_core_bool_to_string(t1363)
    t1364 = inline2756
    var inline2753 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1364)
    _goml_runtime_core_string_println(inline2753)
    var t1365 bool = _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(frozen__19, 3)
    var t1366 string
    var inline2751 string = _goml_runtime_core_bool_to_string(t1365)
    t1366 = inline2751
    var inline2748 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1366)
    _goml_runtime_core_string_println(inline2748)
    var empty__20 *_goml_vec_int
    var inline2746 *_goml_vec_int = vec_new__Vec_3int()
    empty__20 = inline2746
    var t1367 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(empty__20, ",")
    var inline2743 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1367)
    _goml_runtime_core_string_println(inline2743)
    var t1368 closure_env_main_9 = closure_env_main_9{}
    var t1369 func(int, int) int = func(p0 int, p1 int) int {
        return _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(t1368, p0, p1)
    }
    var t1370 Option__isize = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__isize(empty__20, t1369)
    var t1371 bool
    var inline2740 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__isize(t1370)
    var inline2741 bool = !inline2740
    t1371 = inline2741
    var t1372 string
    var inline2738 string = _goml_runtime_core_bool_to_string(t1371)
    t1372 = inline2738
    var inline2735 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1372)
    _goml_runtime_core_string_println(inline2735)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t1759 string
    t1759 = value__1
    _goml_runtime_core_string_println(t1759)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_position____T__isize(self__572 *_goml_vec_int, predicate__573 func(int) bool) Option__isize {
    var t1763 int
    var inline3260 int = vec_len__Vec_3int(self__572)
    t1763 = inline3260
    var t1764 FnIterator__isize
    var inline3254 int = 0
    var inline3255 *ref_int_x = ref__Ref_3int(inline3254)
    var inline3256 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3255,
        end_1: t1763,
    }
    var inline3257 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3256)
    }
    var inline3258 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3257)
    t1764 = inline3258
    var for_iter642 FnIterator__isize
    for_iter642 = t1764
    Loop_loop1766:
    for {
        var for_next643 Option__isize
        var inline3250 func() Option__isize = for_iter642.next_fn
        var inline3251 Option__isize = inline3250()
        for_next643 = inline3251
        switch for_next643._tag {
        case 0:
            break Loop_loop1766
        case 1:
            var x644 int = for_next643._v1_0
            var t1769 int = vec_get__Vec_3int(self__572, x644)
            var t1770 bool = predicate__573(t1769)
            if t1770 {
                var t1771 Option__isize = Option__isize{
                    _tag: 1,
                    _v1_0: x644,
                }
                return t1771
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
    var inline3262 int64 = int64(int(self__285))
    var inline3263 string = signed_decimal_string(inline3262)
    return inline3263
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by____T__isize(self__588 *_goml_vec_int, compare__589 func(int, int) int) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__isize(self__588, compare__589)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__isize(self__631 *_goml_vec_int, separator__632 string) string {
    var t1783 int
    var inline3303 int = vec_len__Vec_3int(self__631)
    t1783 = inline3303
    var parts__633 *_goml_vec_string
    var inline3301 *_goml_vec_string = vec_with_capacity__Vec_6string(t1783)
    parts__633 = inline3301
    var t1784 int
    var inline3299 int = vec_len__Vec_3int(self__631)
    t1784 = inline3299
    var t1785 FnIterator__isize
    var inline3293 int = 0
    var inline3294 *ref_int_x = ref__Ref_3int(inline3293)
    var inline3295 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3294,
        end_1: t1784,
    }
    var inline3296 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3295)
    }
    var inline3297 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3296)
    t1785 = inline3297
    var for_iter734 FnIterator__isize
    for_iter734 = t1785
    Loop_loop1800:
    for {
        var for_next735 Option__isize
        var inline3269 func() Option__isize = for_iter734.next_fn
        var inline3270 Option__isize = inline3269()
        for_next735 = inline3270
        switch for_next735._tag {
        case 0:
            break Loop_loop1800
        case 1:
            var x736 int = for_next735._v1_0
            var t1802 int = vec_get__Vec_3int(self__631, x736)
            var t1803 string
            var inline3267 string = __goml_builtin_int_to_string(t1802)
            t1803 = inline3267
            vec_push__Vec_6string(parts__633, t1803)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1787 int
    var inline3290 int = vec_len__Vec_6string(parts__633)
    t1787 = inline3290
    var t1788 int = t1787 * 2
    var result__635 *_goml_vec_string
    var inline3288 *_goml_vec_string = vec_with_capacity__Vec_6string(t1788)
    result__635 = inline3288
    var t1789 int
    var inline3286 int = vec_len__Vec_6string(parts__633)
    t1789 = inline3286
    var t1790 FnIterator__isize
    var inline3280 int = 0
    var inline3281 *ref_int_x = ref__Ref_3int(inline3280)
    var inline3282 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3281,
        end_1: t1789,
    }
    var inline3283 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3282)
    }
    var inline3284 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3283)
    t1790 = inline3284
    var for_iter738 FnIterator__isize
    for_iter738 = t1790
    Loop_loop1793:
    for {
        var for_next739 Option__isize
        var inline3276 func() Option__isize = for_iter738.next_fn
        var inline3277 Option__isize = inline3276()
        for_next739 = inline3277
        switch for_next739._tag {
        case 0:
            break Loop_loop1793
        case 1:
            var x740 int = for_next739._v1_0
            var t1798 bool = x740 > 0
            if t1798 {
                vec_push__Vec_6string(result__635, separator__632)
            } else {}
            var t1796 string = vec_get__Vec_6string(parts__633, x740)
            vec_push__Vec_6string(result__635, t1796)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1792 string = __goml_builtin_string_concat(result__635)
    return t1792
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup____T__isize(self__640 *_goml_vec_int) struct{} {
    var t1806 closure_env_inherent_Vec_Vec_T_dedup_T_isize_10 = closure_env_inherent_Vec_Vec_T_dedup_T_isize_10{}
    var t1807 func(int, int) bool = func(p0 int, p1 int) bool {
        return _goml_m_inherent_i_closure__en_h5b5a2c4c0a397a14d2b6a2dc409e76d2_ize__10_i_apply(t1806, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__isize(self__640, t1807)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_binary__search__by____T__isize(self__598 *_goml_vec_int, compare__599 func(int) int) Option__isize {
    var low__600 int = 0
    var high__601 int
    var inline3307 int = vec_len__Vec_3int(self__598)
    high__601 = inline3307
    Loop_loop1822:
    for {
        var t1823 bool = low__600 < high__601
        if t1823 {
            var t1824 int = high__601 - low__600
            var t1825 int = t1824 / 2
            var middle__602 int = low__600 + t1825
            var t1827 int = vec_get__Vec_3int(self__598, middle__602)
            var t1828 int = compare__599(t1827)
            var t1829 bool = t1828 < 0
            if t1829 {
                var t1830 int = middle__602 + 1
                low__600 = t1830
                continue
            } else {
                high__601 = middle__602
                continue
            }
        } else {
            break Loop_loop1822
        }
    }
    var t1817 int
    var inline3305 int = vec_len__Vec_3int(self__598)
    t1817 = inline3305
    var t1818 bool = low__600 < t1817
    var jp1815 bool
    if t1818 {
        var t1819 int = vec_get__Vec_3int(self__598, low__600)
        var t1820 int = compare__599(t1819)
        var t1821 bool = t1820 == 0
        jp1815 = t1821
    } else {
        jp1815 = false
    }
    if jp1815 {
        var t1816 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: low__600,
        }
        return t1816
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_min__by____T__isize(self__606 *_goml_vec_int, compare__607 func(int, int) int) Option__isize {
    var t1835 bool
    var inline3321 int = vec_len__Vec_3int(self__606)
    var inline3322 bool = inline3321 == 0
    t1835 = inline3322
    if t1835 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var best__608 int = vec_get__Vec_3int(self__606, 0)
        var t1836 int
        var inline3319 int = vec_len__Vec_3int(self__606)
        t1836 = inline3319
        var t1837 FnIterator__isize
        var inline3313 int = 1
        var inline3314 *ref_int_x = ref__Ref_3int(inline3313)
        var inline3315 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3314,
            end_1: t1836,
        }
        var inline3316 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3315)
        }
        var inline3317 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3316)
        t1837 = inline3317
        var for_iter711 FnIterator__isize
        for_iter711 = t1837
        Loop_loop1840:
        for {
            var for_next712 Option__isize
            var inline3309 func() Option__isize = for_iter711.next_fn
            var inline3310 Option__isize = inline3309()
            for_next712 = inline3310
            switch for_next712._tag {
            case 0:
                break Loop_loop1840
            case 1:
                var x713 int = for_next712._v1_0
                var value__610 int = vec_get__Vec_3int(self__606, x713)
                var t1843 int = compare__607(value__610, best__608)
                var t1844 bool = t1843 < 0
                if t1844 {
                    best__608 = value__610
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1839 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: best__608,
        }
        return t1839
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_max__by____T__isize(self__611 *_goml_vec_int, compare__612 func(int, int) int) Option__isize {
    var t1849 bool
    var inline3336 int = vec_len__Vec_3int(self__611)
    var inline3337 bool = inline3336 == 0
    t1849 = inline3337
    if t1849 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var best__613 int = vec_get__Vec_3int(self__611, 0)
        var t1850 int
        var inline3334 int = vec_len__Vec_3int(self__611)
        t1850 = inline3334
        var t1851 FnIterator__isize
        var inline3328 int = 1
        var inline3329 *ref_int_x = ref__Ref_3int(inline3328)
        var inline3330 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3329,
            end_1: t1850,
        }
        var inline3331 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3330)
        }
        var inline3332 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3331)
        t1851 = inline3332
        var for_iter716 FnIterator__isize
        for_iter716 = t1851
        Loop_loop1854:
        for {
            var for_next717 Option__isize
            var inline3324 func() Option__isize = for_iter716.next_fn
            var inline3325 Option__isize = inline3324()
            for_next717 = inline3325
            switch for_next717._tag {
            case 0:
                break Loop_loop1854
            case 1:
                var x718 int = for_next717._v1_0
                var value__615 int = vec_get__Vec_3int(self__611, x718)
                var t1857 int = compare__612(value__615, best__613)
                var t1858 bool = t1857 > 0
                if t1858 {
                    best__613 = value__615
                    continue
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        var t1853 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: best__613,
        }
        return t1853
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_sort__by__ordering____T__string(self__590 *_goml_vec_string, compare__591 func(string, string) Ordering) struct{} {
    var t1863 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11{
        compare_0: compare__591,
    }
    var t1864 func(string, string) int = func(p0 string, p1 string) int {
        return _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(t1863, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__590, t1864)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__631 *_goml_vec_string, separator__632 string) string {
    var t1868 int
    var inline3381 int = vec_len__Vec_6string(self__631)
    t1868 = inline3381
    var parts__633 *_goml_vec_string
    var inline3379 *_goml_vec_string = vec_with_capacity__Vec_6string(t1868)
    parts__633 = inline3379
    var t1869 int
    var inline3377 int = vec_len__Vec_6string(self__631)
    t1869 = inline3377
    var t1870 FnIterator__isize
    var inline3371 int = 0
    var inline3372 *ref_int_x = ref__Ref_3int(inline3371)
    var inline3373 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3372,
        end_1: t1869,
    }
    var inline3374 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3373)
    }
    var inline3375 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3374)
    t1870 = inline3375
    var for_iter734 FnIterator__isize
    for_iter734 = t1870
    Loop_loop1885:
    for {
        var for_next735 Option__isize
        var inline3347 func() Option__isize = for_iter734.next_fn
        var inline3348 Option__isize = inline3347()
        for_next735 = inline3348
        switch for_next735._tag {
        case 0:
            break Loop_loop1885
        case 1:
            var x736 int = for_next735._v1_0
            var t1887 string = vec_get__Vec_6string(self__631, x736)
            var t1888 string
            t1888 = t1887
            vec_push__Vec_6string(parts__633, t1888)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1872 int
    var inline3368 int = vec_len__Vec_6string(parts__633)
    t1872 = inline3368
    var t1873 int = t1872 * 2
    var result__635 *_goml_vec_string
    var inline3366 *_goml_vec_string = vec_with_capacity__Vec_6string(t1873)
    result__635 = inline3366
    var t1874 int
    var inline3364 int = vec_len__Vec_6string(parts__633)
    t1874 = inline3364
    var t1875 FnIterator__isize
    var inline3358 int = 0
    var inline3359 *ref_int_x = ref__Ref_3int(inline3358)
    var inline3360 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
        current_0: inline3359,
        end_1: t1874,
    }
    var inline3361 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3360)
    }
    var inline3362 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3361)
    t1875 = inline3362
    var for_iter738 FnIterator__isize
    for_iter738 = t1875
    Loop_loop1878:
    for {
        var for_next739 Option__isize
        var inline3354 func() Option__isize = for_iter738.next_fn
        var inline3355 Option__isize = inline3354()
        for_next739 = inline3355
        switch for_next739._tag {
        case 0:
            break Loop_loop1878
        case 1:
            var x740 int = for_next739._v1_0
            var t1883 bool = x740 > 0
            if t1883 {
                vec_push__Vec_6string(result__635, separator__632)
            } else {}
            var t1881 string = vec_get__Vec_6string(parts__633, x740)
            vec_push__Vec_6string(result__635, t1881)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t1877 string = __goml_builtin_string_concat(result__635)
    return t1877
}

func _goml_m_inherent_i_Vec_i_Vec_l_hfde7b787e35869a5469e590f5ee0e2ea_size_c_isize_q_(self__590 *_goml_vec_Tuple2_3int_3int, compare__591 func(Tuple2_3int_3int, Tuple2_3int_3int) Ordering) struct{} {
    var t1894 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12 = closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_isize_isize_12{
        compare_0: compare__591,
    }
    var t1895 func(Tuple2_3int_3int, Tuple2_3int_3int) int = func(p0 Tuple2_3int_3int, p1 Tuple2_3int_3int) int {
        return _goml_m_inherent_i_closure__en_hc4b58ff73aa2ff3a45662a7349486da0_ize__12_i_apply(t1894, p0, p1)
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_isize_c_isize_q_(self__590, t1895)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_contains____T__isize(self__643 []int, expected__644 int) bool {
    var index__645 int = 0
    Loop_loop1906:
    for {
        var t1907 int
        var inline3390 int = len(self__643)
        t1907 = inline3390
        var t1908 bool = index__645 < t1907
        if t1908 {
            var t1912 int = self__643[index__645]
            var t1913 bool
            var inline3388 bool = t1912 == expected__644
            t1913 = inline3388
            if t1913 {
                return true
            } else {
                var compound_old749 int = index__645
                var compound_value750 int = 1
                var t1910 int = compound_old749 + compound_value750
                index__645 = t1910
                continue
            }
        } else {
            break Loop_loop1906
        }
    }
    return false
}

func _goml_m_inherent_i_FrozenVec_i_FrozenVec_l_T_r__i_contains____T__isize(self__646 FrozenVec__isize, expected__647 int) bool {
    var index__648 int = 0
    Loop_loop1924:
    for {
        var t1925 int
        var inline3397 *_goml_vec_int = self__646.values
        var inline3398 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(inline3397)
        t1925 = inline3398
        var t1926 bool = index__648 < t1925
        if t1926 {
            var t1930 int
            var inline3394 *_goml_vec_int = self__646.values
            var inline3395 int = vec_get__Vec_3int(inline3394, index__648)
            t1930 = inline3395
            var t1931 bool
            var inline3392 bool = t1930 == expected__647
            t1931 = inline3392
            if t1931 {
                return true
            } else {
                var compound_old754 int = index__648
                var compound_value755 int = 1
                var t1928 int = compound_old754 + compound_value755
                index__648 = t1928
                continue
            }
        } else {
            break Loop_loop1924
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__isize(self__526 *_goml_vec_int) int {
    var t1948 int = vec_len__Vec_3int(self__526)
    return t1948
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1957 int64 = int64(int(value__222))
    var inline3403 bool = t1957 < 0
    if inline3403 {
        var inline3404 uint64 = uint64(int64(t1957))
        var inline3405 uint64 = 0 - inline3404
        var inline3406 string = decimal_string(inline3405)
        var inline3407 string = "-" + inline3406
        return inline3407
    } else {
        var inline3408 uint64 = uint64(int64(t1957))
        var inline3409 string = decimal_string(inline3408)
        return inline3409
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__isize(self__575 *_goml_vec_int, compare__576 func(int, int) int) struct{} {
    var length__577 int
    var inline3439 int = vec_len__Vec_3int(self__575)
    length__577 = inline3439
    var t2020 bool = length__577 < 2
    if t2020 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_int
        var inline3437 *_goml_vec_int = vec_with_capacity__Vec_3int(length__577)
        buffer__578 = inline3437
        var t1961 FnIterator__isize
        var inline3431 int = 0
        var inline3432 *ref_int_x = ref__Ref_3int(inline3431)
        var inline3433 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3432,
            end_1: length__577,
        }
        var inline3434 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3433)
        }
        var inline3435 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3434)
        t1961 = inline3435
        var for_iter647 FnIterator__isize
        for_iter647 = t1961
        Loop_loop2016:
        for {
            var for_next648 Option__isize
            var inline3413 func() Option__isize = for_iter647.next_fn
            var inline3414 Option__isize = inline3413()
            for_next648 = inline3414
            switch for_next648._tag {
            case 0:
                break Loop_loop2016
            case 1:
                var x649 int = for_next648._v1_0
                var t2018 int = vec_get__Vec_3int(self__575, x649)
                vec_push__Vec_3int(buffer__578, t2018)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop1964:
        for {
            var t1965 bool = width__580 < length__577
            if t1965 {
                var left__581 int = 0
                Loop_loop1977:
                for {
                    var t1978 bool = left__581 < length__577
                    if t1978 {
                        var t1979 int = left__581 + width__580
                        var middle__582 int
                        var inline3418 bool = t1979 < length__577
                        if inline3418 {
                            middle__582 = t1979
                        } else {
                            middle__582 = length__577
                        }
                        var t1980 int = middle__582 + width__580
                        var right__583 int
                        var inline3416 bool = t1980 < length__577
                        if inline3416 {
                            right__583 = t1980
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop1998:
                        for {
                            var t2014 bool = first__584 < middle__582
                            var jp2000 bool
                            if t2014 {
                                var t2015 bool = second__585 < right__583
                                jp2000 = t2015
                            } else {
                                jp2000 = false
                            }
                            if jp2000 {
                                var t2004 int = vec_get__Vec_3int(self__575, first__584)
                                var t2005 int = vec_get__Vec_3int(self__575, second__585)
                                var t2006 int = compare__576(t2004, t2005)
                                var t2007 bool = t2006 <= 0
                                if t2007 {
                                    var index652 int = output__586
                                    vec_get__Vec_3int(buffer__578, index652)
                                    var value654 int = vec_get__Vec_3int(self__575, first__584)
                                    vec_set__Vec_3int(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2009 int = compound_old656 + compound_value657
                                    first__584 = t2009
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_3int(buffer__578, index660)
                                    var value662 int = vec_get__Vec_3int(self__575, second__585)
                                    vec_set__Vec_3int(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2012 int = compound_old664 + compound_value665
                                    second__585 = t2012
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2002 int = compound_old668 + compound_value669
                                output__586 = t2002
                                continue
                            } else {
                                break Loop_loop1998
                            }
                        }
                        Loop_loop1991:
                        for {
                            var t1992 bool = first__584 < middle__582
                            if t1992 {
                                var index673 int = output__586
                                vec_get__Vec_3int(buffer__578, index673)
                                var value675 int = vec_get__Vec_3int(self__575, first__584)
                                vec_set__Vec_3int(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t1994 int = compound_old677 + compound_value678
                                first__584 = t1994
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t1996 int = compound_old680 + compound_value681
                                output__586 = t1996
                                continue
                            } else {
                                break Loop_loop1991
                            }
                        }
                        Loop_loop1984:
                        for {
                            var t1985 bool = second__585 < right__583
                            if t1985 {
                                var index685 int = output__586
                                vec_get__Vec_3int(buffer__578, index685)
                                var value687 int = vec_get__Vec_3int(self__575, second__585)
                                vec_set__Vec_3int(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t1987 int = compound_old689 + compound_value690
                                second__585 = t1987
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t1989 int = compound_old692 + compound_value693
                                output__586 = t1989
                                continue
                            } else {
                                break Loop_loop1984
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop1977
                    }
                }
                var t1967 FnIterator__isize
                var inline3424 int = 0
                var inline3425 *ref_int_x = ref__Ref_3int(inline3424)
                var inline3426 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3425,
                    end_1: length__577,
                }
                var inline3427 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3426)
                }
                var inline3428 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3427)
                t1967 = inline3428
                var for_iter698 FnIterator__isize
                for_iter698 = t1967
                Loop_loop1974:
                for {
                    var for_next699 Option__isize
                    var inline3420 func() Option__isize = for_iter698.next_fn
                    var inline3421 Option__isize = inline3420()
                    for_next699 = inline3421
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop1974
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
                var t1971 int = length__577 / 2
                var t1972 bool = width__580 > t1971
                var jp1970 int
                if t1972 {
                    jp1970 = length__577
                } else {
                    var t1973 int = width__580 * 2
                    jp1970 = t1973
                }
                width__580 = jp1970
                continue
            } else {
                break Loop_loop1964
            }
        }
        return struct{}{}
    }
}

func __goml_builtin_string_concat(values__215 *_goml_vec_string) string {
    var length__216 int = 0
    var value_index__217 int = 0
    Loop_loop2048:
    for {
        var t2049 int
        var inline3446 int = vec_len__Vec_6string(values__215)
        t2049 = inline3446
        var t2050 bool = value_index__217 < t2049
        if t2050 {
            var compound_old365 int = length__216
            var t2051 string = vec_get__Vec_6string(values__215, value_index__217)
            var compound_value366 int
            var inline3444 int = _goml_runtime_core_string_len(t2051)
            compound_value366 = inline3444
            var t2052 int = compound_old365 + compound_value366
            length__216 = t2052
            var compound_old368 int = value_index__217
            var compound_value369 int = 1
            var t2054 int = compound_old368 + compound_value369
            value_index__217 = t2054
            continue
        } else {
            break Loop_loop2048
        }
    }
    var bytes__218 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(length__216)
    value_index__217 = 0
    Loop_loop2036:
    for {
        var t2037 int
        var inline3454 int = vec_len__Vec_6string(values__215)
        t2037 = inline3454
        var t2038 bool = value_index__217 < t2037
        if t2038 {
            var value__219 string = vec_get__Vec_6string(values__215, value_index__217)
            var byte_index__220 int = 0
            Loop_loop2042:
            for {
                var t2043 int
                var inline3452 int = _goml_runtime_core_string_len(value__219)
                t2043 = inline3452
                var t2044 bool = byte_index__220 < t2043
                if t2044 {
                    var t2045 uint8
                    var inline3450 uint8 = _goml_runtime_core_string_byte_get(value__219, byte_index__220)
                    t2045 = inline3450
                    vec_push__Vec_5uint8(bytes__218, t2045)
                    var compound_old374 int = byte_index__220
                    var compound_value375 int = 1
                    var t2046 int = compound_old374 + compound_value375
                    byte_index__220 = t2046
                    continue
                } else {
                    break Loop_loop2042
                }
            }
            var compound_old378 int = value_index__217
            var compound_value379 int = 1
            var t2040 int = compound_old378 + compound_value379
            value_index__217 = t2040
            continue
        } else {
            break Loop_loop2036
        }
    }
    var mtmp382 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__218)
    var x384 string = mtmp382._1
    return x384
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_dedup__by____T__isize(self__624 *_goml_vec_int, equal__625 func(int, int) bool) struct{} {
    var t2075 int
    var inline3470 int = vec_len__Vec_3int(self__624)
    t2075 = inline3470
    var t2076 bool = t2075 < 2
    if t2076 {
        return struct{}{}
    } else {
        var output__626 int = 1
        var t2061 int
        var inline3468 int = vec_len__Vec_3int(self__624)
        t2061 = inline3468
        var t2062 FnIterator__isize
        var inline3462 int = 1
        var inline3463 *ref_int_x = ref__Ref_3int(inline3462)
        var inline3464 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3463,
            end_1: t2061,
        }
        var inline3465 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3464)
        }
        var inline3466 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3465)
        t2062 = inline3466
        var for_iter722 FnIterator__isize
        for_iter722 = t2062
        Loop_loop2065:
        for {
            var for_next723 Option__isize
            var inline3456 func() Option__isize = for_iter722.next_fn
            var inline3457 Option__isize = inline3456()
            for_next723 = inline3457
            switch for_next723._tag {
            case 0:
                break Loop_loop2065
            case 1:
                var x724 int = for_next723._v1_0
                var value__628 int = vec_get__Vec_3int(self__624, x724)
                var t2068 int = output__626 - 1
                var t2069 int = vec_get__Vec_3int(self__624, t2068)
                var t2070 bool = equal__625(t2069, value__628)
                var t2071 bool = !t2070
                if t2071 {
                    var index726 int = output__626
                    vec_get__Vec_3int(self__624, index726)
                    vec_set__Vec_3int(self__624, index726, value__628)
                    var compound_old730 int = output__626
                    var compound_value731 int = 1
                    var t2073 int = compound_old730 + compound_value731
                    output__626 = t2073
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__isize(self__517 *_goml_vec_int) *_goml_vec_int {
    var t2094 int
    var inline3478 int = vec_len__Vec_3int(self__517)
    t2094 = inline3478
    var result__518 *_goml_vec_int
    var inline3476 *_goml_vec_int = vec_with_capacity__Vec_3int(t2094)
    result__518 = inline3476
    var index__519 int = 0
    Loop_loop2096:
    for {
        var t2097 int
        var inline3474 int = vec_len__Vec_3int(self__517)
        t2097 = inline3474
        var t2098 bool = index__519 < t2097
        if t2098 {
            var t2099 int = vec_get__Vec_3int(self__517, index__519)
            vec_push__Vec_3int(result__518, t2099)
            var compound_old581 int = index__519
            var compound_value582 int = 1
            var t2100 int = compound_old581 + compound_value582
            index__519 = t2100
            continue
        } else {
            break Loop_loop2096
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
    var t2116 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__507,
    }
    return t2116
}

func signed_decimal_string(value__214 int64) string {
    var t2121 bool = value__214 < 0
    if t2121 {
        var t2122 uint64 = uint64(int64(value__214))
        var t2123 uint64 = 0 - t2122
        var t2124 string = decimal_string(t2123)
        var t2125 string = "-" + t2124
        return t2125
    } else {
        var t2126 uint64 = uint64(int64(value__214))
        var t2127 string = decimal_string(t2126)
        return t2127
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T__string(self__575 *_goml_vec_string, compare__576 func(string, string) int) struct{} {
    var length__577 int
    var inline3510 int = vec_len__Vec_6string(self__575)
    length__577 = inline3510
    var t2209 bool = length__577 < 2
    if t2209 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_string
        var inline3508 *_goml_vec_string = vec_with_capacity__Vec_6string(length__577)
        buffer__578 = inline3508
        var t2150 FnIterator__isize
        var inline3502 int = 0
        var inline3503 *ref_int_x = ref__Ref_3int(inline3502)
        var inline3504 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3503,
            end_1: length__577,
        }
        var inline3505 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3504)
        }
        var inline3506 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3505)
        t2150 = inline3506
        var for_iter647 FnIterator__isize
        for_iter647 = t2150
        Loop_loop2205:
        for {
            var for_next648 Option__isize
            var inline3484 func() Option__isize = for_iter647.next_fn
            var inline3485 Option__isize = inline3484()
            for_next648 = inline3485
            switch for_next648._tag {
            case 0:
                break Loop_loop2205
            case 1:
                var x649 int = for_next648._v1_0
                var t2207 string = vec_get__Vec_6string(self__575, x649)
                vec_push__Vec_6string(buffer__578, t2207)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop2153:
        for {
            var t2154 bool = width__580 < length__577
            if t2154 {
                var left__581 int = 0
                Loop_loop2166:
                for {
                    var t2167 bool = left__581 < length__577
                    if t2167 {
                        var t2168 int = left__581 + width__580
                        var middle__582 int
                        var inline3489 bool = t2168 < length__577
                        if inline3489 {
                            middle__582 = t2168
                        } else {
                            middle__582 = length__577
                        }
                        var t2169 int = middle__582 + width__580
                        var right__583 int
                        var inline3487 bool = t2169 < length__577
                        if inline3487 {
                            right__583 = t2169
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop2187:
                        for {
                            var t2203 bool = first__584 < middle__582
                            var jp2189 bool
                            if t2203 {
                                var t2204 bool = second__585 < right__583
                                jp2189 = t2204
                            } else {
                                jp2189 = false
                            }
                            if jp2189 {
                                var t2193 string = vec_get__Vec_6string(self__575, first__584)
                                var t2194 string = vec_get__Vec_6string(self__575, second__585)
                                var t2195 int = compare__576(t2193, t2194)
                                var t2196 bool = t2195 <= 0
                                if t2196 {
                                    var index652 int = output__586
                                    vec_get__Vec_6string(buffer__578, index652)
                                    var value654 string = vec_get__Vec_6string(self__575, first__584)
                                    vec_set__Vec_6string(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2198 int = compound_old656 + compound_value657
                                    first__584 = t2198
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_6string(buffer__578, index660)
                                    var value662 string = vec_get__Vec_6string(self__575, second__585)
                                    vec_set__Vec_6string(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2201 int = compound_old664 + compound_value665
                                    second__585 = t2201
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2191 int = compound_old668 + compound_value669
                                output__586 = t2191
                                continue
                            } else {
                                break Loop_loop2187
                            }
                        }
                        Loop_loop2180:
                        for {
                            var t2181 bool = first__584 < middle__582
                            if t2181 {
                                var index673 int = output__586
                                vec_get__Vec_6string(buffer__578, index673)
                                var value675 string = vec_get__Vec_6string(self__575, first__584)
                                vec_set__Vec_6string(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t2183 int = compound_old677 + compound_value678
                                first__584 = t2183
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t2185 int = compound_old680 + compound_value681
                                output__586 = t2185
                                continue
                            } else {
                                break Loop_loop2180
                            }
                        }
                        Loop_loop2173:
                        for {
                            var t2174 bool = second__585 < right__583
                            if t2174 {
                                var index685 int = output__586
                                vec_get__Vec_6string(buffer__578, index685)
                                var value687 string = vec_get__Vec_6string(self__575, second__585)
                                vec_set__Vec_6string(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t2176 int = compound_old689 + compound_value690
                                second__585 = t2176
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t2178 int = compound_old692 + compound_value693
                                output__586 = t2178
                                continue
                            } else {
                                break Loop_loop2173
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop2166
                    }
                }
                var t2156 FnIterator__isize
                var inline3495 int = 0
                var inline3496 *ref_int_x = ref__Ref_3int(inline3495)
                var inline3497 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3496,
                    end_1: length__577,
                }
                var inline3498 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3497)
                }
                var inline3499 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3498)
                t2156 = inline3499
                var for_iter698 FnIterator__isize
                for_iter698 = t2156
                Loop_loop2163:
                for {
                    var for_next699 Option__isize
                    var inline3491 func() Option__isize = for_iter698.next_fn
                    var inline3492 Option__isize = inline3491()
                    for_next699 = inline3492
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop2163
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
                var t2160 int = length__577 / 2
                var t2161 bool = width__580 > t2160
                var jp2159 int
                if t2161 {
                    jp2159 = length__577
                } else {
                    var t2162 int = width__580 * 2
                    jp2159 = t2162
                }
                width__580 = jp2159
                continue
            } else {
                break Loop_loop2153
            }
        }
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_stable__sort__by____T___o_isize_c_isize_q_(self__575 *_goml_vec_Tuple2_3int_3int, compare__576 func(Tuple2_3int_3int, Tuple2_3int_3int) int) struct{} {
    var length__577 int
    var inline3540 int = vec_len__Vec_16Tuple2_3int_3int(self__575)
    length__577 = inline3540
    var t2271 bool = length__577 < 2
    if t2271 {
        return struct{}{}
    } else {
        var buffer__578 *_goml_vec_Tuple2_3int_3int
        var inline3538 *_goml_vec_Tuple2_3int_3int = vec_with_capacity__Vec_16Tuple2_3int_3int(length__577)
        buffer__578 = inline3538
        var t2212 FnIterator__isize
        var inline3532 int = 0
        var inline3533 *ref_int_x = ref__Ref_3int(inline3532)
        var inline3534 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
            current_0: inline3533,
            end_1: length__577,
        }
        var inline3535 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3534)
        }
        var inline3536 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3535)
        t2212 = inline3536
        var for_iter647 FnIterator__isize
        for_iter647 = t2212
        Loop_loop2267:
        for {
            var for_next648 Option__isize
            var inline3514 func() Option__isize = for_iter647.next_fn
            var inline3515 Option__isize = inline3514()
            for_next648 = inline3515
            switch for_next648._tag {
            case 0:
                break Loop_loop2267
            case 1:
                var x649 int = for_next648._v1_0
                var t2269 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, x649)
                vec_push__Vec_16Tuple2_3int_3int(buffer__578, t2269)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var width__580 int = 1
        Loop_loop2215:
        for {
            var t2216 bool = width__580 < length__577
            if t2216 {
                var left__581 int = 0
                Loop_loop2228:
                for {
                    var t2229 bool = left__581 < length__577
                    if t2229 {
                        var t2230 int = left__581 + width__580
                        var middle__582 int
                        var inline3519 bool = t2230 < length__577
                        if inline3519 {
                            middle__582 = t2230
                        } else {
                            middle__582 = length__577
                        }
                        var t2231 int = middle__582 + width__580
                        var right__583 int
                        var inline3517 bool = t2231 < length__577
                        if inline3517 {
                            right__583 = t2231
                        } else {
                            right__583 = length__577
                        }
                        var first__584 int = left__581
                        var second__585 int = middle__582
                        var output__586 int = left__581
                        Loop_loop2249:
                        for {
                            var t2265 bool = first__584 < middle__582
                            var jp2251 bool
                            if t2265 {
                                var t2266 bool = second__585 < right__583
                                jp2251 = t2266
                            } else {
                                jp2251 = false
                            }
                            if jp2251 {
                                var t2255 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                var t2256 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                var t2257 int = compare__576(t2255, t2256)
                                var t2258 bool = t2257 <= 0
                                if t2258 {
                                    var index652 int = output__586
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__578, index652)
                                    var value654 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__578, index652, value654)
                                    var compound_old656 int = first__584
                                    var compound_value657 int = 1
                                    var t2260 int = compound_old656 + compound_value657
                                    first__584 = t2260
                                } else {
                                    var index660 int = output__586
                                    vec_get__Vec_16Tuple2_3int_3int(buffer__578, index660)
                                    var value662 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                    vec_set__Vec_16Tuple2_3int_3int(buffer__578, index660, value662)
                                    var compound_old664 int = second__585
                                    var compound_value665 int = 1
                                    var t2263 int = compound_old664 + compound_value665
                                    second__585 = t2263
                                }
                                var compound_old668 int = output__586
                                var compound_value669 int = 1
                                var t2253 int = compound_old668 + compound_value669
                                output__586 = t2253
                                continue
                            } else {
                                break Loop_loop2249
                            }
                        }
                        Loop_loop2242:
                        for {
                            var t2243 bool = first__584 < middle__582
                            if t2243 {
                                var index673 int = output__586
                                vec_get__Vec_16Tuple2_3int_3int(buffer__578, index673)
                                var value675 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, first__584)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__578, index673, value675)
                                var compound_old677 int = first__584
                                var compound_value678 int = 1
                                var t2245 int = compound_old677 + compound_value678
                                first__584 = t2245
                                var compound_old680 int = output__586
                                var compound_value681 int = 1
                                var t2247 int = compound_old680 + compound_value681
                                output__586 = t2247
                                continue
                            } else {
                                break Loop_loop2242
                            }
                        }
                        Loop_loop2235:
                        for {
                            var t2236 bool = second__585 < right__583
                            if t2236 {
                                var index685 int = output__586
                                vec_get__Vec_16Tuple2_3int_3int(buffer__578, index685)
                                var value687 Tuple2_3int_3int = vec_get__Vec_16Tuple2_3int_3int(self__575, second__585)
                                vec_set__Vec_16Tuple2_3int_3int(buffer__578, index685, value687)
                                var compound_old689 int = second__585
                                var compound_value690 int = 1
                                var t2238 int = compound_old689 + compound_value690
                                second__585 = t2238
                                var compound_old692 int = output__586
                                var compound_value693 int = 1
                                var t2240 int = compound_old692 + compound_value693
                                output__586 = t2240
                                continue
                            } else {
                                break Loop_loop2235
                            }
                        }
                        left__581 = right__583
                        continue
                    } else {
                        break Loop_loop2228
                    }
                }
                var t2218 FnIterator__isize
                var inline3525 int = 0
                var inline3526 *ref_int_x = ref__Ref_3int(inline3525)
                var inline3527 closure_env_goml_builtin_range_13 = closure_env_goml_builtin_range_13{
                    current_0: inline3526,
                    end_1: length__577,
                }
                var inline3528 func() Option__isize = func() Option__isize {
                    return _goml_m_inherent_i_closure__en_h705ec68e290747cc19a5685005dd16c1_nge__13_i_apply(inline3527)
                }
                var inline3529 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline3528)
                t2218 = inline3529
                var for_iter698 FnIterator__isize
                for_iter698 = t2218
                Loop_loop2225:
                for {
                    var for_next699 Option__isize
                    var inline3521 func() Option__isize = for_iter698.next_fn
                    var inline3522 Option__isize = inline3521()
                    for_next699 = inline3522
                    switch for_next699._tag {
                    case 0:
                        break Loop_loop2225
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
                var t2222 int = length__577 / 2
                var t2223 bool = width__580 > t2222
                var jp2221 int
                if t2223 {
                    jp2221 = length__577
                } else {
                    var t2224 int = width__580 * 2
                    jp2221 = t2224
                }
                width__580 = jp2221
                continue
            } else {
                break Loop_loop2215
            }
        }
        return struct{}{}
    }
}

func decimal_string(value__208 uint64) string {
    var t2294 bool = value__208 == 0
    if t2294 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop2287:
        for {
            var t2288 bool = remaining__210 > 0
            if t2288 {
                var t2289_rhs uint64 = 10
                var t2289 uint64 = remaining__210 % t2289_rhs
                var t2290 uint8 = uint8(uint64(t2289))
                var t2291 uint8 = t2290 + 48
                vec_push__Vec_5uint8(reversed__209, t2291)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t2292 uint64 = compound_old353 / compound_value354
                remaining__210 = t2292
                continue
            } else {
                break Loop_loop2287
            }
        }
        var t2276 int
        var inline3550 int = vec_len__Vec_5uint8(reversed__209)
        t2276 = inline3550
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t2276)
        var offset__212 int = 0
        Loop_loop2278:
        for {
            var t2279 int
            var inline3548 int = vec_len__Vec_5uint8(reversed__209)
            t2279 = inline3548
            var t2280 bool = offset__212 < t2279
            if t2280 {
                var t2281 int
                var inline3546 int = vec_len__Vec_5uint8(reversed__209)
                t2281 = inline3546
                var t2282 int = t2281 - offset__212
                var t2283 int = t2282 - 1
                var t2284 uint8 = vec_get__Vec_5uint8(reversed__209, t2283)
                vec_push__Vec_5uint8(bytes__211, t2284)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t2285 int = compound_old358 + compound_value359
                offset__212 = t2285
                continue
            } else {
                break Loop_loop2278
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env815 closure_env_main_0, value__1 int) bool {
    var t2308 bool = value__1 == 5
    return t2308
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env816 closure_env_main_1, value__2 int) bool {
    var t2311 bool = value__2 == 99
    return t2311
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env817 closure_env_main_2, left__3 int, right__4 int) int {
    var t2314 int = left__3 - right__4
    return t2314
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env818 closure_env_main_3, value__6 int) int {
    var t2317 int = value__6 - 4
    return t2317
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env819 closure_env_main_4, value__7 int) int {
    var t2320 int = value__7 - 3
    return t2320
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env820 closure_env_main_5, left__8 int, right__9 int) int {
    var t2323 int = left__8 - right__9
    return t2323
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env821 closure_env_main_6, left__10 int, right__11 int) int {
    var t2326 int = left__10 - right__11
    return t2326
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env822 closure_env_main_7, left__13 string, right__14 string) Ordering {
    var inline3552 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_string_i_cmp(left__13, right__14)
    return inline3552
}

func _goml_m_inherent_i_closure__env__main__8_i_closure__env__main__8_i_apply(env823 closure_env_main_8, left__16 Tuple2_3int_3int, right__17 Tuple2_3int_3int) Ordering {
    var t2332 int = left__16._0
    var t2333 int = right__17._0
    var inline3554 Ordering = _goml_m_trait__impl_i_std_p_cmp_p_Ord_i_isize_i_cmp(t2332, t2333)
    return inline3554
}

func _goml_m_inherent_i_closure__env__main__9_i_closure__env__main__9_i_apply(env824 closure_env_main_9, left__21 int, right__22 int) int {
    var t2337 int = left__21 - right__22
    return t2337
}

func _goml_m_inherent_i_closure__en_h5b5a2c4c0a397a14d2b6a2dc409e76d2_ize__10_i_apply(env825 closure_env_inherent_Vec_Vec_T_dedup_T_isize_10, left__641 int, right__642 int) bool {
    var inline3556 bool = left__641 == right__642
    return inline3556
}

func _goml_m_inherent_i_closure__en_h8a0fbf92fe59c5b247b9c312b83aa9e5_ing__11_i_apply(env826 closure_env_inherent_Vec_Vec_T_sort_by_ordering_T_string_11, left__592 string, right__593 string) int {
    var compare__591 func(string, string) Ordering = env826.compare_0
    var t2343 Ordering = compare__591(left__592, right__593)
    switch t2343 {
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
    var t2347 Ordering = compare__591(left__592, right__593)
    switch t2347 {
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
    var t2353 bool = value__759 < end__757
    if t2353 {
        var t2354 int = value__759 + 1
        ref_set__Ref_3int(current__758, t2354)
        var t2355 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t2355
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
