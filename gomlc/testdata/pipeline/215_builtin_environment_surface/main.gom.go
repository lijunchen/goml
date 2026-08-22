package main

import (
    _goml_os "os"
    _goml_reflect "reflect"
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

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_5int32(a *ref_int32_x, b *ref_int32_x) bool {
    return a == b
}

func ptr_hash__Ref_5int32(reference *ref_int32_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
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

type Tuple2_5int32_4bool struct {
    _0 int32
    _1 bool
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

type closure_env_goml_builtin_range_1 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var native__0 int = 7
    var small__1 int8 = 8
    var unsigned__2 uint8 = 9
    var t806 string
    var inline1014 string = __goml_builtin_int_to_string(native__0)
    t806 = inline1014
    var t807 string
    var inline1012 string = __goml_builtin_int8_to_string(small__1)
    t807 = inline1012
    var t808 string = t806 + t807
    var t809 string
    var inline1010 string = __goml_builtin_uint8_to_string(unsigned__2)
    t809 = inline1010
    var t810 string = t808 + t809
    var t811 string
    var inline1000 string = "abcd"
    var inline1001 int = 1
    var inline1002 int = 3
    var inline1003 bool = string_is_char_boundary(inline1000, inline1001)
    var inline1005 bool
    if inline1003 {
        var inline1008 bool = string_is_char_boundary(inline1000, inline1002)
        inline1005 = inline1008
    } else {
        inline1005 = false
    }
    if inline1005 {
        var inline1006 string = _goml_runtime_core_string_byte_slice(inline1000, inline1001, inline1002)
        t811 = inline1006
        var text__3 string = t810 + t811
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t812 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t812)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t813 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t813]
        var t814 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t814]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t815 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t815)
        var received__10 Tuple2_5int32_4bool = func(p0 chan int32) Tuple2_5int32_4bool {
            var value int32
            var ok bool
            value, ok = <-p0
            return Tuple2_5int32_4bool{
                _0: value,
                _1: ok,
            }
        }(channel__9)
        func(p0 chan int32) struct{} {
            close(p0)
            return struct{}{}
        }(channel__9)
        var t817 FnIterator__isize
        var inline996 int = 0
        var inline997 int = 3
        var inline998 FnIterator__isize = __goml_builtin_range(inline996, inline997)
        t817 = inline998
        var t818 closure_env_main_0 = closure_env_main_0{}
        var t819 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t818, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t817, 0, t819)
        var t820 string
        var inline994 string = _goml_runtime_core_bool_to_string(same__5)
        t820 = inline994
        var t821 string = text__3 + t820
        var t822 int32 = received__10._0
        var t823 string
        var inline992 string = __goml_builtin_int32_to_string(t822)
        t823 = inline992
        var t824 string = t821 + t823
        var t825 bool = received__10._1
        var t826 string
        var inline990 string = _goml_runtime_core_bool_to_string(t825)
        t826 = inline990
        var t827 string = t824 + t826
        var t828 string
        var inline988 string = __goml_builtin_int_to_string(total__13)
        t828 = inline988
        var t829 string = t827 + t828
        var inline985 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
        _goml_runtime_core_string_println(inline985)
        return struct{}{}
    } else {
        var inline1007 string = _goml_runtime_core_string_byte_slice(inline1000, -1, -1)
        t811 = inline1007
        var text__3 string = t810 + t811
        var value__4 *ref_int32_x = ref__Ref_5int32(1)
        ref_set__Ref_5int32(value__4, 2)
        var same__5 bool = ptr_eq__Ref_5int32(value__4, value__4)
        ptr_hash__Ref_5int32(value__4)
        var values__6 *_goml_vec_int32 = vec_new__Vec_5int32()
        var t812 int32 = ref_get__Ref_5int32(value__4)
        vec_push__Vec_5int32(values__6, t812)
        vec_push__Vec_5int32(values__6, 3)
        vec_set__Vec_5int32(values__6, 1, 4)
        var t813 int = vec_len__Vec_5int32(values__6)
        var values_slice__7 []int32 = values__6.items[0:t813]
        var t814 int = len(values_slice__7)
        var nested__8 []int32 = values_slice__7[0:t814]
        var channel__9 chan int32 = func(p0 int) chan int32 {
            return make(chan int32, p0)
        }(1)
        var t815 int32 = nested__8[1]
        func(p0 chan int32, p1 int32) struct{} {
            p0 <- p1
            return struct{}{}
        }(channel__9, t815)
        var received__10 Tuple2_5int32_4bool = func(p0 chan int32) Tuple2_5int32_4bool {
            var value int32
            var ok bool
            value, ok = <-p0
            return Tuple2_5int32_4bool{
                _0: value,
                _1: ok,
            }
        }(channel__9)
        func(p0 chan int32) struct{} {
            close(p0)
            return struct{}{}
        }(channel__9)
        var t817 FnIterator__isize
        var inline996 int = 0
        var inline997 int = 3
        var inline998 FnIterator__isize = __goml_builtin_range(inline996, inline997)
        t817 = inline998
        var t818 closure_env_main_0 = closure_env_main_0{}
        var t819 func(int, int) int = func(p0 int, p1 int) int {
            return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t818, p0, p1)
        }
        var total__13 int = _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(t817, 0, t819)
        var t820 string
        var inline994 string = _goml_runtime_core_bool_to_string(same__5)
        t820 = inline994
        var t821 string = text__3 + t820
        var t822 int32 = received__10._0
        var t823 string
        var inline992 string = __goml_builtin_int32_to_string(t822)
        t823 = inline992
        var t824 string = t821 + t823
        var t825 bool = received__10._1
        var t826 string
        var inline990 string = _goml_runtime_core_bool_to_string(t825)
        t826 = inline990
        var t827 string = t824 + t826
        var t828 string
        var inline988 string = __goml_builtin_int_to_string(total__13)
        t828 = inline988
        var t829 string = t827 + t828
        var inline985 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
        _goml_runtime_core_string_println(inline985)
        return struct{}{}
    }
}

func _goml_m_std_p_iter_p_fold____A__isize____I__FnIterator_l_isize_r_____T__isize(iterator__48 FnIterator__isize, initial__49 int, combine__50 func(int, int) int) int {
    var accumulator__51 int = initial__49
    Loop_loop_expr853:
    for {
        var mtmp43 Option__isize
        var inline1025 func() Option__isize = iterator__48.next_fn
        var inline1026 Option__isize = inline1025()
        mtmp43 = inline1026
        switch mtmp43._tag {
        case 0:
            break Loop_loop_expr853
        case 1:
            var x44 int = mtmp43._v1_0
            var t855 int = combine__50(accumulator__51, x44)
            accumulator__51 = t855
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return accumulator__51
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t873 int64 = int64(int(value__222))
    var inline1040 bool = t873 < 0
    if inline1040 {
        var inline1041 uint64 = uint64(int64(t873))
        var inline1042 uint64 = 0 - inline1041
        var inline1043 string = decimal_string(inline1042)
        var inline1044 string = "-" + inline1043
        return inline1044
    } else {
        var inline1045 uint64 = uint64(int64(t873))
        var inline1046 string = decimal_string(inline1045)
        return inline1046
    }
}

func __goml_builtin_int8_to_string(value__223 int8) string {
    var t877 int64 = int64(int8(value__223))
    var inline1048 bool = t877 < 0
    if inline1048 {
        var inline1049 uint64 = uint64(int64(t877))
        var inline1050 uint64 = 0 - inline1049
        var inline1051 string = decimal_string(inline1050)
        var inline1052 string = "-" + inline1051
        return inline1052
    } else {
        var inline1053 uint64 = uint64(int64(t877))
        var inline1054 string = decimal_string(inline1053)
        return inline1054
    }
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t881 uint64 = uint64(uint8(value__228))
    var t882 string = decimal_string(t881)
    return t882
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t896 bool = index__269 < 0
    var jp888 bool
    if t896 {
        jp888 = true
    } else {
        var t897 int
        var inline1056 int = _goml_runtime_core_string_len(value__268)
        t897 = inline1056
        var t898 bool = index__269 > t897
        jp888 = t898
    }
    if jp888 {
        return false
    } else {
        var t891 int
        var inline1060 int = _goml_runtime_core_string_len(value__268)
        t891 = inline1060
        var t892 bool = index__269 == t891
        if t892 {
            return true
        } else {
            var t893 uint8
            var inline1058 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t893 = inline1058
            var t894_rhs uint8 = 192
            var t894 uint8 = t893 & t894_rhs
            var t895 bool = t894 != 128
            return t895
        }
    }
}

func __goml_builtin_range(start__756 int, end__757 int) FnIterator__isize {
    var current__758 *ref_int_x = ref__Ref_3int(start__756)
    var t905 closure_env_goml_builtin_range_1 = closure_env_goml_builtin_range_1{
        current_0: current__758,
        end_1: end__757,
    }
    var t906 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(t905)
    }
    var inline1062 FnIterator__isize = FnIterator__isize{
        next_fn: t906,
    }
    return inline1062
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t912 int64 = int64(int32(value__225))
    var inline1064 bool = t912 < 0
    if inline1064 {
        var inline1065 uint64 = uint64(int64(t912))
        var inline1066 uint64 = 0 - inline1065
        var inline1067 string = decimal_string(inline1066)
        var inline1068 string = "-" + inline1067
        return inline1068
    } else {
        var inline1069 uint64 = uint64(int64(t912))
        var inline1070 string = decimal_string(inline1069)
        return inline1070
    }
}

func decimal_string(value__208 uint64) string {
    var t947 bool = value__208 == 0
    if t947 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop940:
        for {
            var t941 bool = remaining__210 > 0
            if t941 {
                var t942_rhs uint64 = 10
                var t942 uint64 = remaining__210 % t942_rhs
                var t943 uint8 = uint8(uint64(t942))
                var t944 uint8 = t943 + 48
                vec_push__Vec_5uint8(reversed__209, t944)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t945 uint64 = compound_old353 / compound_value354
                remaining__210 = t945
                continue
            } else {
                break Loop_loop940
            }
        }
        var t929 int
        var inline1080 int = vec_len__Vec_5uint8(reversed__209)
        t929 = inline1080
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t929)
        var offset__212 int = 0
        Loop_loop931:
        for {
            var t932 int
            var inline1078 int = vec_len__Vec_5uint8(reversed__209)
            t932 = inline1078
            var t933 bool = offset__212 < t932
            if t933 {
                var t934 int
                var inline1076 int = vec_len__Vec_5uint8(reversed__209)
                t934 = inline1076
                var t935 int = t934 - offset__212
                var t936 int = t935 - 1
                var t937 uint8 = vec_get__Vec_5uint8(reversed__209, t936)
                vec_push__Vec_5uint8(bytes__211, t937)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t938 int = compound_old358 + compound_value359
                offset__212 = t938
                continue
            } else {
                break Loop_loop931
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env803 closure_env_main_0, sum__11 int, item__12 int) int {
    var t976 int = sum__11 + item__12
    return t976
}

func _goml_m_inherent_i_closure__en_h07c29ff1f344b08e028033881af7c2d9_ange__1_i_apply(env804 closure_env_goml_builtin_range_1) Option__isize {
    var current__758 *ref_int_x = env804.current_0
    var end__757 int = env804.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t981 bool = value__759 < end__757
    if t981 {
        var t982 int = value__759 + 1
        ref_set__Ref_3int(current__758, t982)
        var t983 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t983
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
