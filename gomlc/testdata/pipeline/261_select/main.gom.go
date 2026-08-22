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

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_6string_4bool struct {
    _0 string
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

type closure_env_read_0 struct {
    value_0 Option__isize
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var received__6 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(received__6, 7)
    var jp818 int
    var _goml_m_value____7_i_select__value int
    var _goml_m_value____7_i_select__open bool
    var value__7 Option__isize = Option__isize{
        _tag: 0,
    }
    select {
    case _goml_m_value____7_i_select__value, _goml_m_value____7_i_select__open = <-received__6:
        if _goml_m_value____7_i_select__open {
            value__7 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____7_i_select__value,
            }
        }
        var t836 closure_env_read_0 = closure_env_read_0{
            value_0: value__7,
        }
        var read__8 func() int = func() int {
            return _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(t836)
        }
        var t837 int = read__8()
        jp818 = t837
    default:
        jp818 = 0
    }
    println__T_isize(jp818)
    var sent__10 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(1)
    var jp820 string
    select {
    case sent__10 <- "ready":
        jp820 = "sent"
    default:
        jp820 = "blocked"
    }
    var inline995 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp820)
    _goml_runtime_core_string_println(inline995)
    var t821 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(sent__10)
    var t822 string
    var inline991 string = "missing"
    switch t821._tag {
    case 0:
        t822 = inline991
    case 1:
        var inline992 string = t821._v1_0
        t822 = inline992
    default:
        panic("non-exhaustive match")
    }
    var inline988 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline988)
    var empty__12 chan int
    var inline985 int = 0
    var inline986 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline985)
    empty__12 = inline986
    var jp824 int
    var _goml_m_______13_i_select__open bool
    select {
    case _, _goml_m_______13_i_select__open = <-empty__12:
        if _goml_m_______13_i_select__open {}
        jp824 = -1
    default:
        jp824 = 42
    }
    var inline982 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp824)
    _goml_runtime_core_string_println(inline982)
    var log__15 *ref_string_x
    var inline979 string = ""
    var inline980 *ref_string_x = ref__Ref_6string(inline979)
    log__15 = inline980
    var left__16 chan int
    var inline976 int = 0
    var inline977 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline976)
    left__16 = inline977
    var right__17 chan int
    var inline973 int = 0
    var inline974 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline973)
    right__17 = inline974
    var t832 chan int
    var inline968 string = "a"
    var inline969 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline970 string = inline969 + inline968
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline970)
    t832 = left__16
    var t833 int
    var inline962 string = "1"
    var inline963 int = 1
    var inline964 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline965 string = inline964 + inline962
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline965)
    t833 = inline963
    var t834 chan int
    var inline957 string = "b"
    var inline958 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline959 string = inline958 + inline957
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline959)
    t834 = right__17
    var t835 int
    var inline951 string = "2"
    var inline952 int = 2
    var inline953 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline954 string = inline953 + inline951
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline954)
    t835 = inline952
    select {
    case t832 <- t833:
    case t834 <- t835:
    default:
    }
    var t826 string
    var inline949 string = ref_get__Ref_6string(log__15)
    t826 = inline949
    var inline946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline946)
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(received__6)
    var jp828 string
    var _goml_m_value____18_i_select__value int
    var _goml_m_value____18_i_select__open bool
    var value__18 Option__isize = Option__isize{
        _tag: 0,
    }
    select {
    case _goml_m_value____18_i_select__value, _goml_m_value____18_i_select__open = <-received__6:
        if _goml_m_value____18_i_select__open {
            value__18 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____18_i_select__value,
            }
        }
        switch value__18._tag {
        case 0:
            jp828 = "closed"
        case 1:
            jp828 = "open"
        default:
            panic("non-exhaustive match")
        }
    }
    var inline941 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp828)
    _goml_runtime_core_string_println(inline941)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__685 *ref_string_x) string {
    var t840 string = ref_get__Ref_6string(self__685)
    return t840
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__686 *ref_string_x, value__687 string) struct{} {
    ref_set__Ref_6string(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__688 int) chan int {
    var t845 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__688)
    return t845
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(self__689 chan int, value__690 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__689, value__690)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t853 string
    var inline998 string = __goml_builtin_int_to_string(value__1)
    t853 = inline998
    _goml_runtime_core_string_println(t853)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__688 int) chan string {
    var t857 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__688)
    return t857
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__691 chan string) Option__string {
    var mtmp764 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__691)
    var x765 string = mtmp764._0
    var x766 bool = mtmp764._1
    if x766 {
        var t865 Option__string = Option__string{
            _tag: 1,
            _v1_0: x765,
        }
        return t865
    } else {
        return Option__string{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1001 int64 = int64(int(self__404))
    var inline1002 string = signed_decimal_string(inline1001)
    return inline1002
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t882 int64 = int64(int(value__222))
    var inline1004 bool = t882 < 0
    if inline1004 {
        var inline1005 uint64 = uint64(int64(t882))
        var inline1006 uint64 = 0 - inline1005
        var inline1007 string = decimal_string(inline1006)
        var inline1008 string = "-" + inline1007
        return inline1008
    } else {
        var inline1009 uint64 = uint64(int64(t882))
        var inline1010 string = decimal_string(inline1009)
        return inline1010
    }
}

func signed_decimal_string(value__214 int64) string {
    var t888 bool = value__214 < 0
    if t888 {
        var t889 uint64 = uint64(int64(value__214))
        var t890 uint64 = 0 - t889
        var t891 string = decimal_string(t890)
        var t892 string = "-" + t891
        return t892
    } else {
        var t893 uint64 = uint64(int64(value__214))
        var t894 string = decimal_string(t893)
        return t894
    }
}

func decimal_string(value__208 uint64) string {
    var t917 bool = value__208 == 0
    if t917 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop910:
        for {
            var t911 bool = remaining__210 > 0
            if t911 {
                var t912_rhs uint64 = 10
                var t912 uint64 = remaining__210 % t912_rhs
                var t913 uint8 = uint8(uint64(t912))
                var t914 uint8 = t913 + 48
                vec_push__Vec_5uint8(reversed__209, t914)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t915 uint64 = compound_old353 / compound_value354
                remaining__210 = t915
                continue
            } else {
                break Loop_loop910
            }
        }
        var t899 int
        var inline1020 int = vec_len__Vec_5uint8(reversed__209)
        t899 = inline1020
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t899)
        var offset__212 int = 0
        Loop_loop901:
        for {
            var t902 int
            var inline1018 int = vec_len__Vec_5uint8(reversed__209)
            t902 = inline1018
            var t903 bool = offset__212 < t902
            if t903 {
                var t904 int
                var inline1016 int = vec_len__Vec_5uint8(reversed__209)
                t904 = inline1016
                var t905 int = t904 - offset__212
                var t906 int = t905 - 1
                var t907 uint8 = vec_get__Vec_5uint8(reversed__209, t906)
                vec_push__Vec_5uint8(bytes__211, t907)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t908 int = compound_old358 + compound_value359
                offset__212 = t908
                continue
            } else {
                break Loop_loop901
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(env807 closure_env_read_0) int {
    var value__7 Option__isize = env807.value_0
    var inline1022 int = -1
    switch value__7._tag {
    case 0:
        return inline1022
    case 1:
        var inline1023 int = value__7._v1_0
        return inline1023
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
