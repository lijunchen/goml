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

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
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

type closure_env_roundtrip_T_string_0 struct {
    channel_0 chan string
    value_1 string
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
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__isize(buffered__2)
    var mtmp800 Option__isize
    var inline951 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline952 int = inline951._0
    var inline953 bool = inline951._1
    if inline953 {
        var inline956 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline952,
        }
        mtmp800 = inline956
    } else {
        mtmp800 = Option__isize{
            _tag: 0,
        }
    }
    var jp819 int
    switch mtmp800._tag {
    case 0:
        jp819 = -1
    case 1:
        var x801 int = mtmp800._v1_0
        jp819 = x801
    default:
        panic("non-exhaustive match")
    }
    var inline948 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp819)
    _goml_runtime_core_string_println(inline948)
    var mtmp803 Option__isize
    var inline941 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline942 int = inline941._0
    var inline943 bool = inline941._1
    if inline943 {
        var inline946 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline942,
        }
        mtmp803 = inline946
    } else {
        mtmp803 = Option__isize{
            _tag: 0,
        }
    }
    var jp821 int
    switch mtmp803._tag {
    case 0:
        jp821 = -1
    case 1:
        var x804 int = mtmp803._v1_0
        jp821 = x804
    default:
        panic("non-exhaustive match")
    }
    var inline938 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp821)
    _goml_runtime_core_string_println(inline938)
    var mtmp806 Option__isize
    var inline931 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline932 int = inline931._0
    var inline933 bool = inline931._1
    if inline933 {
        var inline936 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: inline932,
        }
        mtmp806 = inline936
    } else {
        mtmp806 = Option__isize{
            _tag: 0,
        }
    }
    var jp823 string
    switch mtmp806._tag {
    case 0:
        jp823 = "closed"
    case 1:
        jp823 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline928 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp823)
    _goml_runtime_core_string_println(inline928)
    var unbuffered__5 chan string
    var inline926 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline926
    var mtmp809 Option__string
    var inline920 string = "ready"
    var inline921 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline920,
    }
    var inline922 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline921)
    }
    go inline922()
    var inline924 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp809 = inline924
    var jp825 string
    switch mtmp809._tag {
    case 0:
        jp825 = "closed"
    case 1:
        var x810 string = mtmp809._v1_0
        jp825 = x810
    default:
        panic("non-exhaustive match")
    }
    var inline917 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp825)
    _goml_runtime_core_string_println(inline917)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__688 int) chan string {
    var t828 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__688)
    return t828
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__688 int) chan int {
    var t831 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__688)
    return t831
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(self__689 chan int, value__690 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__689, value__690)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__isize(self__694 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__694)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline968 int64 = int64(int(self__404))
    var inline969 string = signed_decimal_string(inline968)
    return inline969
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
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

func signed_decimal_string(value__214 int64) string {
    var t874 bool = value__214 < 0
    if t874 {
        var t875 uint64 = uint64(int64(value__214))
        var t876 uint64 = 0 - t875
        var t877 string = decimal_string(t876)
        var t878 string = "-" + t877
        return t878
    } else {
        var t879 uint64 = uint64(int64(value__214))
        var t880 string = decimal_string(t879)
        return t880
    }
}

func decimal_string(value__208 uint64) string {
    var t903 bool = value__208 == 0
    if t903 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop896:
        for {
            var t897 bool = remaining__210 > 0
            if t897 {
                var t898_rhs uint64 = 10
                var t898 uint64 = remaining__210 % t898_rhs
                var t899 uint8 = uint8(uint64(t898))
                var t900 uint8 = t899 + 48
                vec_push__Vec_5uint8(reversed__209, t900)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t901 uint64 = compound_old353 / compound_value354
                remaining__210 = t901
                continue
            } else {
                break Loop_loop896
            }
        }
        var t885 int
        var inline987 int = vec_len__Vec_5uint8(reversed__209)
        t885 = inline987
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t885)
        var offset__212 int = 0
        Loop_loop887:
        for {
            var t888 int
            var inline985 int = vec_len__Vec_5uint8(reversed__209)
            t888 = inline985
            var t889 bool = offset__212 < t888
            if t889 {
                var t890 int
                var inline983 int = vec_len__Vec_5uint8(reversed__209)
                t890 = inline983
                var t891 int = t890 - offset__212
                var t892 int = t891 - 1
                var t893 uint8 = vec_get__Vec_5uint8(reversed__209, t892)
                vec_push__Vec_5uint8(bytes__211, t893)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t894 int = compound_old358 + compound_value359
                offset__212 = t894
                continue
            } else {
                break Loop_loop887
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env813 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env813.channel_0
    var value__1 string = env813.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
