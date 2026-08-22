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

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var log__9 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var disabled__10 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__isize(disabled__10)
    var t835 chan int
    var inline982 string = "c"
    var inline983 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline984 string = inline983 + inline982
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline984)
    t835 = disabled__10
    var t836 int
    var inline976 string = "v"
    var inline977 int = 1
    var inline978 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline979 string = inline978 + inline976
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline979)
    t836 = inline977
    var t837 bool
    var inline970 string = "g"
    var inline971 bool = false
    var inline972 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline973 string = inline972 + inline970
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline973)
    t837 = inline971
    var jp821 string
    var select_channel_0_0 chan int
    if t837 {
        select_channel_0_0 = t835
    }
    select {
    case select_channel_0_0 <- t836:
        jp821 = "sent"
    default:
        jp821 = "default"
    }
    var t822 string
    var inline968 string = ref_get__Ref_6string(log__9)
    t822 = inline968
    var inline965 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t822)
    _goml_runtime_core_string_println(inline965)
    var inline962 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp821)
    _goml_runtime_core_string_println(inline962)
    var first__12 chan int
    var inline959 int = 1
    var inline960 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline959)
    first__12 = inline960
    var second__13 chan int
    var inline956 int = 1
    var inline957 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline956)
    second__13 = inline957
    var inline953 int = 10
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(first__12, inline953)
    var inline950 int = 20
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(second__13, inline950)
    var jp824 int
    var _goml_m_value____14_i_select__value int
    var _goml_m_value____14_i_select__open bool
    var value__14 Option__isize = Option__isize{
        _tag: 0,
    }
    var select_channel_1_0 chan int
    if true {
        select_channel_1_0 = first__12
    }
    var _goml_m_value____15_i_select__value int
    var _goml_m_value____15_i_select__open bool
    var value__15 Option__isize = Option__isize{
        _tag: 0,
    }
    var select_channel_1_1 chan int
    if true {
        select_channel_1_1 = second__13
    }
    select {
    case _goml_m_value____14_i_select__value, _goml_m_value____14_i_select__open = <-select_channel_1_0:
        if _goml_m_value____14_i_select__open {
            value__14 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____14_i_select__value,
            }
        }
        var inline930 int = -1
        switch value__14._tag {
        case 0:
            jp824 = inline930
        case 1:
            var inline931 int = value__14._v1_0
            jp824 = inline931
        default:
            panic("non-exhaustive match")
        }
    default:
        select {
        case _goml_m_value____15_i_select__value, _goml_m_value____15_i_select__open = <-select_channel_1_1:
            if _goml_m_value____15_i_select__open {
                value__15 = Option__isize{
                    _tag: 1,
                    _v1_0: _goml_m_value____15_i_select__value,
                }
            }
            var inline934 int = -1
            switch value__15._tag {
            case 0:
                jp824 = inline934
            case 1:
                var inline935 int = value__15._v1_0
                jp824 = inline935
            default:
                panic("non-exhaustive match")
            }
        default:
            jp824 = 0
        }
    }
    var inline947 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp824)
    _goml_runtime_core_string_println(inline947)
    var events__17 chan int
    var inline944 int = 1
    var inline945 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline944)
    events__17 = inline945
    var inline941 int = 7
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(events__17, inline941)
    var t828 int = 1 + 1
    var t829 bool = t828 == 2
    var jp826 int
    var _goml_m__d_select__recv__1186____18_i_select__value int
    var _goml_m__d_select__recv__1186____18_i_select__open bool
    var _goml_m__d_select__recv__1186____18 Option__isize = Option__isize{
        _tag: 0,
    }
    var select_channel_2_0 chan int
    if t829 {
        select_channel_2_0 = events__17
    }
    select {
    case _goml_m__d_select__recv__1186____18_i_select__value, _goml_m__d_select__recv__1186____18_i_select__open = <-select_channel_2_0:
        if _goml_m__d_select__recv__1186____18_i_select__open {
            _goml_m__d_select__recv__1186____18 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m__d_select__recv__1186____18_i_select__value,
            }
        }
        switch _goml_m__d_select__recv__1186____18._tag {
        case 0:
            jp826 = 0
        case 1:
            var x806 int = _goml_m__d_select__recv__1186____18._v1_0
            var t832 int = x806 + 1
            jp826 = t832
        default:
            panic("non-exhaustive match")
        }
    default:
        jp826 = -1
    }
    var inline938 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp826)
    _goml_runtime_core_string_println(inline938)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__684 string) *ref_string_x {
    var t845 *ref_string_x = ref__Ref_6string(value__684)
    return t845
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__688 int) chan int {
    var t848 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__688)
    return t848
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__isize(self__694 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__694)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline990 int64 = int64(int(self__404))
    var inline991 string = signed_decimal_string(inline990)
    return inline991
}

func signed_decimal_string(value__214 int64) string {
    var t876 bool = value__214 < 0
    if t876 {
        var t877 uint64 = uint64(int64(value__214))
        var t878 uint64 = 0 - t877
        var t879 string = decimal_string(t878)
        var t880 string = "-" + t879
        return t880
    } else {
        var t881 uint64 = uint64(int64(value__214))
        var t882 string = decimal_string(t881)
        return t882
    }
}

func decimal_string(value__208 uint64) string {
    var t905 bool = value__208 == 0
    if t905 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop898:
        for {
            var t899 bool = remaining__210 > 0
            if t899 {
                var t900_rhs uint64 = 10
                var t900 uint64 = remaining__210 % t900_rhs
                var t901 uint8 = uint8(uint64(t900))
                var t902 uint8 = t901 + 48
                vec_push__Vec_5uint8(reversed__209, t902)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t903 uint64 = compound_old353 / compound_value354
                remaining__210 = t903
                continue
            } else {
                break Loop_loop898
            }
        }
        var t887 int
        var inline1009 int = vec_len__Vec_5uint8(reversed__209)
        t887 = inline1009
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t887)
        var offset__212 int = 0
        Loop_loop889:
        for {
            var t890 int
            var inline1007 int = vec_len__Vec_5uint8(reversed__209)
            t890 = inline1007
            var t891 bool = offset__212 < t890
            if t891 {
                var t892 int
                var inline1005 int = vec_len__Vec_5uint8(reversed__209)
                t892 = inline1005
                var t893 int = t892 - offset__212
                var t894 int = t893 - 1
                var t895 uint8 = vec_get__Vec_5uint8(reversed__209, t894)
                vec_push__Vec_5uint8(bytes__211, t895)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t896 int = compound_old358 + compound_value359
                offset__212 = t896
                continue
            } else {
                break Loop_loop889
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
