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

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

func vec_truncate__Vec_6string(vec *_goml_vec_string, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
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

func vec_set__Vec_5uint8(vec *_goml_vec_uint8, index int, value uint8) struct{} {
    vec.items[index] = value
    return struct{}{}
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

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple2_13Option__isize_13Option__isize struct {
    _0 Option__isize
    _1 Option__isize
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_isize_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_isize_c_char_q_
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type closure_env_inherent_string_string_chars_1 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_inherent_string_string_char_indices_2 struct {
    index_0 *ref_int_x
    self_1 string
}

type Ordering int32

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

type _goml_m_Option_____o_isize_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

func main0() struct{} {
    var text__0 string = "  Héllo, World!  "
    var t817 string = _goml_m_inherent_i_string_i_string_i_trim(text__0)
    var t818 string = "[" + t817
    var t819 string = t818 + "]"
    println__T_string(t819)
    var t820 string = _goml_m_inherent_i_string_i_string_i_trim__start(text__0)
    var t821 string = "[" + t820
    var t822 string = t821 + "]"
    println__T_string(t822)
    var t823 string = _goml_m_inherent_i_string_i_string_i_trim__end(text__0)
    var t824 string = "[" + t823
    var t825 string = t824 + "]"
    println__T_string(t825)
    var t826 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(text__0, ",")
    var t827 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(t826, "|")
    println__T_string(t827)
    var t828 _goml_m_Option_____o_string_c_string_q_ = _goml_m_inherent_i_string_i_string_i_split__once(text__0, ",")
    var t829 Tuple2_6string_6string = Tuple2_6string_6string{
        _0: "",
        _1: "",
    }
    var t830 Tuple2_6string_6string = _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(t828, t829)
    var t831 string = t830._1
    println__T_string(t831)
    var t832 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_lines(text__0)
    var t833 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t832)
    var t834 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t833)
    println__T_string(t834)
    var t835 Option__isize = _goml_m_inherent_i_string_i_string_i_find(text__0, "World")
    var t836 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t835, -1)
    var t837 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t836)
    println__T_string(t837)
    var t838 Option__isize = _goml_m_inherent_i_string_i_string_i_rfind(text__0, "l")
    var t839 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t838, -1)
    var t840 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t839)
    println__T_string(t840)
    var t841 Option__isize
    var inline1640 string = "lo"
    var inline1641 Option__isize = _goml_m_inherent_i_string_i_string_i_find(text__0, inline1640)
    t841 = inline1641
    var t842 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t841, -1)
    var t843 string
    var inline1638 string = __goml_builtin_int_to_string(t842)
    t843 = inline1638
    var inline1635 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t843)
    _goml_runtime_core_string_println(inline1635)
    var t844 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(text__0, 2, "Hé")
    var t845 string
    var inline1633 string = _goml_runtime_core_bool_to_string(t844)
    t845 = inline1633
    var inline1630 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t845)
    _goml_runtime_core_string_println(inline1630)
    var t846 int = _goml_m_inherent_i_string_i_string_i_char__count(text__0)
    var t847 string
    var inline1628 string = __goml_builtin_int_to_string(t846)
    t847 = inline1628
    var inline1625 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t847)
    _goml_runtime_core_string_println(inline1625)
    var t848 Option__string = _goml_m_inherent_i_string_i_string_i_slice__chars(text__0, 2, 7)
    var t849 string
    var inline1621 string = "none"
    switch t848._tag {
    case 0:
        t849 = inline1621
    case 1:
        var inline1622 string = t848._v1_0
        t849 = inline1622
    default:
        panic("non-exhaustive match")
    }
    var inline1618 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t849)
    _goml_runtime_core_string_println(inline1618)
    var t850 string = _goml_m_inherent_i_string_i_string_i_replace(text__0, "l", "L")
    var inline1615 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t850)
    _goml_runtime_core_string_println(inline1615)
    var t851 string = _goml_m_inherent_i_string_i_string_i_repeat("ab", 3)
    var inline1612 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t851)
    _goml_runtime_core_string_println(inline1612)
    var t852 bool = _goml_m_inherent_i_string_i_string_i_is__ascii(text__0)
    var t853 string
    var inline1610 string = _goml_runtime_core_bool_to_string(t852)
    t853 = inline1610
    var inline1607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t853)
    _goml_runtime_core_string_println(inline1607)
    var t854 bool = _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case("ABC", "abc")
    var t855 string
    var inline1605 string = _goml_runtime_core_bool_to_string(t854)
    t855 = inline1605
    var inline1602 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t855)
    _goml_runtime_core_string_println(inline1602)
    var t856 string = _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase("AbC")
    var inline1599 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t856)
    _goml_runtime_core_string_println(inline1599)
    var t857 string = _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase("aBc")
    var inline1596 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t857)
    _goml_runtime_core_string_println(inline1596)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t860 string
    t860 = value__1
    _goml_runtime_core_string_println(t860)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_trim(self__347 string) string {
    var t864 string = _goml_m_inherent_i_string_i_string_i_trim__start(self__347)
    var t865 string = _goml_m_inherent_i_string_i_string_i_trim__end(t864)
    return t865
}

func _goml_m_inherent_i_string_i_string_i_trim__start(self__343 string) string {
    var start__344 int = 0
    Loop_loop871:
    for {
        var t876 int
        var inline1646 int = _goml_runtime_core_string_len(self__343)
        t876 = inline1646
        var t877 bool = start__344 < t876
        var jp873 bool
        if t877 {
            var t878 uint8
            var inline1644 uint8 = _goml_runtime_core_string_byte_get(self__343, start__344)
            t878 = inline1644
            var t879 bool = ascii_is_whitespace(t878)
            jp873 = t879
        } else {
            jp873 = false
        }
        if jp873 {
            var compound_old462 int = start__344
            var compound_value463 int = 1
            var t874 int = compound_old462 + compound_value463
            start__344 = t874
            continue
        } else {
            break Loop_loop871
        }
    }
    var t869 int
    var inline1650 int = _goml_runtime_core_string_len(self__343)
    t869 = inline1650
    var inline1648 string = string_byte_slice(self__343, start__344, t869)
    return inline1648
}

func _goml_m_inherent_i_string_i_string_i_trim__end(self__345 string) string {
    var end__346 int
    var inline1657 int = _goml_runtime_core_string_len(self__345)
    end__346 = inline1657
    Loop_loop884:
    for {
        var t889 bool = end__346 > 0
        var jp886 bool
        if t889 {
            var t890 int = end__346 - 1
            var t891 uint8
            var inline1652 uint8 = _goml_runtime_core_string_byte_get(self__345, t890)
            t891 = inline1652
            var t892 bool = ascii_is_whitespace(t891)
            jp886 = t892
        } else {
            jp886 = false
        }
        if jp886 {
            var compound_old466 int = end__346
            var compound_value467 int = 1
            var t887 int = compound_old466 - compound_value467
            end__346 = t887
            continue
        } else {
            break Loop_loop884
        }
    }
    var inline1654 int = 0
    var inline1655 string = string_byte_slice(self__345, inline1654, end__346)
    return inline1655
}

func _goml_m_inherent_i_string_i_string_i_split(self__348 string, separator__349 string) *_goml_vec_string {
    var result__350 *_goml_vec_string
    var inline1673 *_goml_vec_string = vec_new__Vec_6string()
    result__350 = inline1673
    var separator_len__351 int
    var inline1671 int = _goml_runtime_core_string_len(separator__349)
    separator_len__351 = inline1671
    var value_len__352 int
    var inline1669 int = _goml_runtime_core_string_len(self__348)
    value_len__352 = inline1669
    var t902 bool = separator_len__351 == 0
    if t902 {
        vec_push__Vec_6string(result__350, self__348)
        return result__350
    } else {
        var start__353 int = 0
        Loop_loop_expr897:
        for {
            var mtmp472 Option__isize = string_find_from(self__348, separator__349, start__353)
            switch mtmp472._tag {
            case 0:
                var t899 string
                var inline1663 string = string_byte_slice(self__348, start__353, value_len__352)
                t899 = inline1663
                vec_push__Vec_6string(result__350, t899)
                break Loop_loop_expr897
            case 1:
                var x473 int = mtmp472._v1_0
                var t900 string
                var inline1667 string = string_byte_slice(self__348, start__353, x473)
                t900 = inline1667
                vec_push__Vec_6string(result__350, t900)
                var t901 int = x473 + separator_len__351
                start__353 = t901
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        return result__350
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_join____T__string(self__631 *_goml_vec_string, separator__632 string) string {
    var t905 int
    var inline1712 int = vec_len__Vec_6string(self__631)
    t905 = inline1712
    var parts__633 *_goml_vec_string
    var inline1710 *_goml_vec_string = vec_with_capacity__Vec_6string(t905)
    parts__633 = inline1710
    var t906 int
    var inline1708 int = vec_len__Vec_6string(self__631)
    t906 = inline1708
    var t907 FnIterator__isize
    var inline1702 int = 0
    var inline1703 *ref_int_x = ref__Ref_3int(inline1702)
    var inline1704 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1703,
        end_1: t906,
    }
    var inline1705 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1704)
    }
    var inline1706 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1705)
    t907 = inline1706
    var for_iter734 FnIterator__isize
    for_iter734 = t907
    Loop_loop922:
    for {
        var for_next735 Option__isize
        var inline1678 func() Option__isize = for_iter734.next_fn
        var inline1679 Option__isize = inline1678()
        for_next735 = inline1679
        switch for_next735._tag {
        case 0:
            break Loop_loop922
        case 1:
            var x736 int = for_next735._v1_0
            var t924 string = vec_get__Vec_6string(self__631, x736)
            var t925 string
            t925 = t924
            vec_push__Vec_6string(parts__633, t925)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t909 int
    var inline1699 int = vec_len__Vec_6string(parts__633)
    t909 = inline1699
    var t910 int = t909 * 2
    var result__635 *_goml_vec_string
    var inline1697 *_goml_vec_string = vec_with_capacity__Vec_6string(t910)
    result__635 = inline1697
    var t911 int
    var inline1695 int = vec_len__Vec_6string(parts__633)
    t911 = inline1695
    var t912 FnIterator__isize
    var inline1689 int = 0
    var inline1690 *ref_int_x = ref__Ref_3int(inline1689)
    var inline1691 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1690,
        end_1: t911,
    }
    var inline1692 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1691)
    }
    var inline1693 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1692)
    t912 = inline1693
    var for_iter738 FnIterator__isize
    for_iter738 = t912
    Loop_loop915:
    for {
        var for_next739 Option__isize
        var inline1685 func() Option__isize = for_iter738.next_fn
        var inline1686 Option__isize = inline1685()
        for_next739 = inline1686
        switch for_next739._tag {
        case 0:
            break Loop_loop915
        case 1:
            var x740 int = for_next739._v1_0
            var t920 bool = x740 > 0
            if t920 {
                vec_push__Vec_6string(result__635, separator__632)
            } else {}
            var t918 string = vec_get__Vec_6string(parts__633, x740)
            vec_push__Vec_6string(result__635, t918)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var t914 string = __goml_builtin_string_concat(result__635)
    return t914
}

func _goml_m_inherent_i_string_i_string_i_split__once(self__355 string, separator__356 string) _goml_m_Option_____o_string_c_string_q_ {
    var separator_len__357 int
    var inline1723 int = _goml_runtime_core_string_len(separator__356)
    separator_len__357 = inline1723
    var value_len__358 int
    var inline1721 int = _goml_runtime_core_string_len(self__355)
    value_len__358 = inline1721
    var t931 bool = separator_len__357 == 0
    if t931 {
        return _goml_m_Option_____o_string_c_string_q_{
            _tag: 0,
        }
    } else {
        var mtmp478 Option__isize
        var inline1719 Option__isize = string_find_from(self__355, separator__356, 0)
        mtmp478 = inline1719
        switch mtmp478._tag {
        case 0:
            return _goml_m_Option_____o_string_c_string_q_{
                _tag: 0,
            }
        case 1:
            var x479 int = mtmp478._v1_0
            var t934 string
            var inline1716 int = 0
            var inline1717 string = string_byte_slice(self__355, inline1716, x479)
            t934 = inline1717
            var t935 int = x479 + separator_len__357
            var t936 string
            var inline1714 string = string_byte_slice(self__355, t935, value_len__358)
            t936 = inline1714
            var t937 Tuple2_6string_6string = Tuple2_6string_6string{
                _0: t934,
                _1: t936,
            }
            var t938 _goml_m_Option_____o_string_c_string_q_ = _goml_m_Option_____o_string_c_string_q_{
                _tag: 1,
                _v1_0: t937,
            }
            return t938
        default:
            panic("non-exhaustive match")
        }
    }
}

func _goml_m_inherent_i_Option_i_Op_h43ad6114bf7944dd0f967bbedf6419b6_ing_c_string_q_(self__720 _goml_m_Option_____o_string_c_string_q_, fallback__721 Tuple2_6string_6string) Tuple2_6string_6string {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 Tuple2_6string_6string = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_string_i_string_i_lines(self__360 string) *_goml_vec_string {
    var result__361 *_goml_vec_string = _goml_m_inherent_i_string_i_string_i_split(self__360, "\n")
    var t965 bool
    var inline1758 int = vec_len__Vec_6string(result__361)
    var inline1759 bool = inline1758 == 0
    t965 = inline1759
    var t966 bool = !t965
    var jp964 bool
    if t966 {
        var t967 int
        var inline1725 int = vec_len__Vec_6string(result__361)
        t967 = inline1725
        var t968 int = t967 - 1
        var t969 string = vec_get__Vec_6string(result__361, t968)
        var t970 bool = t969 == ""
        jp964 = t970
    } else {
        jp964 = false
    }
    if jp964 {
        var inline1727 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(result__361)
        var inline1728 bool = inline1727 == 0
        if inline1728 {} else {
            var inline1729 int = inline1727 - 1
            vec_get__Vec_6string(result__361, inline1729)
            var inline1731 int = inline1727 - 1
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(result__361, inline1731)
        }
    } else {}
    var t946 int
    var inline1756 int = vec_len__Vec_6string(result__361)
    t946 = inline1756
    var t947 FnIterator__isize
    var inline1750 int = 0
    var inline1751 *ref_int_x = ref__Ref_3int(inline1750)
    var inline1752 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1751,
        end_1: t946,
    }
    var inline1753 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1752)
    }
    var inline1754 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1753)
    t947 = inline1754
    var for_iter482 FnIterator__isize
    for_iter482 = t947
    Loop_loop949:
    for {
        var for_next483 Option__isize
        var inline1746 func() Option__isize = for_iter482.next_fn
        var inline1747 Option__isize = inline1746()
        for_next483 = inline1747
        switch for_next483._tag {
        case 0:
            break Loop_loop949
        case 1:
            var x484 int = for_next483._v1_0
            var line__363 string = vec_get__Vec_6string(result__361, x484)
            var t957 int
            var inline1744 int = _goml_runtime_core_string_len(line__363)
            t957 = inline1744
            var t958 bool = t957 > 0
            var jp953 bool
            if t958 {
                var t959 int
                var inline1737 int = _goml_runtime_core_string_len(line__363)
                t959 = inline1737
                var t960 int = t959 - 1
                var t961 uint8
                var inline1735 uint8 = _goml_runtime_core_string_byte_get(line__363, t960)
                t961 = inline1735
                var t962 bool = t961 == 13
                jp953 = t962
            } else {
                jp953 = false
            }
            if jp953 {
                vec_get__Vec_6string(result__361, x484)
                var t954 int
                var inline1742 int = _goml_runtime_core_string_len(line__363)
                t954 = inline1742
                var t955 int = t954 - 1
                var value488 string
                var inline1739 int = 0
                var inline1740 string = string_byte_slice(line__363, inline1739, t955)
                value488 = inline1740
                vec_set__Vec_6string(result__361, x484, value488)
                continue
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return result__361
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__526 *_goml_vec_string) int {
    var t973 int = vec_len__Vec_6string(self__526)
    return t973
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1761 int64 = int64(int(self__285))
    var inline1762 string = signed_decimal_string(inline1761)
    return inline1762
}

func _goml_m_inherent_i_string_i_string_i_find(self__322 string, expected__323 string) Option__isize {
    var t979 Option__isize = string_find_from(self__322, expected__323, 0)
    return t979
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

func _goml_m_inherent_i_string_i_string_i_rfind(self__338 string, expected__339 string) Option__isize {
    var value_len__340 int
    var inline1766 int = _goml_runtime_core_string_len(self__338)
    value_len__340 = inline1766
    var expected_len__341 int
    var inline1764 int = _goml_runtime_core_string_len(expected__339)
    expected_len__341 = inline1764
    var t988 bool = expected_len__341 > value_len__340
    if t988 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var start__342 int = value_len__340 - expected_len__341
        Loop_loop990:
        for {
            var t991 bool = start__342 >= 0
            if t991 {
                var t993 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(self__338, start__342, expected__339)
                if t993 {
                    var t994 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: start__342,
                    }
                    return t994
                } else {
                    var compound_old458 int = start__342
                    var compound_value459 int = 1
                    var t995 int = compound_old458 - compound_value459
                    start__342 = t995
                    continue
                }
            } else {
                break Loop_loop990
            }
        }
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_starts__with__at(self__316 string, start__317 int, prefix__318 string) bool {
    var value_len__319 int
    var inline1778 int = _goml_runtime_core_string_len(self__316)
    value_len__319 = inline1778
    var prefix_len__320 int
    var inline1776 int = _goml_runtime_core_string_len(prefix__318)
    prefix_len__320 = inline1776
    var t1018 bool = start__317 < 0
    var jp1015 bool
    if t1018 {
        jp1015 = true
    } else {
        var t1019 bool = start__317 > value_len__319
        jp1015 = t1019
    }
    var jp1005 bool
    if jp1015 {
        jp1005 = true
    } else {
        var t1016 int = value_len__319 - start__317
        var t1017 bool = prefix_len__320 > t1016
        jp1005 = t1017
    }
    if jp1005 {
        return false
    } else {
        var end__321 int = start__317 + prefix_len__320
        var t1012 bool
        var inline1774 bool = string_is_char_boundary(self__316, start__317)
        t1012 = inline1774
        var jp1009 bool
        if t1012 {
            var inline1770 bool = string_is_char_boundary(self__316, end__321)
            jp1009 = inline1770
        } else {
            jp1009 = false
        }
        if jp1009 {
            var t1010 string
            var inline1772 string = string_byte_slice(self__316, start__317, end__321)
            t1010 = inline1772
            var t1011 bool = t1010 == prefix__318
            return t1011
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_char__count(self__326 string) int {
    var count__327 int = 0
    var t1025 FnIterator__char
    var inline1784 *ref_int_x = ref__Ref_3int(0)
    var inline1785 closure_env_inherent_string_string_chars_1 = closure_env_inherent_string_string_chars_1{
        self_0: self__326,
        index_1: inline1784,
    }
    var inline1786 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(inline1785)
    }
    var inline1787 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1786)
    t1025 = inline1787
    var for_iter428 FnIterator__char
    for_iter428 = t1025
    Loop_loop1027:
    for {
        var for_next429 Option__char
        var inline1780 func() Option__char = for_iter428.next_fn
        var inline1781 Option__char = inline1780()
        for_next429 = inline1781
        switch for_next429._tag {
        case 0:
            break Loop_loop1027
        case 1:
            var compound_old431 int = count__327
            var compound_value432 int = 1
            var t1029 int = compound_old431 + compound_value432
            count__327 = t1029
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return count__327
}

func _goml_m_inherent_i_string_i_string_i_slice__chars(self__328 string, start__329 int, end__330 int) Option__string {
    var t1071 bool = start__329 < 0
    var jp1036 bool
    if t1071 {
        jp1036 = true
    } else {
        var t1072 bool = end__330 < start__329
        jp1036 = t1072
    }
    if jp1036 {
        return Option__string{
            _tag: 0,
        }
    } else {
        var char_index__331 int = 0
        var t1069 bool = start__329 == 0
        var jp1038 Option__isize
        if t1069 {
            var t1070 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: 0,
            }
            jp1038 = t1070
        } else {
            jp1038 = Option__isize{
                _tag: 0,
            }
        }
        var start_byte__332 Option__isize = jp1038
        var t1067 bool = end__330 == 0
        var jp1040 Option__isize
        if t1067 {
            var t1068 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: 0,
            }
            jp1040 = t1068
        } else {
            jp1040 = Option__isize{
                _tag: 0,
            }
        }
        var end_byte__333 Option__isize = jp1040
        var t1041 _goml_m_FnIterator_____o_isize_c_char_q_
        var inline1799 *ref_int_x = ref__Ref_3int(0)
        var inline1800 closure_env_inherent_string_string_char_indices_2 = closure_env_inherent_string_string_char_indices_2{
            index_0: inline1799,
            self_1: self__328,
        }
        var inline1801 func() _goml_m_Option_____o_isize_c_char_q_ = func() _goml_m_Option_____o_isize_c_char_q_ {
            return _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(inline1800)
        }
        var inline1802 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(inline1801)
        t1041 = inline1802
        var for_iter435 _goml_m_FnIterator_____o_isize_c_char_q_
        for_iter435 = t1041
        Loop_loop1057:
        for {
            var for_next436 _goml_m_Option_____o_isize_c_char_q_
            var inline1789 func() _goml_m_Option_____o_isize_c_char_q_ = for_iter435.next_fn
            var inline1790 _goml_m_Option_____o_isize_c_char_q_ = inline1789()
            for_next436 = inline1790
            switch for_next436._tag {
            case 0:
                break Loop_loop1057
            case 1:
                var x437 Tuple2_3int_4char = for_next436._v1_0
                var x439 int = x437._0
                var t1065 bool = char_index__331 == start__329
                if t1065 {
                    var t1066 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x439,
                    }
                    start_byte__332 = t1066
                } else {}
                var t1063 bool = char_index__331 == end__330
                if t1063 {
                    var t1064 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x439,
                    }
                    end_byte__333 = t1064
                } else {}
                var compound_old445 int = char_index__331
                var compound_value446 int = 1
                var t1061 int = compound_old445 + compound_value446
                char_index__331 = t1061
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t1054 bool = char_index__331 == start__329
        if t1054 {
            var t1055 int
            var inline1792 int = _goml_runtime_core_string_len(self__328)
            t1055 = inline1792
            var t1056 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: t1055,
            }
            start_byte__332 = t1056
        } else {}
        var t1051 bool = char_index__331 == end__330
        if t1051 {
            var t1052 int
            var inline1794 int = _goml_runtime_core_string_len(self__328)
            t1052 = inline1794
            var t1053 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: t1052,
            }
            end_byte__333 = t1053
        } else {}
        var mtmp453 Tuple2_13Option__isize_13Option__isize = Tuple2_13Option__isize_13Option__isize{
            _0: start_byte__332,
            _1: end_byte__333,
        }
        var x454 Option__isize = mtmp453._0
        var x455 Option__isize = mtmp453._1
        switch x455._tag {
        case 1:
            var x456 int = x455._v1_0
            switch x454._tag {
            case 1:
                var x457 int = x454._v1_0
                var t1049 string
                var inline1796 string = string_byte_slice(self__328, x457, x456)
                t1049 = inline1796
                var t1050 Option__string = Option__string{
                    _tag: 1,
                    _v1_0: t1049,
                }
                return t1050
            default:
                return Option__string{
                    _tag: 0,
                }
            }
        default:
            return Option__string{
                _tag: 0,
            }
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_replace(self__364 string, expected__365 string, replacement__366 string) string {
    var t1081 int
    var inline1820 int = _goml_runtime_core_string_len(expected__365)
    t1081 = inline1820
    var t1082 bool = t1081 == 0
    if t1082 {
        return self__364
    } else {
        var parts__367 *_goml_vec_string
        var inline1818 *_goml_vec_string = vec_new__Vec_6string()
        parts__367 = inline1818
        var start__368 int = 0
        Loop_loop_expr1085:
        for {
            var mtmp491 Option__isize = string_find_from(self__364, expected__365, start__368)
            switch mtmp491._tag {
            case 0:
                var t1087 int
                var inline1808 int = _goml_runtime_core_string_len(self__364)
                t1087 = inline1808
                var t1088 string
                var inline1806 string = string_byte_slice(self__364, start__368, t1087)
                t1088 = inline1806
                vec_push__Vec_6string(parts__367, t1088)
                break Loop_loop_expr1085
            case 1:
                var x492 int = mtmp491._v1_0
                var t1089 string
                var inline1816 string = string_byte_slice(self__364, start__368, x492)
                t1089 = inline1816
                vec_push__Vec_6string(parts__367, t1089)
                vec_push__Vec_6string(parts__367, replacement__366)
                var t1090 int
                var inline1810 int = _goml_runtime_core_string_len(expected__365)
                t1090 = inline1810
                var t1091 int = x492 + t1090
                start__368 = t1091
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t1084 string = __goml_builtin_string_concat(parts__367)
        return t1084
    }
}

func _goml_m_inherent_i_string_i_string_i_repeat(self__370 string, count__371 int) string {
    var t1104 bool = count__371 <= 0
    var jp1097 bool
    if t1104 {
        jp1097 = true
    } else {
        var t1105 int
        var inline1822 int = _goml_runtime_core_string_len(self__370)
        t1105 = inline1822
        var t1106 bool = t1105 == 0
        jp1097 = t1106
    }
    if jp1097 {
        return ""
    } else {
        var parts__372 *_goml_vec_string
        var inline1836 *_goml_vec_string = vec_with_capacity__Vec_6string(count__371)
        parts__372 = inline1836
        var t1098 FnIterator__isize
        var inline1830 int = 0
        var inline1831 *ref_int_x = ref__Ref_3int(inline1830)
        var inline1832 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1831,
            end_1: count__371,
        }
        var inline1833 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1832)
        }
        var inline1834 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1833)
        t1098 = inline1834
        var for_iter498 FnIterator__isize
        for_iter498 = t1098
        Loop_loop1101:
        for {
            var for_next499 Option__isize
            var inline1826 func() Option__isize = for_iter498.next_fn
            var inline1827 Option__isize = inline1826()
            for_next499 = inline1827
            switch for_next499._tag {
            case 0:
                break Loop_loop1101
            case 1:
                vec_push__Vec_6string(parts__372, self__370)
                continue
            default:
                panic("non-exhaustive match")
            }
        }
        var t1100 string = __goml_builtin_string_concat(parts__372)
        return t1100
    }
}

func _goml_m_inherent_i_string_i_string_i_is__ascii(self__373 string) bool {
    var t1109 int
    var inline1850 int = _goml_runtime_core_string_len(self__373)
    t1109 = inline1850
    var t1110 FnIterator__isize
    var inline1844 int = 0
    var inline1845 *ref_int_x = ref__Ref_3int(inline1844)
    var inline1846 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1845,
        end_1: t1109,
    }
    var inline1847 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1846)
    }
    var inline1848 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1847)
    t1110 = inline1848
    var for_iter502 FnIterator__isize
    for_iter502 = t1110
    Loop_loop1112:
    for {
        var for_next503 Option__isize
        var inline1840 func() Option__isize = for_iter502.next_fn
        var inline1841 Option__isize = inline1840()
        for_next503 = inline1841
        switch for_next503._tag {
        case 0:
            break Loop_loop1112
        case 1:
            var x504 int = for_next503._v1_0
            var t1115 uint8
            var inline1838 uint8 = _goml_runtime_core_string_byte_get(self__373, x504)
            t1115 = inline1838
            var t1116 bool = t1115 > 127
            if t1116 {
                return false
            } else {
                continue
            }
        default:
            panic("non-exhaustive match")
        }
    }
    return true
}

func _goml_m_inherent_i_string_i_string_i_eq__ignore__ascii__case(self__375 string, other__376 string) bool {
    var t1121 int
    var inline1874 int = _goml_runtime_core_string_len(self__375)
    t1121 = inline1874
    var t1122 int
    var inline1872 int = _goml_runtime_core_string_len(other__376)
    t1122 = inline1872
    var t1123 bool = t1121 != t1122
    if t1123 {
        return false
    } else {
        var t1124 int
        var inline1870 int = _goml_runtime_core_string_len(self__375)
        t1124 = inline1870
        var t1125 FnIterator__isize
        var inline1864 int = 0
        var inline1865 *ref_int_x = ref__Ref_3int(inline1864)
        var inline1866 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
            current_0: inline1865,
            end_1: t1124,
        }
        var inline1867 func() Option__isize = func() Option__isize {
            return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1866)
        }
        var inline1868 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1867)
        t1125 = inline1868
        var for_iter506 FnIterator__isize
        for_iter506 = t1125
        Loop_loop1127:
        for {
            var for_next507 Option__isize
            var inline1860 func() Option__isize = for_iter506.next_fn
            var inline1861 Option__isize = inline1860()
            for_next507 = inline1861
            switch for_next507._tag {
            case 0:
                break Loop_loop1127
            case 1:
                var x508 int = for_next507._v1_0
                var t1130 uint8
                var inline1858 uint8 = _goml_runtime_core_string_byte_get(self__375, x508)
                t1130 = inline1858
                var t1131 uint8
                var inline1856 uint8 = _goml_runtime_core_string_byte_get(other__376, x508)
                t1131 = inline1856
                var t1132 bool
                var inline1852 uint8 = ascii_to_lowercase(t1130)
                var inline1853 uint8 = ascii_to_lowercase(t1131)
                var inline1854 bool = inline1852 == inline1853
                t1132 = inline1854
                var t1133 bool = !t1132
                if t1133 {
                    return false
                } else {
                    continue
                }
            default:
                panic("non-exhaustive match")
            }
        }
        return true
    }
}

func _goml_m_inherent_i_string_i_string_i_to__ascii__lowercase(self__378 string) string {
    var values__379 *_goml_vec_uint8
    var inline1892 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__378)
    values__379 = inline1892
    var t1136 int
    var inline1890 int = vec_len__Vec_5uint8(values__379)
    t1136 = inline1890
    var t1137 FnIterator__isize
    var inline1884 int = 0
    var inline1885 *ref_int_x = ref__Ref_3int(inline1884)
    var inline1886 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1885,
        end_1: t1136,
    }
    var inline1887 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1886)
    }
    var inline1888 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1887)
    t1137 = inline1888
    var for_iter510 FnIterator__isize
    for_iter510 = t1137
    var inline1877 uint8 = 97 - 65
    Loop_loop1139:
    for {
        var for_next511 Option__isize
        var inline1880 func() Option__isize = for_iter510.next_fn
        var inline1881 Option__isize = inline1880()
        for_next511 = inline1881
        switch for_next511._tag {
        case 0:
            break Loop_loop1139
        case 1:
            var x512 int = for_next511._v1_0
            vec_get__Vec_5uint8(values__379, x512)
            var t1141 uint8 = vec_get__Vec_5uint8(values__379, x512)
            var value516 uint8
            var inline1876 bool = ascii_is_uppercase(t1141)
            if inline1876 {
                var inline1878 uint8 = t1141 + inline1877
                value516 = inline1878
            } else {
                value516 = t1141
            }
            vec_set__Vec_5uint8(values__379, x512, value516)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var mtmp519 Tuple2_4bool_6string = string_from_utf8(values__379)
    var x521 string = mtmp519._1
    return x521
}

func _goml_m_inherent_i_string_i_string_i_to__ascii__uppercase(self__382 string) string {
    var values__383 *_goml_vec_uint8
    var inline1910 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__382)
    values__383 = inline1910
    var t1145 int
    var inline1908 int = vec_len__Vec_5uint8(values__383)
    t1145 = inline1908
    var t1146 FnIterator__isize
    var inline1902 int = 0
    var inline1903 *ref_int_x = ref__Ref_3int(inline1902)
    var inline1904 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: inline1903,
        end_1: t1145,
    }
    var inline1905 func() Option__isize = func() Option__isize {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1904)
    }
    var inline1906 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1905)
    t1146 = inline1906
    var for_iter522 FnIterator__isize
    for_iter522 = t1146
    var inline1895 uint8 = 97 - 65
    Loop_loop1148:
    for {
        var for_next523 Option__isize
        var inline1898 func() Option__isize = for_iter522.next_fn
        var inline1899 Option__isize = inline1898()
        for_next523 = inline1899
        switch for_next523._tag {
        case 0:
            break Loop_loop1148
        case 1:
            var x524 int = for_next523._v1_0
            vec_get__Vec_5uint8(values__383, x524)
            var t1150 uint8 = vec_get__Vec_5uint8(values__383, x524)
            var value528 uint8
            var inline1894 bool = ascii_is_lowercase(t1150)
            if inline1894 {
                var inline1896 uint8 = t1150 - inline1895
                value528 = inline1896
            } else {
                value528 = t1150
            }
            vec_set__Vec_5uint8(values__383, x524, value528)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    var mtmp531 Tuple2_4bool_6string = string_from_utf8(values__383)
    var x533 string = mtmp531._1
    return x533
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1156 int = _goml_runtime_core_string_len(self__289)
    return t1156
}

func ascii_is_whitespace(value__393 uint8) bool {
    var t1173 bool = value__393 == 9
    var jp1171 bool
    if t1173 {
        jp1171 = true
    } else {
        var t1174 bool = value__393 == 10
        jp1171 = t1174
    }
    var jp1168 bool
    if jp1171 {
        jp1168 = true
    } else {
        var t1172 bool = value__393 == 11
        jp1168 = t1172
    }
    var jp1165 bool
    if jp1168 {
        jp1165 = true
    } else {
        var t1169 bool = value__393 == 12
        jp1165 = t1169
    }
    var jp1162 bool
    if jp1165 {
        jp1162 = true
    } else {
        var t1166 bool = value__393 == 13
        jp1162 = t1166
    }
    if jp1162 {
        return true
    } else {
        var t1163 bool = value__393 == 32
        return t1163
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1177 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1177
}

func string_find_from(value__386 string, expected__387 string, start__388 int) Option__isize {
    var value_len__389 int
    var inline1921 int = _goml_runtime_core_string_len(value__386)
    value_len__389 = inline1921
    var expected_len__390 int
    var inline1919 int = _goml_runtime_core_string_len(expected__387)
    expected_len__390 = inline1919
    var t1208 bool = start__388 < 0
    var jp1191 bool
    if t1208 {
        jp1191 = true
    } else {
        var t1209 bool = start__388 > value_len__389
        jp1191 = t1209
    }
    if jp1191 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var t1194 bool = expected_len__390 == 0
        if t1194 {
            var t1195 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: start__388,
            }
            return t1195
        } else {
            var t1198 int = value_len__389 - start__388
            var t1199 bool = expected_len__390 > t1198
            if t1199 {
                return Option__isize{
                    _tag: 0,
                }
            } else {
                var limit__391 int = value_len__389 - expected_len__390
                var index__392 int = start__388
                Loop_loop1201:
                for {
                    var t1202 bool = index__392 <= limit__391
                    if t1202 {
                        var t1204 bool = _goml_m_inherent_i_string_i_string_i_starts__with__at(value__386, index__392, expected__387)
                        if t1204 {
                            var t1205 Option__isize = Option__isize{
                                _tag: 1,
                                _v1_0: index__392,
                            }
                            return t1205
                        } else {
                            var compound_old534 int = index__392
                            var compound_value535 int = 1
                            var t1206 int = compound_old534 + compound_value535
                            index__392 = t1206
                            continue
                        }
                    } else {
                        break Loop_loop1201
                    }
                }
                return Option__isize{
                    _tag: 0,
                }
            }
        }
    }
}

func __goml_builtin_string_concat(values__215 *_goml_vec_string) string {
    var length__216 int = 0
    var value_index__217 int = 0
    Loop_loop1240:
    for {
        var t1241 int
        var inline1927 int = vec_len__Vec_6string(values__215)
        t1241 = inline1927
        var t1242 bool = value_index__217 < t1241
        if t1242 {
            var compound_old365 int = length__216
            var t1243 string = vec_get__Vec_6string(values__215, value_index__217)
            var compound_value366 int
            var inline1925 int = _goml_runtime_core_string_len(t1243)
            compound_value366 = inline1925
            var t1244 int = compound_old365 + compound_value366
            length__216 = t1244
            var compound_old368 int = value_index__217
            var compound_value369 int = 1
            var t1246 int = compound_old368 + compound_value369
            value_index__217 = t1246
            continue
        } else {
            break Loop_loop1240
        }
    }
    var bytes__218 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(length__216)
    value_index__217 = 0
    Loop_loop1228:
    for {
        var t1229 int
        var inline1935 int = vec_len__Vec_6string(values__215)
        t1229 = inline1935
        var t1230 bool = value_index__217 < t1229
        if t1230 {
            var value__219 string = vec_get__Vec_6string(values__215, value_index__217)
            var byte_index__220 int = 0
            Loop_loop1234:
            for {
                var t1235 int
                var inline1933 int = _goml_runtime_core_string_len(value__219)
                t1235 = inline1933
                var t1236 bool = byte_index__220 < t1235
                if t1236 {
                    var t1237 uint8
                    var inline1931 uint8 = _goml_runtime_core_string_byte_get(value__219, byte_index__220)
                    t1237 = inline1931
                    vec_push__Vec_5uint8(bytes__218, t1237)
                    var compound_old374 int = byte_index__220
                    var compound_value375 int = 1
                    var t1238 int = compound_old374 + compound_value375
                    byte_index__220 = t1238
                    continue
                } else {
                    break Loop_loop1234
                }
            }
            var compound_old378 int = value_index__217
            var compound_value379 int = 1
            var t1232 int = compound_old378 + compound_value379
            value_index__217 = t1232
            continue
        } else {
            break Loop_loop1228
        }
    }
    var mtmp382 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__218)
    var x384 string = mtmp382._1
    return x384
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1262 int64 = int64(int(value__222))
    var inline1941 bool = t1262 < 0
    if inline1941 {
        var inline1942 uint64 = uint64(int64(t1262))
        var inline1943 uint64 = 0 - inline1942
        var inline1944 string = decimal_string(inline1943)
        var inline1945 string = "-" + inline1944
        return inline1945
    } else {
        var inline1946 uint64 = uint64(int64(t1262))
        var inline1947 string = decimal_string(inline1946)
        return inline1947
    }
}

func ascii_to_lowercase(value__396 uint8) uint8 {
    var t1304 bool
    var inline1961 bool = value__396 >= 65
    if inline1961 {
        var inline1962 bool = value__396 <= 90
        t1304 = inline1962
    } else {
        t1304 = false
    }
    if t1304 {
        var t1305 uint8 = 97 - 65
        var t1306 uint8 = value__396 + t1305
        return t1306
    } else {
        return value__396
    }
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1311:
    for {
        var t1312 int
        var inline1964 int = _goml_runtime_core_string_len(x397)
        t1312 = inline1964
        var t1313 bool = index__279 < t1312
        if t1313 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1315 int = compound_old402 + x401
                index__279 = t1315
                continue
            } else {
                var t1317 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1317
            }
        } else {
            break Loop_loop1311
        }
    }
    var t1310 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1310
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1333 bool = string_is_char_boundary(value__274, start__275)
    var jp1330 bool
    if t1333 {
        var t1334 bool = string_is_char_boundary(value__274, end__276)
        jp1330 = t1334
    } else {
        jp1330 = false
    }
    if jp1330 {
        var t1331 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1331
    } else {
        var t1332 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1332
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__507 func() Option__isize) FnIterator__isize {
    var t1337 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__507,
    }
    return t1337
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__string(self__531 *_goml_vec_string, len__532 int) struct{} {
    vec_truncate__Vec_6string(self__531, len__532)
    return struct{}{}
}

func signed_decimal_string(value__214 int64) string {
    var t1346 bool = value__214 < 0
    if t1346 {
        var t1347 uint64 = uint64(int64(value__214))
        var t1348 uint64 = 0 - t1347
        var t1349 string = decimal_string(t1348)
        var t1350 string = "-" + t1349
        return t1350
    } else {
        var t1351 uint64 = uint64(int64(value__214))
        var t1352 string = decimal_string(t1351)
        return t1352
    }
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1366 bool = index__269 < 0
    var jp1358 bool
    if t1366 {
        jp1358 = true
    } else {
        var t1367 int
        var inline1969 int = _goml_runtime_core_string_len(value__268)
        t1367 = inline1969
        var t1368 bool = index__269 > t1367
        jp1358 = t1368
    }
    if jp1358 {
        return false
    } else {
        var t1361 int
        var inline1973 int = _goml_runtime_core_string_len(value__268)
        t1361 = inline1973
        var t1362 bool = index__269 == t1361
        if t1362 {
            return true
        } else {
            var t1363 uint8
            var inline1971 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1363 = inline1971
            var t1364_rhs uint8 = 192
            var t1364 uint8 = t1363 & t1364_rhs
            var t1365 bool = t1364 != 128
            return t1365
        }
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__507 func() Option__char) FnIterator__char {
    var t1377 FnIterator__char = FnIterator__char{
        next_fn: next_fn__507,
    }
    return t1377
}

func _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(next_fn__507 func() _goml_m_Option_____o_isize_c_char_q_) _goml_m_FnIterator_____o_isize_c_char_q_ {
    var t1380 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_FnIterator_____o_isize_c_char_q_{
        next_fn: next_fn__507,
    }
    return t1380
}

func ascii_is_uppercase(value__394 uint8) bool {
    var t1385 bool = value__394 >= 65
    if t1385 {
        var t1386 bool = value__394 <= 90
        return t1386
    } else {
        return false
    }
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1505 bool = index__259 < 0
    var jp1503 bool
    if t1505 {
        jp1503 = true
    } else {
        var t1506 bool = index__259 >= length__260
        jp1503 = t1506
    }
    if jp1503 {
        var inline1975 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1975
    } else {
        var t1390 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1390))
        var t1393 bool = first__261 < 128
        if t1393 {
            var inline1977 int = 1
            var inline1978 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline1978._tag {
            case 0:
                var inline1979 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1979
            case 1:
                var inline1980 rune = inline1978._v1_0
                var inline1982 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1980,
                    _2: inline1977,
                }
                return inline1982
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1397 bool = first__261 < 194
            if t1397 {
                var inline1984 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1984
            } else {
                var t1401 bool = first__261 < 224
                if t1401 {
                    var t1414 int = length__260 - index__259
                    var t1415 bool = t1414 < 2
                    if t1415 {
                        var inline1986 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1986
                    } else {
                        var t1403 int = index__259 + 1
                        var t1404 uint8
                        var inline2000 uint8 = _goml_runtime_core_string_byte_get(value__258, t1403)
                        t1404 = inline2000
                        var second__262 uint32 = uint32(uint8(t1404))
                        var t1407 bool
                        var inline1997 bool = second__262 < 128
                        if inline1997 {
                            t1407 = true
                        } else {
                            var inline1998 bool = second__262 > 191
                            t1407 = inline1998
                        }
                        if t1407 {
                            var inline1988 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1988
                        } else {
                            var t1409_rhs uint32 = 31
                            var t1409 uint32 = first__261 & t1409_rhs
                            var t1410_rhs int = 6
                            var t1410 uint32 = t1409 << t1410_rhs
                            var t1411_rhs uint32 = 63
                            var t1411 uint32 = second__262 & t1411_rhs
                            var t1412 uint32 = t1410 | t1411
                            var inline1990 int = 2
                            var inline1991 Option__char = __goml_builtin_char_from_uint32(t1412)
                            switch inline1991._tag {
                            case 0:
                                var inline1992 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1992
                            case 1:
                                var inline1993 rune = inline1991._v1_0
                                var inline1995 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1993,
                                    _2: inline1990,
                                }
                                return inline1995
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1419 bool = first__261 < 240
                    if t1419 {
                        var t1452 int = length__260 - index__259
                        var t1453 bool = t1452 < 3
                        if t1453 {
                            var inline2002 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2002
                        } else {
                            var t1421 int = index__259 + 1
                            var t1422 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1421)
                            var second__263 uint32 = uint32(uint8(t1422))
                            var t1423 int = index__259 + 2
                            var t1424 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1423)
                            var third__264 uint32 = uint32(uint8(t1424))
                            var t1450 bool = utf8_invalid_continuation(second__263)
                            var jp1445 bool
                            if t1450 {
                                jp1445 = true
                            } else {
                                var inline2004 bool = third__264 < 128
                                if inline2004 {
                                    jp1445 = true
                                } else {
                                    var inline2005 bool = third__264 > 191
                                    jp1445 = inline2005
                                }
                            }
                            var jp1439 bool
                            if jp1445 {
                                jp1439 = true
                            } else {
                                var t1448 bool = first__261 == 224
                                if t1448 {
                                    var t1449 bool = second__263 < 160
                                    jp1439 = t1449
                                } else {
                                    jp1439 = false
                                }
                            }
                            var jp1428 bool
                            if jp1439 {
                                jp1428 = true
                            } else {
                                var t1442 bool = first__261 == 237
                                if t1442 {
                                    var t1443 bool = second__263 >= 160
                                    jp1428 = t1443
                                } else {
                                    jp1428 = false
                                }
                            }
                            if jp1428 {
                                var inline2007 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2007
                            } else {
                                var t1430_rhs uint32 = 15
                                var t1430 uint32 = first__261 & t1430_rhs
                                var t1431_rhs int = 12
                                var t1431 uint32 = t1430 << t1431_rhs
                                var t1432_rhs uint32 = 63
                                var t1432 uint32 = second__263 & t1432_rhs
                                var t1433_rhs int = 6
                                var t1433 uint32 = t1432 << t1433_rhs
                                var t1434 uint32 = t1431 | t1433
                                var t1435_rhs uint32 = 63
                                var t1435 uint32 = third__264 & t1435_rhs
                                var t1436 uint32 = t1434 | t1435
                                var inline2009 int = 3
                                var inline2010 Option__char = __goml_builtin_char_from_uint32(t1436)
                                switch inline2010._tag {
                                case 0:
                                    var inline2011 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2011
                                case 1:
                                    var inline2012 rune = inline2010._v1_0
                                    var inline2014 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2012,
                                        _2: inline2009,
                                    }
                                    return inline2014
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1457 bool = first__261 < 245
                        if t1457 {
                            var t1498 int = length__260 - index__259
                            var t1499 bool = t1498 < 4
                            if t1499 {
                                var t1500 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1500
                            } else {
                                var t1459 int = index__259 + 1
                                var t1460 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1459)
                                var second__265 uint32 = uint32(uint8(t1460))
                                var t1461 int = index__259 + 2
                                var t1462 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1461)
                                var third__266 uint32 = uint32(uint8(t1462))
                                var t1463 int = index__259 + 3
                                var t1464 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1463)
                                var fourth__267 uint32 = uint32(uint8(t1464))
                                var t1496 bool = utf8_invalid_continuation(second__265)
                                var jp1494 bool
                                if t1496 {
                                    jp1494 = true
                                } else {
                                    var t1497 bool = utf8_invalid_continuation(third__266)
                                    jp1494 = t1497
                                }
                                var jp1488 bool
                                if jp1494 {
                                    jp1488 = true
                                } else {
                                    var t1495 bool = utf8_invalid_continuation(fourth__267)
                                    jp1488 = t1495
                                }
                                var jp1482 bool
                                if jp1488 {
                                    jp1482 = true
                                } else {
                                    var t1491 bool = first__261 == 240
                                    if t1491 {
                                        var t1492 bool = second__265 < 144
                                        jp1482 = t1492
                                    } else {
                                        jp1482 = false
                                    }
                                }
                                var jp1468 bool
                                if jp1482 {
                                    jp1468 = true
                                } else {
                                    var t1485 bool = first__261 == 244
                                    if t1485 {
                                        var t1486 bool = second__265 > 143
                                        jp1468 = t1486
                                    } else {
                                        jp1468 = false
                                    }
                                }
                                if jp1468 {
                                    var t1469 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1469
                                } else {
                                    var t1470_rhs uint32 = 7
                                    var t1470 uint32 = first__261 & t1470_rhs
                                    var t1471_rhs int = 18
                                    var t1471 uint32 = t1470 << t1471_rhs
                                    var t1472_rhs uint32 = 63
                                    var t1472 uint32 = second__265 & t1472_rhs
                                    var t1473_rhs int = 12
                                    var t1473 uint32 = t1472 << t1473_rhs
                                    var t1474 uint32 = t1471 | t1473
                                    var t1475_rhs uint32 = 63
                                    var t1475 uint32 = third__266 & t1475_rhs
                                    var t1476_rhs int = 6
                                    var t1476 uint32 = t1475 << t1476_rhs
                                    var t1477 uint32 = t1474 | t1476
                                    var t1478_rhs uint32 = 63
                                    var t1478 uint32 = fourth__267 & t1478_rhs
                                    var t1479 uint32 = t1477 | t1478
                                    var t1480 Tuple3_4bool_4char_3int = utf8_valid_decode(t1479, 4)
                                    return t1480
                                }
                            }
                        } else {
                            var t1501 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1501
                        }
                    }
                }
            }
        }
    }
}

func ascii_is_lowercase(value__395 uint8) bool {
    var t1511 bool = value__395 >= 97
    if t1511 {
        var t1512 bool = value__395 <= 122
        return t1512
    } else {
        return false
    }
}

func decimal_string(value__208 uint64) string {
    var t1535 bool = value__208 == 0
    if t1535 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1528:
        for {
            var t1529 bool = remaining__210 > 0
            if t1529 {
                var t1530_rhs uint64 = 10
                var t1530 uint64 = remaining__210 % t1530_rhs
                var t1531 uint8 = uint8(uint64(t1530))
                var t1532 uint8 = t1531 + 48
                vec_push__Vec_5uint8(reversed__209, t1532)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1533 uint64 = compound_old353 / compound_value354
                remaining__210 = t1533
                continue
            } else {
                break Loop_loop1528
            }
        }
        var t1517 int
        var inline2024 int = vec_len__Vec_5uint8(reversed__209)
        t1517 = inline2024
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1517)
        var offset__212 int = 0
        Loop_loop1519:
        for {
            var t1520 int
            var inline2022 int = vec_len__Vec_5uint8(reversed__209)
            t1520 = inline2022
            var t1521 bool = offset__212 < t1520
            if t1521 {
                var t1522 int
                var inline2020 int = vec_len__Vec_5uint8(reversed__209)
                t1522 = inline2020
                var t1523 int = t1522 - offset__212
                var t1524 int = t1523 - 1
                var t1525 uint8 = vec_get__Vec_5uint8(reversed__209, t1524)
                vec_push__Vec_5uint8(bytes__211, t1525)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1526 int = compound_old358 + compound_value359
                offset__212 = t1526
                continue
            } else {
                break Loop_loop1519
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1538 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1538
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2092 rune
    var inline2028 bool = utf8_valid_scalar(value__253)
    if inline2028 {
        var inline2029 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2030 rune = inline2029._1
        commute_field2092 = inline2030
        var t1544 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2092,
            _2: width__254,
        }
        return t1544
    } else {
        var inline2026 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2026
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1549 bool = value__256 < 128
    if t1549 {
        return true
    } else {
        var t1550 bool = value__256 > 191
        return t1550
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1555 bool
    var inline2034 bool = value__283 <= 1114111
    if inline2034 {
        var inline2035 bool = value__283 >= 55296
        var inline2037 bool
        if inline2035 {
            var inline2039 bool = value__283 <= 57343
            inline2037 = inline2039
        } else {
            inline2037 = false
        }
        var inline2038 bool = !inline2037
        t1555 = inline2038
    } else {
        t1555 = false
    }
    if t1555 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1556 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1556
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1561 bool = value__257 <= 1114111
    if t1561 {
        var t1565 bool = value__257 >= 55296
        var jp1563 bool
        if t1565 {
            var t1566 bool = value__257 <= 57343
            jp1563 = t1566
        } else {
            jp1563 = false
        }
        var t1564 bool = !jp1563
        return t1564
    } else {
        return false
    }
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env813 closure_env_goml_builtin_range_0) Option__isize {
    var current__758 *ref_int_x = env813.current_0
    var end__757 int = env813.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t1577 bool = value__759 < end__757
    if t1577 {
        var t1578 int = value__759 + 1
        ref_set__Ref_3int(current__758, t1578)
        var t1579 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t1579
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h6cc414bec6b61ab808929b69f0900e8f_hars__1_i_apply(env814 closure_env_inherent_string_string_chars_1) Option__char {
    var self__305 string = env814.self_0
    var index__306 *ref_int_x = env814.index_1
    var t1582 int = ref_get__Ref_3int(index__306)
    var commute_field2095 Tuple2_4char_3int
    var inline2041 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__305, t1582)
    var inline2042 bool = inline2041._0
    var inline2043 rune = inline2041._1
    var inline2044 int = inline2041._2
    if inline2042 {
        var inline2048 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline2043,
            _1: inline2044,
        }
        commute_field2095 = inline2048
        var x417 rune = commute_field2095._0
        var x418 int = commute_field2095._1
        var compound_old419 int = ref_get__Ref_3int(index__306)
        var t1585 int = compound_old419 + x418
        ref_set__Ref_3int(index__306, t1585)
        var t1587 Option__char = Option__char{
            _tag: 1,
            _v1_0: x417,
        }
        return t1587
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_hce113b295882d0c03e72873edb11c646_ices__2_i_apply(env815 closure_env_inherent_string_string_char_indices_2) _goml_m_Option_____o_isize_c_char_q_ {
    var index__311 *ref_int_x = env815.index_0
    var self__310 string = env815.self_1
    var current__312 int = ref_get__Ref_3int(index__311)
    var commute_field2098 Tuple2_4char_3int
    var inline2051 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__310, current__312)
    var inline2052 bool = inline2051._0
    var inline2053 rune = inline2051._1
    var inline2054 int = inline2051._2
    if inline2052 {
        var inline2058 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline2053,
            _1: inline2054,
        }
        commute_field2098 = inline2058
        var x425 rune = commute_field2098._0
        var x426 int = commute_field2098._1
        var t1592 int = current__312 + x426
        ref_set__Ref_3int(index__311, t1592)
        var t1593 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__312,
            _1: x425,
        }
        var t1594 _goml_m_Option_____o_isize_c_char_q_ = _goml_m_Option_____o_isize_c_char_q_{
            _tag: 1,
            _v1_0: t1593,
        }
        return t1594
    } else {
        return _goml_m_Option_____o_isize_c_char_q_{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
