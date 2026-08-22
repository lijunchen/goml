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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
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

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var value__0 string = "a你好z"
    var t808 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "a你")
    var inline992 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t808)
    _goml_runtime_core_string_println(inline992)
    var t809 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "你")
    var inline989 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t809)
    _goml_runtime_core_string_println(inline989)
    var t810 bool = _goml_m_inherent_i_string_i_string_i_starts__with(value__0, "")
    var inline986 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t810)
    _goml_runtime_core_string_println(inline986)
    var t811 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "好z")
    var inline983 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t811)
    _goml_runtime_core_string_println(inline983)
    var t812 bool = _goml_m_inherent_i_string_i_string_i_ends__with(value__0, "你好")
    var inline980 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t812)
    _goml_runtime_core_string_println(inline980)
    var t813 bool
    var inline968 string = ""
    var inline969 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline968)
    var inline970 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
    var inline971 bool = inline969 > inline970
    if inline971 {
        t813 = false
    } else {
        var inline972 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
        var inline973 int = _goml_m_inherent_i_string_i_string_i_byte__len(inline968)
        var inline974 int = inline972 - inline973
        var inline975 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__0, inline974)
        if inline975 {
            var inline976 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__0)
            var inline977 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__0, inline974, inline976)
            var inline978 bool = inline977 == inline968
            t813 = inline978
        } else {
            t813 = false
        }
    }
    var inline965 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t813)
    _goml_runtime_core_string_println(inline965)
    var t814 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你好")
    var inline962 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t814)
    _goml_runtime_core_string_println(inline962)
    var t815 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "好z")
    var inline959 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t815)
    _goml_runtime_core_string_println(inline959)
    var t816 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "你z")
    var inline956 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t816)
    _goml_runtime_core_string_println(inline956)
    var t817 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "")
    var inline953 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t817)
    _goml_runtime_core_string_println(inline953)
    var t818 bool = _goml_m_inherent_i_string_i_string_i_contains(value__0, "a你好z!")
    var inline950 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t818)
    _goml_runtime_core_string_println(inline950)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_starts__with(self__709 string, prefix__710 string) bool {
    var t832 int
    var inline1008 int = _goml_runtime_core_string_len(prefix__710)
    t832 = inline1008
    var t833 int
    var inline1006 int = _goml_runtime_core_string_len(self__709)
    t833 = inline1006
    var t834 bool = t832 <= t833
    var jp828 bool
    if t834 {
        var t835 int
        var inline999 int = _goml_runtime_core_string_len(prefix__710)
        t835 = inline999
        var inline997 bool = string_is_char_boundary(self__709, t835)
        jp828 = inline997
    } else {
        jp828 = false
    }
    if jp828 {
        var t829 int
        var inline1004 int = _goml_runtime_core_string_len(prefix__710)
        t829 = inline1004
        var t830 string
        var inline1001 int = 0
        var inline1002 string = string_byte_slice(self__709, inline1001, t829)
        t830 = inline1002
        var t831 bool = t830 == prefix__710
        return t831
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_ends__with(self__711 string, suffix__712 string) bool {
    var t841 int
    var inline1022 int = _goml_runtime_core_string_len(suffix__712)
    t841 = inline1022
    var t842 int
    var inline1020 int = _goml_runtime_core_string_len(self__711)
    t842 = inline1020
    var t843 bool = t841 > t842
    if t843 {
        return false
    } else {
        var t844 int
        var inline1018 int = _goml_runtime_core_string_len(self__711)
        t844 = inline1018
        var t845 int
        var inline1016 int = _goml_runtime_core_string_len(suffix__712)
        t845 = inline1016
        var start__713 int = t844 - t845
        var t848 bool
        var inline1014 bool = string_is_char_boundary(self__711, start__713)
        t848 = inline1014
        if t848 {
            var t849 int
            var inline1012 int = _goml_runtime_core_string_len(self__711)
            t849 = inline1012
            var t850 string
            var inline1010 string = string_byte_slice(self__711, start__713, t849)
            t850 = inline1010
            var t851 bool = t850 == suffix__712
            return t851
        } else {
            return false
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_contains(self__714 string, expected__715 string) bool {
    var t856 int
    var inline1050 int = _goml_runtime_core_string_len(expected__715)
    t856 = inline1050
    var t857 bool = t856 == 0
    if t857 {
        return true
    } else {
        var t860 int
        var inline1048 int = _goml_runtime_core_string_len(expected__715)
        t860 = inline1048
        var t861 int
        var inline1046 int = _goml_runtime_core_string_len(self__714)
        t861 = inline1046
        var t862 bool = t860 > t861
        if t862 {
            return false
        } else {
            var t863 int
            var inline1044 int = _goml_runtime_core_string_len(self__714)
            t863 = inline1044
            var t864 int
            var inline1042 int = _goml_runtime_core_string_len(expected__715)
            t864 = inline1042
            var t865 int = t863 - t864
            var t866 int = t865 + 1
            var t867 FnIterator__isize
            var inline1036 int = 0
            var inline1037 *ref_int_x = ref__Ref_3int(inline1036)
            var inline1038 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
                current_0: inline1037,
                end_1: t866,
            }
            var inline1039 func() Option__isize = func() Option__isize {
                return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(inline1038)
            }
            var inline1040 FnIterator__isize = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(inline1039)
            t867 = inline1040
            var for_iter770 FnIterator__isize
            for_iter770 = t867
            Loop_loop869:
            for {
                var for_next771 Option__isize
                var inline1032 func() Option__isize = for_iter770.next_fn
                var inline1033 Option__isize = inline1032()
                for_next771 = inline1033
                switch for_next771._tag {
                case 0:
                    break Loop_loop869
                case 1:
                    var x772 int = for_next771._v1_0
                    var t871 int
                    var inline1030 int = _goml_runtime_core_string_len(expected__715)
                    t871 = inline1030
                    var end__717 int = x772 + t871
                    var t879 bool
                    var inline1028 bool = string_is_char_boundary(self__714, x772)
                    t879 = inline1028
                    var jp876 bool
                    if t879 {
                        var inline1024 bool = string_is_char_boundary(self__714, end__717)
                        jp876 = inline1024
                    } else {
                        jp876 = false
                    }
                    var jp874 bool
                    if jp876 {
                        var t877 string
                        var inline1026 string = string_byte_slice(self__714, x772, end__717)
                        t877 = inline1026
                        var t878 bool = t877 == expected__715
                        jp874 = t878
                    } else {
                        jp874 = false
                    }
                    if jp874 {
                        return true
                    } else {
                        continue
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
            return false
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t883 string = _goml_runtime_core_bool_to_string(self__401)
    return t883
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t886 int = _goml_runtime_core_string_len(self__289)
    return t886
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__297 string, index__298 int) bool {
    var t889 bool = string_is_char_boundary(self__297, index__298)
    return t889
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline1052 bool = string_is_char_boundary(self__294, start__295)
    var inline1054 bool
    if inline1052 {
        var inline1057 bool = string_is_char_boundary(self__294, end__296)
        inline1054 = inline1057
    } else {
        inline1054 = false
    }
    if inline1054 {
        var inline1055 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline1055
    } else {
        var inline1056 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline1056
    }
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t917 bool = index__269 < 0
    var jp909 bool
    if t917 {
        jp909 = true
    } else {
        var t918 int
        var inline1061 int = _goml_runtime_core_string_len(value__268)
        t918 = inline1061
        var t919 bool = index__269 > t918
        jp909 = t919
    }
    if jp909 {
        return false
    } else {
        var t912 int
        var inline1065 int = _goml_runtime_core_string_len(value__268)
        t912 = inline1065
        var t913 bool = index__269 == t912
        if t913 {
            return true
        } else {
            var t914 uint8
            var inline1063 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t914 = inline1063
            var t915_rhs uint8 = 192
            var t915 uint8 = t914 & t915_rhs
            var t916 bool = t915 != 128
            return t916
        }
    }
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t928 bool = string_is_char_boundary(value__274, start__275)
    var jp925 bool
    if t928 {
        var t929 bool = string_is_char_boundary(value__274, end__276)
        jp925 = t929
    } else {
        jp925 = false
    }
    if jp925 {
        var t926 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t926
    } else {
        var t927 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t927
    }
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__isize(next_fn__507 func() Option__isize) FnIterator__isize {
    var t932 FnIterator__isize = FnIterator__isize{
        next_fn: next_fn__507,
    }
    return t932
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env806 closure_env_goml_builtin_range_0) Option__isize {
    var current__758 *ref_int_x = env806.current_0
    var end__757 int = env806.end_1
    var value__759 int = ref_get__Ref_3int(current__758)
    var t946 bool = value__759 < end__757
    if t946 {
        var t947 int = value__759 + 1
        ref_set__Ref_3int(current__758, t947)
        var t948 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: value__759,
        }
        return t948
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
