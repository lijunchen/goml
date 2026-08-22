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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
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

func test_int_comparisons() struct{} {
    var a__0 int32 = 10
    var b__1 int32 = 20
    var c__2 int32 = 10
    var less__3 bool = a__0 < b__1
    var t822 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__3)
    var t823 string = "10 < 20: " + t822
    var inline918 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t823)
    _goml_runtime_core_string_println(inline918)
    var greater__4 bool = b__1 > a__0
    var t824 string
    var inline916 string = _goml_runtime_core_bool_to_string(greater__4)
    t824 = inline916
    var t825 string = "20 > 10: " + t824
    var inline913 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t825)
    _goml_runtime_core_string_println(inline913)
    var less_eq1__5 bool = a__0 <= b__1
    var t826 string
    var inline911 string = _goml_runtime_core_bool_to_string(less_eq1__5)
    t826 = inline911
    var t827 string = "10 <= 20: " + t826
    var inline908 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t827)
    _goml_runtime_core_string_println(inline908)
    var less_eq2__6 bool = a__0 <= c__2
    var t828 string
    var inline906 string = _goml_runtime_core_bool_to_string(less_eq2__6)
    t828 = inline906
    var t829 string = "10 <= 10: " + t828
    var inline903 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t829)
    _goml_runtime_core_string_println(inline903)
    var greater_eq1__7 bool = b__1 >= a__0
    var t830 string
    var inline901 string = _goml_runtime_core_bool_to_string(greater_eq1__7)
    t830 = inline901
    var t831 string = "20 >= 10: " + t830
    var inline898 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline898)
    var greater_eq2__8 bool = c__2 >= a__0
    var t832 string
    var inline896 string = _goml_runtime_core_bool_to_string(greater_eq2__8)
    t832 = inline896
    var t833 string = "10 >= 10: " + t832
    var inline893 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t833)
    _goml_runtime_core_string_println(inline893)
    var eq1__9 bool = a__0 == c__2
    var t834 string
    var inline891 string = _goml_runtime_core_bool_to_string(eq1__9)
    t834 = inline891
    var t835 string = "10 == 10: " + t834
    var inline888 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t835)
    _goml_runtime_core_string_println(inline888)
    var eq2__10 bool = a__0 == b__1
    var t836 string
    var inline886 string = _goml_runtime_core_bool_to_string(eq2__10)
    t836 = inline886
    var t837 string = "10 == 20: " + t836
    var inline883 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t837)
    _goml_runtime_core_string_println(inline883)
    var neq1__11 bool = a__0 != b__1
    var t838 string
    var inline881 string = _goml_runtime_core_bool_to_string(neq1__11)
    t838 = inline881
    var t839 string = "10 != 20: " + t838
    var inline878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t839)
    _goml_runtime_core_string_println(inline878)
    var neq2__12 bool = a__0 != c__2
    var t840 string
    var inline876 string = _goml_runtime_core_bool_to_string(neq2__12)
    t840 = inline876
    var t841 string = "10 != 10: " + t840
    var inline873 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t841)
    _goml_runtime_core_string_println(inline873)
    return struct{}{}
}

func test_float_comparisons() struct{} {
    var x__13 float64 = 3.14
    var y__14 float64 = 2.71
    var z__15 float64 = 3.14
    var less__16 bool = y__14 < x__13
    var t843 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__16)
    var t844 string = "2.71 < 3.14: " + t843
    var inline966 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t844)
    _goml_runtime_core_string_println(inline966)
    var greater__17 bool = x__13 > y__14
    var t845 string
    var inline964 string = _goml_runtime_core_bool_to_string(greater__17)
    t845 = inline964
    var t846 string = "3.14 > 2.71: " + t845
    var inline961 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t846)
    _goml_runtime_core_string_println(inline961)
    var less_eq1__18 bool = y__14 <= x__13
    var t847 string
    var inline959 string = _goml_runtime_core_bool_to_string(less_eq1__18)
    t847 = inline959
    var t848 string = "2.71 <= 3.14: " + t847
    var inline956 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t848)
    _goml_runtime_core_string_println(inline956)
    var less_eq2__19 bool = x__13 <= z__15
    var t849 string
    var inline954 string = _goml_runtime_core_bool_to_string(less_eq2__19)
    t849 = inline954
    var t850 string = "3.14 <= 3.14: " + t849
    var inline951 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t850)
    _goml_runtime_core_string_println(inline951)
    var greater_eq1__20 bool = x__13 >= y__14
    var t851 string
    var inline949 string = _goml_runtime_core_bool_to_string(greater_eq1__20)
    t851 = inline949
    var t852 string = "3.14 >= 2.71: " + t851
    var inline946 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t852)
    _goml_runtime_core_string_println(inline946)
    var greater_eq2__21 bool = z__15 >= x__13
    var t853 string
    var inline944 string = _goml_runtime_core_bool_to_string(greater_eq2__21)
    t853 = inline944
    var t854 string = "3.14 >= 3.14: " + t853
    var inline941 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t854)
    _goml_runtime_core_string_println(inline941)
    var eq1__22 bool = x__13 == z__15
    var t855 string
    var inline939 string = _goml_runtime_core_bool_to_string(eq1__22)
    t855 = inline939
    var t856 string = "3.14 == 3.14: " + t855
    var inline936 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t856)
    _goml_runtime_core_string_println(inline936)
    var eq2__23 bool = x__13 == y__14
    var t857 string
    var inline934 string = _goml_runtime_core_bool_to_string(eq2__23)
    t857 = inline934
    var t858 string = "3.14 == 2.71: " + t857
    var inline931 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t858)
    _goml_runtime_core_string_println(inline931)
    var neq1__24 bool = x__13 != y__14
    var t859 string
    var inline929 string = _goml_runtime_core_bool_to_string(neq1__24)
    t859 = inline929
    var t860 string = "3.14 != 2.71: " + t859
    var inline926 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t860)
    _goml_runtime_core_string_println(inline926)
    var neq2__25 bool = x__13 != z__15
    var t861 string
    var inline924 string = _goml_runtime_core_bool_to_string(neq2__25)
    t861 = inline924
    var t862 string = "3.14 != 3.14: " + t861
    var inline921 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t862)
    _goml_runtime_core_string_println(inline921)
    return struct{}{}
}

func main0() struct{} {
    var inline977 string = "=== Integer Comparisons ==="
    var inline978 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline977)
    _goml_runtime_core_string_println(inline978)
    test_int_comparisons()
    var inline973 string = ""
    var inline974 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline973)
    _goml_runtime_core_string_println(inline974)
    var inline969 string = "=== Float Comparisons ==="
    var inline970 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline969)
    _goml_runtime_core_string_println(inline970)
    test_float_comparisons()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t869 string = _goml_runtime_core_bool_to_string(self__401)
    return t869
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
