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

type _goml_vec_int struct {
    items []int
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type _goml_vec_Pair struct {
    items []Pair
}

func vec_get__Vec_4Pair(vec *_goml_vec_Pair, index int) Pair {
    return vec.items[index]
}

func vec_len__Vec_4Pair(vec *_goml_vec_Pair) int {
    return int(len(vec.items))
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

type Pair struct {
    left int
    right int
}

type Ordering int32

func main0() struct{} {
    var t814 [3]int = [3]int{1, 2, 3}
    var values__4 *_goml_vec_int = func(values [3]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [3]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t814)
    var t815 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(values__4, 2)
    var t816 string
    var inline932 string = _goml_runtime_core_bool_to_string(t815)
    t816 = inline932
    var inline929 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline929)
    var t817 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(values__4, 9)
    var t818 string
    var inline927 string = _goml_runtime_core_bool_to_string(t817)
    t818 = inline927
    var inline924 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
    _goml_runtime_core_string_println(inline924)
    var t819 [2]string = [2]string{"alpha", "beta"}
    var names__5 *_goml_vec_string = func(values [2]string) *_goml_vec_string {
        var storage struct {
            vector _goml_vec_string
            values [2]string
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t819)
    var t820 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(names__5, "beta")
    var t821 string
    var inline922 string = _goml_runtime_core_bool_to_string(t820)
    t821 = inline922
    var inline919 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline919)
    var t822 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t823 [1]Pair = [1]Pair{t822}
    var pairs__6 *_goml_vec_Pair = func(values [1]Pair) *_goml_vec_Pair {
        var storage struct {
            vector _goml_vec_Pair
            values [1]Pair
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t823)
    var t824 Pair = Pair{
        left: 1,
        right: 2,
    }
    var t825 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(pairs__6, t824)
    var t826 string
    var inline917 string = _goml_runtime_core_bool_to_string(t825)
    t826 = inline917
    var inline914 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline914)
    var t827 bool
    var inline911 int = 3
    var inline912 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(values__4, inline911)
    t827 = inline912
    var t828 string
    var inline909 string = _goml_runtime_core_bool_to_string(t827)
    t828 = inline909
    var inline906 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t828)
    _goml_runtime_core_string_println(inline906)
    var empty__7 *_goml_vec_string
    var inline904 *_goml_vec_string = vec_new__Vec_6string()
    empty__7 = inline904
    var t829 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(empty__7, "x")
    var t830 string
    var inline902 string = _goml_runtime_core_bool_to_string(t829)
    t830 = inline902
    var inline899 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t830)
    _goml_runtime_core_string_println(inline899)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(self__438 int, other__439 int) bool {
    var t834 bool = self__438 == other__439
    return t834
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(self__637 *_goml_vec_int, expected__638 int) bool {
    var index__639 int = 0
    Loop_loop841:
    for {
        var t842 int
        var inline937 int = vec_len__Vec_3int(self__637)
        t842 = inline937
        var t843 bool = index__639 < t842
        if t843 {
            var t847 int = vec_get__Vec_3int(self__637, index__639)
            var t848 bool
            var inline935 bool = t847 == expected__638
            t848 = inline935
            if t848 {
                return true
            } else {
                var compound_old744 int = index__639
                var compound_value745 int = 1
                var t845 int = compound_old744 + compound_value745
                index__639 = t845
                continue
            }
        } else {
            break Loop_loop841
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__637 *_goml_vec_string, expected__638 string) bool {
    var index__639 int = 0
    Loop_loop855:
    for {
        var t856 int
        var inline941 int = vec_len__Vec_6string(self__637)
        t856 = inline941
        var t857 bool = index__639 < t856
        if t857 {
            var t861 string = vec_get__Vec_6string(self__637, index__639)
            var t862 bool
            var inline939 bool = t861 == expected__638
            t862 = inline939
            if t862 {
                return true
            } else {
                var compound_old744 int = index__639
                var compound_value745 int = 1
                var t859 int = compound_old744 + compound_value745
                index__639 = t859
                continue
            }
        } else {
            break Loop_loop855
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__637 *_goml_vec_Pair, expected__638 Pair) bool {
    var index__639 int = 0
    Loop_loop866:
    for {
        var t867 int
        var inline952 int = vec_len__Vec_4Pair(self__637)
        t867 = inline952
        var t868 bool = index__639 < t867
        if t868 {
            var t872 Pair = vec_get__Vec_4Pair(self__637, index__639)
            var t873 bool
            var inline944 bool
            var inline948 int = t872.left
            var inline949 int = expected__638.left
            var inline950 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline948, inline949)
            inline944 = inline950
            if inline944 {
                var inline945 int = t872.right
                var inline946 int = expected__638.right
                var inline947 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline945, inline946)
                t873 = inline947
                if t873 {
                    return true
                } else {
                    var compound_old744 int = index__639
                    var compound_value745 int = 1
                    var t870 int = compound_old744 + compound_value745
                    index__639 = t870
                    continue
                }
            } else {
                t873 = false
                if t873 {
                    return true
                } else {
                    var compound_old744 int = index__639
                    var compound_value745 int = 1
                    var t870 int = compound_old744 + compound_value745
                    index__639 = t870
                    continue
                }
            }
        } else {
            break Loop_loop866
        }
    }
    return false
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
