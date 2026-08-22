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
    var inline928 string = _goml_runtime_core_bool_to_string(t815)
    t816 = inline928
    var inline925 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t816)
    _goml_runtime_core_string_println(inline925)
    var t817 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(values__4, 9)
    var t818 string
    var inline923 string = _goml_runtime_core_bool_to_string(t817)
    t818 = inline923
    var inline920 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t818)
    _goml_runtime_core_string_println(inline920)
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
    var inline918 string = _goml_runtime_core_bool_to_string(t820)
    t821 = inline918
    var inline915 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline915)
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
    var inline913 string = _goml_runtime_core_bool_to_string(t825)
    t826 = inline913
    var inline910 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t826)
    _goml_runtime_core_string_println(inline910)
    var t827 bool
    var inline907 int = 3
    var inline908 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(values__4, inline907)
    t827 = inline908
    var t828 string
    var inline905 string = _goml_runtime_core_bool_to_string(t827)
    t828 = inline905
    var inline902 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t828)
    _goml_runtime_core_string_println(inline902)
    var t829 [0]string = [0]string{}
    var empty__7 *_goml_vec_string = func(values [0]string) *_goml_vec_string {
        return &_goml_vec_string{
            items: values[0:len(values)],
        }
    }(t829)
    var t830 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(empty__7, "x")
    var t831 string
    var inline900 string = _goml_runtime_core_bool_to_string(t830)
    t831 = inline900
    var inline897 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t831)
    _goml_runtime_core_string_println(inline897)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(self__438 int, other__439 int) bool {
    var t835 bool = self__438 == other__439
    return t835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__isize(self__637 *_goml_vec_int, expected__638 int) bool {
    var index__639 int = 0
    Loop_loop842:
    for {
        var t843 int
        var inline933 int = vec_len__Vec_3int(self__637)
        t843 = inline933
        var t844 bool = index__639 < t843
        if t844 {
            var t848 int = vec_get__Vec_3int(self__637, index__639)
            var t849 bool
            var inline931 bool = t848 == expected__638
            t849 = inline931
            if t849 {
                return true
            } else {
                var compound_old744 int = index__639
                var compound_value745 int = 1
                var t846 int = compound_old744 + compound_value745
                index__639 = t846
                continue
            }
        } else {
            break Loop_loop842
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__string(self__637 *_goml_vec_string, expected__638 string) bool {
    var index__639 int = 0
    Loop_loop856:
    for {
        var t857 int
        var inline937 int = vec_len__Vec_6string(self__637)
        t857 = inline937
        var t858 bool = index__639 < t857
        if t858 {
            var t862 string = vec_get__Vec_6string(self__637, index__639)
            var t863 bool
            var inline935 bool = t862 == expected__638
            t863 = inline935
            if t863 {
                return true
            } else {
                var compound_old744 int = index__639
                var compound_value745 int = 1
                var t860 int = compound_old744 + compound_value745
                index__639 = t860
                continue
            }
        } else {
            break Loop_loop856
        }
    }
    return false
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_contains____T__Pair(self__637 *_goml_vec_Pair, expected__638 Pair) bool {
    var index__639 int = 0
    Loop_loop867:
    for {
        var t868 int
        var inline948 int = vec_len__Vec_4Pair(self__637)
        t868 = inline948
        var t869 bool = index__639 < t868
        if t869 {
            var t873 Pair = vec_get__Vec_4Pair(self__637, index__639)
            var t874 bool
            var inline940 bool
            var inline944 int = t873.left
            var inline945 int = expected__638.left
            var inline946 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline944, inline945)
            inline940 = inline946
            if inline940 {
                var inline941 int = t873.right
                var inline942 int = expected__638.right
                var inline943 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline941, inline942)
                t874 = inline943
                if t874 {
                    return true
                } else {
                    var compound_old744 int = index__639
                    var compound_value745 int = 1
                    var t871 int = compound_old744 + compound_value745
                    index__639 = t871
                    continue
                }
            } else {
                t874 = false
                if t874 {
                    return true
                } else {
                    var compound_old744 int = index__639
                    var compound_value745 int = 1
                    var t871 int = compound_old744 + compound_value745
                    index__639 = t871
                    continue
                }
            }
        } else {
            break Loop_loop867
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
