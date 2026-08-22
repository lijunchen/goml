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

type Tuple2_4int8_5int16 struct {
    _0 int8
    _1 int16
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

type PairData struct {
    head int32
    tail int64
}

type Ordering int32

func is_special8(value__0 int8) bool {
    switch value__0 {
    case 5:
        return true
    case 7:
        return true
    default:
        return false
    }
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var x796 int8 = values__4._0
    var x797 int16 = values__4._1
    switch x797 {
    case 2:
        switch x796 {
        case 1:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func match_struct(pair__5 PairData) bool {
    var x798 int32 = pair__5.head
    var x799 int64 = pair__5.tail
    switch x799 {
    case 200:
        switch x798 {
        case 100:
            return true
        default:
            return false
        }
    case 300:
        return true
    default:
        return false
    }
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t834 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t834)
    var t835 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t835)
    var t836 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t836)
    var t837 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t837)
    var t838 bool = is_special8(5)
    var part1__14 string
    var inline904 string = "i8="
    var inline905 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t838)
    var inline906 string = inline904 + inline905
    part1__14 = inline906
    var t839 bool
    var inline902 int16 = 1024
    switch inline902 {
    case 1024:
        t839 = true
    case 2048:
        t839 = true
    default:
        t839 = false
    }
    var part2__15 string
    var inline898 string = ",i16="
    var inline899 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t839)
    var inline900 string = inline898 + inline899
    part2__15 = inline900
    var t840 bool
    var inline896 int32 = 8192
    switch inline896 {
    case 4096:
        t840 = true
    case 8192:
        t840 = true
    default:
        t840 = false
    }
    var part3__16 string
    var inline892 string = ",i32="
    var inline893 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t840)
    var inline894 string = inline892 + inline893
    part3__16 = inline894
    var t841 bool
    var inline890 int64 = 16384
    switch inline890 {
    case 16384:
        t841 = true
    case 32768:
        t841 = true
    default:
        t841 = false
    }
    var part4__17 string
    var inline886 string = ",int64_a="
    var inline887 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t841)
    var inline888 string = inline886 + inline887
    part4__17 = inline888
    var t842 bool
    var inline884 int64 = 32768
    switch inline884 {
    case 16384:
        t842 = true
    case 32768:
        t842 = true
    default:
        t842 = false
    }
    var part5__18 string
    var inline880 string = ",int64_b="
    var inline881 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t842)
    var inline882 string = inline880 + inline881
    part5__18 = inline882
    var part6__19 string
    var inline876 string = ",tuple_hit="
    var inline877 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline878 string = inline876 + inline877
    part6__19 = inline878
    var part7__20 string
    var inline872 string = ",tuple_miss="
    var inline873 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline874 string = inline872 + inline873
    part7__20 = inline874
    var part8__21 string
    var inline868 string = ",struct_first="
    var inline869 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline870 string = inline868 + inline869
    part8__21 = inline870
    var part9__22 string
    var inline864 string = ",struct_second="
    var inline865 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline866 string = inline864 + inline865
    part9__22 = inline866
    var t843 string = part1__14 + part2__15
    var t844 string = t843 + part3__16
    var t845 string = t844 + part4__17
    var t846 string = t845 + part5__18
    var t847 string = t846 + part6__19
    var t848 string = t847 + part7__20
    var t849 string = t848 + part8__21
    var message__23 string = t849 + part9__22
    var inline861 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline861)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t852 string = _goml_runtime_core_bool_to_string(self__401)
    return t852
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
