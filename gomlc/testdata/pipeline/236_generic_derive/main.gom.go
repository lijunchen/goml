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

type NoTraits struct {
    value int
}

type Wrapper__NoTraits struct {
    value NoTraits
}

type Generic__NoTraits__NoTraits struct {
    first Wrapper__NoTraits
    second Wrapper__NoTraits
}

type Ordering int32

type GenericChoice__NoTraits__NoTraits struct {
    _tag int32
    _v1_0 Wrapper__NoTraits
}

func main0() struct{} {
    var t814 NoTraits = NoTraits{
        value: 1,
    }
    var wrapped__24 Wrapper__NoTraits = Wrapper__NoTraits{
        value: t814,
    }
    var left__25 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var right__26 Generic__NoTraits__NoTraits = Generic__NoTraits__NoTraits{
        first: wrapped__24,
        second: wrapped__24,
    }
    var t815 string = _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(left__25)
    println__T_string(t815)
    var t816 bool = _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(left__25, right__26)
    println__T_bool(t816)
    var t817 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(left__25)
    var t818 uint64 = _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(right__26)
    var t819 bool = t817 == t818
    var inline951 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t819)
    _goml_runtime_core_string_println(inline951)
    var empty__27 GenericChoice__NoTraits__NoTraits = GenericChoice__NoTraits__NoTraits{
        _tag: 0,
    }
    var value__28 GenericChoice__NoTraits__NoTraits = GenericChoice__NoTraits__NoTraits{
        _tag: 1,
        _v1_0: wrapped__24,
    }
    var t820 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(empty__27)
    var inline948 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline948)
    var t821 string = _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(value__28)
    var inline945 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t821)
    _goml_runtime_core_string_println(inline945)
    var t822 bool = _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(empty__27, value__28)
    var t823 bool = !t822
    var inline942 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t823)
    _goml_runtime_core_string_println(inline942)
    var t824 uint64
    var inline933_source int = 0
    var inline933 uint64 = uint64(int(inline933_source))
    var inline934 uint64 = inline933 + 14695981039346656037
    var inline935 uint64 = inline934 + 2
    var inline936_source int = 0
    var inline936 uint64 = uint64(int(inline936_source))
    var inline937 uint64 = inline936 + 1099511628211
    var inline938 uint64 = inline935 * inline937
    var inline939 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline940 uint64 = inline938 + inline939
    t824 = inline940
    var t825 uint64
    var inline919_source int = 0
    var inline919 uint64 = uint64(int(inline919_source))
    var inline920 uint64 = inline919 + 14695981039346656037
    var inline921 uint64 = inline920 + 2
    var inline922_source int = 0
    var inline922 uint64 = uint64(int(inline922_source))
    var inline923 uint64 = inline922 + 1099511628211
    var inline924 uint64 = inline921 * inline923
    var inline925 uint64 = _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(wrapped__24)
    var inline926 uint64 = inline924 + inline925
    t825 = inline926
    var t826 bool = t824 == t825
    var inline911 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t826)
    _goml_runtime_core_string_println(inline911)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t829 string
    t829 = value__1
    _goml_runtime_core_string_println(t829)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_Generic____NoTraits____NoTraits_i_to__string(self__4 Generic__NoTraits__NoTraits) string {
    var t833 string = "Generic { " + "first: "
    var t834 string
    t834 = "wrapped"
    var t835 string = t833 + t834
    var t836 string = t835 + ", "
    var t837 string = t836 + "second: "
    var t838 string
    t838 = "wrapped"
    var t839 string = t837 + t838
    var t840 string = t839 + " }"
    return t840
}

func println__T_bool(value__1 bool) struct{} {
    var t842 string
    var inline957 string = _goml_runtime_core_bool_to_string(value__1)
    t842 = inline957
    _goml_runtime_core_string_println(t842)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Generic____NoTraits____NoTraits_i_eq(self__7 Generic__NoTraits__NoTraits, other__8 Generic__NoTraits__NoTraits) bool {
    var jp849 bool
    jp849 = true
    if jp849 {
        return true
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Hash_i_Generic____NoTraits____NoTraits_i_hash(self__9 Generic__NoTraits__NoTraits) uint64 {
    var t858_source int = 0
    var t858 uint64 = uint64(int(t858_source))
    var h__10 uint64 = t858 + 14695981039346656037
    var t859_source int = 0
    var t859 uint64 = uint64(int(t859_source))
    var t860 uint64 = t859 + 1099511628211
    var t861 uint64 = h__10 * t860
    var t863 uint64
    t863 = 7
    var h__11 uint64 = t861 + t863
    var t864_source int = 0
    var t864 uint64 = uint64(int(t864_source))
    var t865 uint64 = t864 + 1099511628211
    var t866 uint64 = h__11 * t865
    var t868 uint64
    t868 = 7
    var h__12 uint64 = t866 + t868
    return h__12
}

func _goml_m_trait__impl_i_ToString_hfd40b94e3e10293076a83269859fcdb0_ts_i_to__string(self__13 GenericChoice__NoTraits__NoTraits) string {
    switch self__13._tag {
    case 0:
        return "GenericChoice::Empty"
    case 1:
        var t873 string
        t873 = "wrapped"
        var t874 string = "GenericChoice::Value(" + t873
        var t875 string = t874 + ")"
        return t875
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_GenericChoice____NoTraits____NoTraits_i_eq(self__15 GenericChoice__NoTraits__NoTraits, other__16 GenericChoice__NoTraits__NoTraits) bool {
    switch other__16._tag {
    case 0:
        switch self__15._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        switch self__15._tag {
        case 1:
            return true
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t905 string = _goml_runtime_core_bool_to_string(self__401)
    return t905
}

func _goml_m_trait__impl_i_Hash_i_Wrapper____NoTraits_i_hash(self__3 Wrapper__NoTraits) uint64 {
    return 7
}

func main() {
    main0()
}
