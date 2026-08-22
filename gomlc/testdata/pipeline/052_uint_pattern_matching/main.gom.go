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

type Counter struct {
    start uint32
    end uint64
}

type Ordering int32

func is_flag8(value__0 uint8) bool {
    switch value__0 {
    case 0:
        return true
    case 200:
        return true
    default:
        return false
    }
}

func is_flag16(value__1 uint16) bool {
    switch value__1 {
    case 1024:
        return true
    case 65000:
        return true
    default:
        return false
    }
}

func is_flag32(value__2 uint32) bool {
    switch value__2 {
    case 4000000000:
        return true
    case 1234567890:
        return true
    default:
        return false
    }
}

func report(label__5 string, value__6 bool) string {
    var t823 string
    var inline863 string = _goml_runtime_core_bool_to_string(value__6)
    t823 = inline863
    var t824 string = label__5 + t823
    return t824
}

func main0() struct{} {
    var t826 bool = is_flag8(200)
    var t827 string = report("u8_hit=", t826)
    var t828 bool = is_flag8(15)
    var t829 string = report(",u8_miss=", t828)
    var t830 string = t827 + t829
    var t831 bool = is_flag16(65000)
    var t832 string = report(",u16_hit=", t831)
    var t833 string = t830 + t832
    var t834 bool = is_flag16(42)
    var t835 string = report(",u16_miss=", t834)
    var t836 string = t833 + t835
    var t837 bool = is_flag32(1234567890)
    var t838 string = report(",u32_hit=", t837)
    var t839 string = t836 + t838
    var t840 bool
    var inline898 uint32 = 99
    switch inline898 {
    case 4000000000:
        t840 = true
    case 1234567890:
        t840 = true
    default:
        t840 = false
    }
    var t841 string
    var inline894 string = ",u32_miss="
    var inline895 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t840)
    var inline896 string = inline894 + inline895
    t841 = inline896
    var t842 string = t839 + t841
    var t843 bool
    var inline892 uint64 = 900000000
    switch inline892 {
    case 900000000:
        t843 = true
    case 600000000:
        t843 = true
    default:
        t843 = false
    }
    var t844 string
    var inline888 string = ",u64_hit="
    var inline889 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t843)
    var inline890 string = inline888 + inline889
    t844 = inline890
    var t845 string = t842 + t844
    var t846 bool
    var inline886 uint64 = 700000000
    switch inline886 {
    case 900000000:
        t846 = true
    case 600000000:
        t846 = true
    default:
        t846 = false
    }
    var t847 string
    var inline882 string = ",u64_miss="
    var inline883 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t846)
    var inline884 string = inline882 + inline883
    t847 = inline884
    var t848 string = t845 + t847
    var t849 bool
    var inline879 uint32 = 4000000000
    var inline880 uint64 = 900000000
    switch inline880 {
    case 900000000:
        switch inline879 {
        case 4000000000:
            t849 = true
        default:
            t849 = false
        }
    case 600000000:
        t849 = true
    default:
        t849 = false
    }
    var t850 string
    var inline875 string = ",struct_first="
    var inline876 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t849)
    var inline877 string = inline875 + inline876
    t850 = inline877
    var t851 string = t848 + t850
    var t852 bool
    var inline872 uint32 = 12
    var inline873 uint64 = 600000000
    switch inline873 {
    case 900000000:
        switch inline872 {
        case 4000000000:
            t852 = true
        default:
            t852 = false
        }
    case 600000000:
        t852 = true
    default:
        t852 = false
    }
    var t853 string
    var inline868 string = ",struct_second="
    var inline869 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t852)
    var inline870 string = inline868 + inline869
    t853 = inline870
    var message__9 string = t851 + t853
    var inline865 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__9)
    _goml_runtime_core_string_println(inline865)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t856 string = _goml_runtime_core_bool_to_string(self__401)
    return t856
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
