package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Counter struct {
    start uint32
    end uint64
}

func is_flag8(value__0 uint8) bool {
    var retv26 bool
    var jp28 bool
    switch value__0 {
    case 0:
        jp28 = true
    case 200:
        jp28 = true
    default:
        jp28 = false
    }
    retv26 = jp28
    return retv26
}

func is_flag16(value__1 uint16) bool {
    var retv30 bool
    var jp32 bool
    switch value__1 {
    case 1024:
        jp32 = true
    case 65000:
        jp32 = true
    default:
        jp32 = false
    }
    retv30 = jp32
    return retv30
}

func is_flag32(value__2 uint32) bool {
    var retv34 bool
    var jp36 bool
    switch value__2 {
    case 4000000000:
        jp36 = true
    case 1234567890:
        jp36 = true
    default:
        jp36 = false
    }
    retv34 = jp36
    return retv34
}

func is_flag64(value__3 uint64) bool {
    var retv38 bool
    var jp40 bool
    switch value__3 {
    case 900000000:
        jp40 = true
    case 600000000:
        jp40 = true
    default:
        jp40 = false
    }
    retv38 = jp40
    return retv38
}

func match_struct(counter__4 Counter) bool {
    var retv42 bool
    var x22 uint32 = counter__4.start
    var x23 uint64 = counter__4.end
    var jp44 bool
    switch x23 {
    case 900000000:
        var jp46 bool
        switch x22 {
        case 4000000000:
            jp46 = true
        default:
            jp46 = false
        }
        jp44 = jp46
    case 600000000:
        jp44 = true
    default:
        jp44 = false
    }
    retv42 = jp44
    return retv42
}

func report(label__5 string, value__6 bool) string {
    var retv48 string
    var t49 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t50 string = label__5 + t49
    retv48 = t50
    return retv48
}

func main0() struct{} {
    var counter__7 Counter = Counter{
        start: 4000000000,
        end: 900000000,
    }
    var alt_counter__8 Counter = Counter{
        start: 12,
        end: 600000000,
    }
    var t52 bool = is_flag8(200)
    var t53 string = report("u8_hit=", t52)
    var t54 bool = is_flag8(15)
    var t55 string = report(",u8_miss=", t54)
    var t56 string = t53 + t55
    var t57 bool = is_flag16(65000)
    var t58 string = report(",u16_hit=", t57)
    var t59 string = t56 + t58
    var t60 bool = is_flag16(42)
    var t61 string = report(",u16_miss=", t60)
    var t62 string = t59 + t61
    var t63 bool = is_flag32(1234567890)
    var t64 string = report(",u32_hit=", t63)
    var t65 string = t62 + t64
    var t66 bool = is_flag32(99)
    var t67 string = report(",u32_miss=", t66)
    var t68 string = t65 + t67
    var t69 bool = is_flag64(900000000)
    var t70 string = report(",u64_hit=", t69)
    var t71 string = t68 + t70
    var t72 bool = is_flag64(700000000)
    var t73 string = report(",u64_miss=", t72)
    var t74 string = t71 + t73
    var t75 bool = match_struct(counter__7)
    var t76 string = report(",struct_first=", t75)
    var t77 string = t74 + t76
    var t78 bool = match_struct(alt_counter__8)
    var t79 string = report(",struct_second=", t78)
    var message__9 string = t77 + t79
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv81 string
    var t82 string = _goml_runtime_core_bool_to_string(self__8)
    retv81 = t82
    return retv81
}

func println__T_string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv87 string
    retv87 = self__9
    return retv87
}

func main() {
    main0()
}
