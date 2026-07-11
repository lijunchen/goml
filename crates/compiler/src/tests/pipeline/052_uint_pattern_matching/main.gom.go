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
    var retv8 bool
    var jp10 bool
    switch value__0 {
    case 0:
        jp10 = true
    case 200:
        jp10 = true
    default:
        jp10 = false
    }
    retv8 = jp10
    return retv8
}

func is_flag16(value__1 uint16) bool {
    var retv12 bool
    var jp14 bool
    switch value__1 {
    case 1024:
        jp14 = true
    case 65000:
        jp14 = true
    default:
        jp14 = false
    }
    retv12 = jp14
    return retv12
}

func is_flag32(value__2 uint32) bool {
    var retv16 bool
    var jp18 bool
    switch value__2 {
    case 4000000000:
        jp18 = true
    case 1234567890:
        jp18 = true
    default:
        jp18 = false
    }
    retv16 = jp18
    return retv16
}

func is_flag64(value__3 uint64) bool {
    var retv20 bool
    var jp22 bool
    switch value__3 {
    case 900000000:
        jp22 = true
    case 600000000:
        jp22 = true
    default:
        jp22 = false
    }
    retv20 = jp22
    return retv20
}

func match_struct(counter__4 Counter) bool {
    var retv24 bool
    var x4 uint32 = counter__4.start
    var x5 uint64 = counter__4.end
    var jp26 bool
    switch x5 {
    case 900000000:
        var jp28 bool
        switch x4 {
        case 4000000000:
            jp28 = true
        default:
            jp28 = false
        }
        jp26 = jp28
    case 600000000:
        jp26 = true
    default:
        jp26 = false
    }
    retv24 = jp26
    return retv24
}

func report(label__5 string, value__6 bool) string {
    var retv30 string
    var t31 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t32 string = label__5 + t31
    retv30 = t32
    return retv30
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
    var t34 bool = is_flag8(200)
    var t35 string = report("u8_hit=", t34)
    var t36 bool = is_flag8(15)
    var t37 string = report(",u8_miss=", t36)
    var t38 string = t35 + t37
    var t39 bool = is_flag16(65000)
    var t40 string = report(",u16_hit=", t39)
    var t41 string = t38 + t40
    var t42 bool = is_flag16(42)
    var t43 string = report(",u16_miss=", t42)
    var t44 string = t41 + t43
    var t45 bool = is_flag32(1234567890)
    var t46 string = report(",u32_hit=", t45)
    var t47 string = t44 + t46
    var t48 bool = is_flag32(99)
    var t49 string = report(",u32_miss=", t48)
    var t50 string = t47 + t49
    var t51 bool = is_flag64(900000000)
    var t52 string = report(",u64_hit=", t51)
    var t53 string = t50 + t52
    var t54 bool = is_flag64(700000000)
    var t55 string = report(",u64_miss=", t54)
    var t56 string = t53 + t55
    var t57 bool = match_struct(counter__7)
    var t58 string = report(",struct_first=", t57)
    var t59 string = t56 + t58
    var t60 bool = match_struct(alt_counter__8)
    var t61 string = report(",struct_second=", t60)
    var message__9 string = t59 + t61
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv63 string
    var t64 string = _goml_runtime_core_bool_to_string(self__8)
    retv63 = t64
    return retv63
}

func println__T_string(value__1 string) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t66)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv69 string
    retv69 = self__9
    return retv69
}

func main() {
    main0()
}
