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
    var retv11 bool
    var jp13 bool
    switch value__0 {
    case 0:
        jp13 = true
    case 200:
        jp13 = true
    default:
        jp13 = false
    }
    retv11 = jp13
    return retv11
}

func is_flag16(value__1 uint16) bool {
    var retv15 bool
    var jp17 bool
    switch value__1 {
    case 1024:
        jp17 = true
    case 65000:
        jp17 = true
    default:
        jp17 = false
    }
    retv15 = jp17
    return retv15
}

func is_flag32(value__2 uint32) bool {
    var retv19 bool
    var jp21 bool
    switch value__2 {
    case 4000000000:
        jp21 = true
    case 1234567890:
        jp21 = true
    default:
        jp21 = false
    }
    retv19 = jp21
    return retv19
}

func is_flag64(value__3 uint64) bool {
    var retv23 bool
    var jp25 bool
    switch value__3 {
    case 900000000:
        jp25 = true
    case 600000000:
        jp25 = true
    default:
        jp25 = false
    }
    retv23 = jp25
    return retv23
}

func match_struct(counter__4 Counter) bool {
    var retv27 bool
    var x7 uint32 = counter__4.start
    var x8 uint64 = counter__4.end
    var jp29 bool
    switch x8 {
    case 900000000:
        var jp31 bool
        switch x7 {
        case 4000000000:
            jp31 = true
        default:
            jp31 = false
        }
        jp29 = jp31
    case 600000000:
        jp29 = true
    default:
        jp29 = false
    }
    retv27 = jp29
    return retv27
}

func report(label__5 string, value__6 bool) string {
    var retv33 string
    var t34 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__6)
    var t35 string = label__5 + t34
    retv33 = t35
    return retv33
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
    var t37 bool = is_flag8(200)
    var t38 string = report("u8_hit=", t37)
    var t39 bool = is_flag8(15)
    var t40 string = report(",u8_miss=", t39)
    var t41 string = t38 + t40
    var t42 bool = is_flag16(65000)
    var t43 string = report(",u16_hit=", t42)
    var t44 string = t41 + t43
    var t45 bool = is_flag16(42)
    var t46 string = report(",u16_miss=", t45)
    var t47 string = t44 + t46
    var t48 bool = is_flag32(1234567890)
    var t49 string = report(",u32_hit=", t48)
    var t50 string = t47 + t49
    var t51 bool = is_flag32(99)
    var t52 string = report(",u32_miss=", t51)
    var t53 string = t50 + t52
    var t54 bool = is_flag64(900000000)
    var t55 string = report(",u64_hit=", t54)
    var t56 string = t53 + t55
    var t57 bool = is_flag64(700000000)
    var t58 string = report(",u64_miss=", t57)
    var t59 string = t56 + t58
    var t60 bool = match_struct(counter__7)
    var t61 string = report(",struct_first=", t60)
    var t62 string = t59 + t61
    var t63 bool = match_struct(alt_counter__8)
    var t64 string = report(",struct_second=", t63)
    var message__9 string = t62 + t64
    println__T_string(message__9)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv66 string
    var t67 string = _goml_runtime_core_bool_to_string(self__8)
    retv66 = t67
    return retv66
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv72 string
    retv72 = self__9
    return retv72
}

func main() {
    main0()
}
