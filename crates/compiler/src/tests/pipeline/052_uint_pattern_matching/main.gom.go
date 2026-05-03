package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Counter struct {
    start uint32
    end uint64
}

func is_flag8(value__0 uint8) bool {
    var retv4 bool
    var jp6 bool
    switch value__0 {
    case 0:
        jp6 = true
    case 200:
        jp6 = true
    default:
        jp6 = false
    }
    retv4 = jp6
    return retv4
}

func is_flag16(value__1 uint16) bool {
    var retv8 bool
    var jp10 bool
    switch value__1 {
    case 1024:
        jp10 = true
    case 65000:
        jp10 = true
    default:
        jp10 = false
    }
    retv8 = jp10
    return retv8
}

func is_flag32(value__2 uint32) bool {
    var retv12 bool
    var jp14 bool
    switch value__2 {
    case 4000000000:
        jp14 = true
    case 1234567890:
        jp14 = true
    default:
        jp14 = false
    }
    retv12 = jp14
    return retv12
}

func is_flag64(value__3 uint64) bool {
    var retv16 bool
    var jp18 bool
    switch value__3 {
    case 900000000:
        jp18 = true
    case 600000000:
        jp18 = true
    default:
        jp18 = false
    }
    retv16 = jp18
    return retv16
}

func match_struct(counter__4 Counter) bool {
    var retv20 bool
    var x0 uint32 = counter__4.start
    var x1 uint64 = counter__4.end
    var jp22 bool
    switch x1 {
    case 900000000:
        var jp24 bool
        switch x0 {
        case 4000000000:
            jp24 = true
        default:
            jp24 = false
        }
        jp22 = jp24
    case 600000000:
        jp22 = true
    default:
        jp22 = false
    }
    retv20 = jp22
    return retv20
}

func report(label__5 string, value__6 bool) string {
    var retv26 string
    var t27 string = bool_to_string(value__6)
    var t28 string = label__5 + t27
    retv26 = t28
    return retv26
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
    var t30 bool = is_flag8(200)
    var t31 string = report("u8_hit=", t30)
    var t32 bool = is_flag8(15)
    var t33 string = report(",u8_miss=", t32)
    var t34 string = t31 + t33
    var t35 bool = is_flag16(65000)
    var t36 string = report(",u16_hit=", t35)
    var t37 string = t34 + t36
    var t38 bool = is_flag16(42)
    var t39 string = report(",u16_miss=", t38)
    var t40 string = t37 + t39
    var t41 bool = is_flag32(1234567890)
    var t42 string = report(",u32_hit=", t41)
    var t43 string = t40 + t42
    var t44 bool = is_flag32(99)
    var t45 string = report(",u32_miss=", t44)
    var t46 string = t43 + t45
    var t47 bool = is_flag64(900000000)
    var t48 string = report(",u64_hit=", t47)
    var t49 string = t46 + t48
    var t50 bool = is_flag64(700000000)
    var t51 string = report(",u64_miss=", t50)
    var t52 string = t49 + t51
    var t53 bool = match_struct(counter__7)
    var t54 string = report(",struct_first=", t53)
    var t55 string = t52 + t54
    var t56 bool = match_struct(alt_counter__8)
    var t57 string = report(",struct_second=", t56)
    var message__9 string = t55 + t57
    println__T_string(message__9)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
