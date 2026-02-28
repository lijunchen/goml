package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Counter struct {
    start uint32
    end uint64
}

func is_flag8(value__0 uint8) bool {
    var jp4 bool
    switch value__0 {
    case 0:
        jp4 = true
    case 200:
        jp4 = true
    default:
        jp4 = false
    }
    return jp4
}

func is_flag16(value__1 uint16) bool {
    var jp6 bool
    switch value__1 {
    case 1024:
        jp6 = true
    case 65000:
        jp6 = true
    default:
        jp6 = false
    }
    return jp6
}

func is_flag32(value__2 uint32) bool {
    var jp8 bool
    switch value__2 {
    case 4000000000:
        jp8 = true
    case 1234567890:
        jp8 = true
    default:
        jp8 = false
    }
    return jp8
}

func is_flag64(value__3 uint64) bool {
    var jp10 bool
    switch value__3 {
    case 900000000:
        jp10 = true
    case 600000000:
        jp10 = true
    default:
        jp10 = false
    }
    return jp10
}

func match_struct(counter__4 Counter) bool {
    var x0 uint32 = counter__4.start
    var x1 uint64 = counter__4.end
    var jp12 bool
    switch x1 {
    case 900000000:
        var jp14 bool
        switch x0 {
        case 4000000000:
            jp14 = true
        default:
            jp14 = false
        }
        jp12 = jp14
        return jp12
    case 600000000:
        jp12 = true
        return jp12
    default:
        jp12 = false
        return jp12
    }
}

func report(label__5 string, value__6 bool) string {
    var t15 string = bool_to_string(value__6)
    var t16 string = label__5 + t15
    return t16
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
    var t17 bool = is_flag8(200)
    var t18 string = report("u8_hit=", t17)
    var t19 bool = is_flag8(15)
    var t20 string = report(",u8_miss=", t19)
    var t21 string = t18 + t20
    var t22 bool = is_flag16(65000)
    var t23 string = report(",u16_hit=", t22)
    var t24 string = t21 + t23
    var t25 bool = is_flag16(42)
    var t26 string = report(",u16_miss=", t25)
    var t27 string = t24 + t26
    var t28 bool = is_flag32(1234567890)
    var t29 string = report(",u32_hit=", t28)
    var t30 string = t27 + t29
    var t31 bool = is_flag32(99)
    var t32 string = report(",u32_miss=", t31)
    var t33 string = t30 + t32
    var t34 bool = is_flag64(900000000)
    var t35 string = report(",u64_hit=", t34)
    var t36 string = t33 + t35
    var t37 bool = is_flag64(700000000)
    var t38 string = report(",u64_miss=", t37)
    var t39 string = t36 + t38
    var t40 bool = match_struct(counter__7)
    var t41 string = report(",struct_first=", t40)
    var t42 string = t39 + t41
    var t43 bool = match_struct(alt_counter__8)
    var t44 string = report(",struct_second=", t43)
    var message__9 string = t42 + t44
    string_println(message__9)
    return struct{}{}
}

func main() {
    main0()
}
