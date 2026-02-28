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
        goto b2
    case 200:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp4
    b2:
    jp4 = true
    goto b1
    b3:
    jp4 = true
    goto b1
    b4:
    jp4 = false
    goto b1
}

func is_flag16(value__1 uint16) bool {
    var jp6 bool
    switch value__1 {
    case 1024:
        goto b2
    case 65000:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp6
    b2:
    jp6 = true
    goto b1
    b3:
    jp6 = true
    goto b1
    b4:
    jp6 = false
    goto b1
}

func is_flag32(value__2 uint32) bool {
    var jp8 bool
    switch value__2 {
    case 4000000000:
        goto b2
    case 1234567890:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp8
    b2:
    jp8 = true
    goto b1
    b3:
    jp8 = true
    goto b1
    b4:
    jp8 = false
    goto b1
}

func is_flag64(value__3 uint64) bool {
    var jp10 bool
    switch value__3 {
    case 900000000:
        goto b2
    case 600000000:
        goto b3
    default:
        goto b4
    }
    b1:
    return jp10
    b2:
    jp10 = true
    goto b1
    b3:
    jp10 = true
    goto b1
    b4:
    jp10 = false
    goto b1
}

func match_struct(counter__4 Counter) bool {
    var x0 uint32
    var x1 uint64
    var jp12 bool
    var jp14 bool
    x0 = counter__4.start
    x1 = counter__4.end
    switch x1 {
    case 900000000:
        goto b2
    case 600000000:
        goto b6
    default:
        goto b7
    }
    b1:
    return jp12
    b2:
    switch x0 {
    case 4000000000:
        goto b4
    default:
        goto b5
    }
    b3:
    jp12 = jp14
    goto b1
    b4:
    jp14 = true
    goto b3
    b5:
    jp14 = false
    goto b3
    b6:
    jp12 = true
    goto b1
    b7:
    jp12 = false
    goto b1
}

func report(label__5 string, value__6 bool) string {
    var t15 string
    var t16 string
    t15 = bool_to_string(value__6)
    t16 = label__5 + t15
    return t16
}

func main0() struct{} {
    var counter__7 Counter
    var alt_counter__8 Counter
    var t17 bool
    var t18 string
    var t19 bool
    var t20 string
    var t21 string
    var t22 bool
    var t23 string
    var t24 string
    var t25 bool
    var t26 string
    var t27 string
    var t28 bool
    var t29 string
    var t30 string
    var t31 bool
    var t32 string
    var t33 string
    var t34 bool
    var t35 string
    var t36 string
    var t37 bool
    var t38 string
    var t39 string
    var t40 bool
    var t41 string
    var t42 string
    var t43 bool
    var t44 string
    var message__9 string
    counter__7 = Counter{
        start: 4000000000,
        end: 900000000,
    }
    alt_counter__8 = Counter{
        start: 12,
        end: 600000000,
    }
    t17 = is_flag8(200)
    t18 = report("u8_hit=", t17)
    t19 = is_flag8(15)
    t20 = report(",u8_miss=", t19)
    t21 = t18 + t20
    t22 = is_flag16(65000)
    t23 = report(",u16_hit=", t22)
    t24 = t21 + t23
    t25 = is_flag16(42)
    t26 = report(",u16_miss=", t25)
    t27 = t24 + t26
    t28 = is_flag32(1234567890)
    t29 = report(",u32_hit=", t28)
    t30 = t27 + t29
    t31 = is_flag32(99)
    t32 = report(",u32_miss=", t31)
    t33 = t30 + t32
    t34 = is_flag64(900000000)
    t35 = report(",u64_hit=", t34)
    t36 = t33 + t35
    t37 = is_flag64(700000000)
    t38 = report(",u64_miss=", t37)
    t39 = t36 + t38
    t40 = match_struct(counter__7)
    t41 = report(",struct_first=", t40)
    t42 = t39 + t41
    t43 = match_struct(alt_counter__8)
    t44 = report(",struct_second=", t43)
    message__9 = t42 + t44
    string_println(message__9)
    return struct{}{}
}

func main() {
    main0()
}
