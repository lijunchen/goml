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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__0 {
            case 0:
                pc = 2
            case 200:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp4
        case 2:
            jp4 = true
            pc = 1
        case 3:
            jp4 = true
            pc = 1
        case 4:
            jp4 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func is_flag16(value__1 uint16) bool {
    var jp6 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__1 {
            case 1024:
                pc = 2
            case 65000:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp6
        case 2:
            jp6 = true
            pc = 1
        case 3:
            jp6 = true
            pc = 1
        case 4:
            jp6 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func is_flag32(value__2 uint32) bool {
    var jp8 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__2 {
            case 4000000000:
                pc = 2
            case 1234567890:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp8
        case 2:
            jp8 = true
            pc = 1
        case 3:
            jp8 = true
            pc = 1
        case 4:
            jp8 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func is_flag64(value__3 uint64) bool {
    var jp10 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__3 {
            case 900000000:
                pc = 2
            case 600000000:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp10
        case 2:
            jp10 = true
            pc = 1
        case 3:
            jp10 = true
            pc = 1
        case 4:
            jp10 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func match_struct(counter__4 Counter) bool {
    var x0 uint32
    var x1 uint64
    var jp12 bool
    var jp14 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x0 = counter__4.start
            x1 = counter__4.end
            switch x1 {
            case 900000000:
                pc = 2
            case 600000000:
                pc = 6
            default:
                pc = 7
            }
        case 1:
            return jp12
        case 2:
            switch x0 {
            case 4000000000:
                pc = 4
            default:
                pc = 5
            }
        case 3:
            jp12 = jp14
            pc = 1
        case 4:
            jp14 = true
            pc = 3
        case 5:
            jp14 = false
            pc = 3
        case 6:
            jp12 = true
            pc = 1
        case 7:
            jp12 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func report(label__5 string, value__6 bool) string {
    var t15 string
    var t16 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t15 = bool_to_string(value__6)
            t16 = label__5 + t15
            return t16
        default:
            panic("invalid pc")
        }
    }
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
    var mtmp2 struct{}
    _ = mtmp2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
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
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
