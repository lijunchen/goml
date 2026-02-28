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

type Tuple2_int8_int16 struct {
    _0 int8
    _1 int16
}

type PairData struct {
    head int32
    tail int64
}

func is_special8(value__0 int8) bool {
    var jp6 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__0 {
            case 5:
                pc = 2
            case 7:
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

func is_special16(value__1 int16) bool {
    var jp8 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__1 {
            case 1024:
                pc = 2
            case 2048:
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

func is_special32(value__2 int32) bool {
    var jp10 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__2 {
            case 4096:
                pc = 2
            case 8192:
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

func is_special64(value__3 int64) bool {
    var jp12 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch value__3 {
            case 16384:
                pc = 2
            case 32768:
                pc = 3
            default:
                pc = 4
            }
        case 1:
            return jp12
        case 2:
            jp12 = true
            pc = 1
        case 3:
            jp12 = true
            pc = 1
        case 4:
            jp12 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func match_tuple(values__4 Tuple2_int8_int16) bool {
    var x0 int8
    var x1 int16
    var jp14 bool
    var jp16 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x0 = values__4._0
            x1 = values__4._1
            switch x1 {
            case 2:
                pc = 2
            default:
                pc = 6
            }
        case 1:
            return jp14
        case 2:
            switch x0 {
            case 1:
                pc = 4
            default:
                pc = 5
            }
        case 3:
            jp14 = jp16
            pc = 1
        case 4:
            jp16 = true
            pc = 3
        case 5:
            jp16 = false
            pc = 3
        case 6:
            jp14 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func match_struct(pair__5 PairData) bool {
    var x2 int32
    var x3 int64
    var jp18 bool
    var jp20 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            x2 = pair__5.head
            x3 = pair__5.tail
            switch x3 {
            case 200:
                pc = 2
            case 300:
                pc = 6
            default:
                pc = 7
            }
        case 1:
            return jp18
        case 2:
            switch x2 {
            case 100:
                pc = 4
            default:
                pc = 5
            }
        case 3:
            jp18 = jp20
            pc = 1
        case 4:
            jp20 = true
            pc = 3
        case 5:
            jp20 = false
            pc = 3
        case 6:
            jp18 = true
            pc = 1
        case 7:
            jp18 = false
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func report(label__6 string, value__7 bool) string {
    var t21 string
    var t22 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t21 = bool_to_string(value__7)
            t22 = label__6 + t21
            return t22
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var tuple_first__8 int8
    var tuple_second__9 int16
    var t23 Tuple2_int8_int16
    var tuple_result_hit__10 bool
    var t24 Tuple2_int8_int16
    var tuple_result_miss__11 bool
    var t25 PairData
    var pair_first__12 bool
    var t26 PairData
    var pair_second__13 bool
    var t27 bool
    var part1__14 string
    var t28 bool
    var part2__15 string
    var t29 bool
    var part3__16 string
    var t30 bool
    var part4__17 string
    var t31 bool
    var part5__18 string
    var part6__19 string
    var part7__20 string
    var part8__21 string
    var part9__22 string
    var t32 string
    var t33 string
    var t34 string
    var t35 string
    var t36 string
    var t37 string
    var t38 string
    var message__23 string
    var mtmp4 struct{}
    _ = mtmp4
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            tuple_first__8 = 1
            tuple_second__9 = 2
            t23 = Tuple2_int8_int16{
                _0: tuple_first__8,
                _1: tuple_second__9,
            }
            tuple_result_hit__10 = match_tuple(t23)
            t24 = Tuple2_int8_int16{
                _0: 3,
                _1: 4,
            }
            tuple_result_miss__11 = match_tuple(t24)
            t25 = PairData{
                head: 100,
                tail: 200,
            }
            pair_first__12 = match_struct(t25)
            t26 = PairData{
                head: 10,
                tail: 300,
            }
            pair_second__13 = match_struct(t26)
            t27 = is_special8(5)
            part1__14 = report("int8=", t27)
            t28 = is_special16(1024)
            part2__15 = report(",int16=", t28)
            t29 = is_special32(8192)
            part3__16 = report(",int32=", t29)
            t30 = is_special64(16384)
            part4__17 = report(",int64_a=", t30)
            t31 = is_special64(32768)
            part5__18 = report(",int64_b=", t31)
            part6__19 = report(",tuple_hit=", tuple_result_hit__10)
            part7__20 = report(",tuple_miss=", tuple_result_miss__11)
            part8__21 = report(",struct_first=", pair_first__12)
            part9__22 = report(",struct_second=", pair_second__13)
            t32 = part1__14 + part2__15
            t33 = t32 + part3__16
            t34 = t33 + part4__17
            t35 = t34 + part5__18
            t36 = t35 + part6__19
            t37 = t36 + part7__20
            t38 = t37 + part8__21
            message__23 = t38 + part9__22
            string_println(message__23)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
