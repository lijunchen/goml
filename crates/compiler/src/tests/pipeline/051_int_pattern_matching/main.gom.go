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

type GoError = error

func is_special8(value__0 int8) bool {
    var retv6 bool
    var jp8 bool
    switch value__0 {
    case 5:
        jp8 = true
    case 7:
        jp8 = true
    default:
        jp8 = false
    }
    retv6 = jp8
    return retv6
}

func is_special16(value__1 int16) bool {
    var retv10 bool
    var jp12 bool
    switch value__1 {
    case 1024:
        jp12 = true
    case 2048:
        jp12 = true
    default:
        jp12 = false
    }
    retv10 = jp12
    return retv10
}

func is_special32(value__2 int32) bool {
    var retv14 bool
    var jp16 bool
    switch value__2 {
    case 4096:
        jp16 = true
    case 8192:
        jp16 = true
    default:
        jp16 = false
    }
    retv14 = jp16
    return retv14
}

func is_special64(value__3 int64) bool {
    var retv18 bool
    var jp20 bool
    switch value__3 {
    case 16384:
        jp20 = true
    case 32768:
        jp20 = true
    default:
        jp20 = false
    }
    retv18 = jp20
    return retv18
}

func match_tuple(values__4 Tuple2_int8_int16) bool {
    var retv22 bool
    var x0 int8 = values__4._0
    var x1 int16 = values__4._1
    var jp24 bool
    switch x1 {
    case 2:
        var jp26 bool
        switch x0 {
        case 1:
            jp26 = true
        default:
            jp26 = false
        }
        jp24 = jp26
    default:
        jp24 = false
    }
    retv22 = jp24
    return retv22
}

func match_struct(pair__5 PairData) bool {
    var retv28 bool
    var x2 int32 = pair__5.head
    var x3 int64 = pair__5.tail
    var jp30 bool
    switch x3 {
    case 200:
        var jp32 bool
        switch x2 {
        case 100:
            jp32 = true
        default:
            jp32 = false
        }
        jp30 = jp32
    case 300:
        jp30 = true
    default:
        jp30 = false
    }
    retv28 = jp30
    return retv28
}

func report(label__6 string, value__7 bool) string {
    var retv34 string
    var t35 string = bool_to_string(value__7)
    var t36 string = label__6 + t35
    retv34 = t36
    return retv34
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t38 Tuple2_int8_int16 = Tuple2_int8_int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t38)
    var t39 Tuple2_int8_int16 = Tuple2_int8_int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t39)
    var t40 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t40)
    var t41 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t41)
    var t42 bool = is_special8(5)
    var part1__14 string = report("int8=", t42)
    var t43 bool = is_special16(1024)
    var part2__15 string = report(",int16=", t43)
    var t44 bool = is_special32(8192)
    var part3__16 string = report(",int32=", t44)
    var t45 bool = is_special64(16384)
    var part4__17 string = report(",int64_a=", t45)
    var t46 bool = is_special64(32768)
    var part5__18 string = report(",int64_b=", t46)
    var part6__19 string = report(",tuple_hit=", tuple_result_hit__10)
    var part7__20 string = report(",tuple_miss=", tuple_result_miss__11)
    var part8__21 string = report(",struct_first=", pair_first__12)
    var part9__22 string = report(",struct_second=", pair_second__13)
    var t47 string = part1__14 + part2__15
    var t48 string = t47 + part3__16
    var t49 string = t48 + part4__17
    var t50 string = t49 + part5__18
    var t51 string = t50 + part6__19
    var t52 string = t51 + part7__20
    var t53 string = t52 + part8__21
    var message__23 string = t53 + part9__22
    string_println(message__23)
    return struct{}{}
}

func main() {
    main0()
}
