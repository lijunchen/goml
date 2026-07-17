package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Choice interface {
    isChoice()
}

type Left struct {
    _0 bool
}

func (_ Left) isChoice() {}

type Right struct {
    _0 bool
}

func (_ Right) isChoice() {}

type Keep struct {
    _0 int32
}

func (_ Keep) isChoice() {}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func read_left(ok__0 bool) Result__int32__string {
    var retv78 Result__int32__string
    var jp80 Result__int32__string
    if ok__0 {
        var t81 Result__int32__string = Ok{
            _0: 10,
        }
        jp80 = t81
    } else {
        var t82 Result__int32__string = Err{
            _0: "left failed",
        }
        jp80 = t82
    }
    retv78 = jp80
    return retv78
}

func read_right(ok__1 bool) Result__int32__string {
    var retv84 Result__int32__string
    var jp86 Result__int32__string
    if ok__1 {
        var t87 Result__int32__string = Ok{
            _0: 20,
        }
        jp86 = t87
    } else {
        var t88 Result__int32__string = Err{
            _0: "right failed",
        }
        jp86 = t88
    }
    retv84 = jp86
    return retv84
}

func choose(choice__2 Choice) Result__int32__string {
    var retv90 Result__int32__string
    var jp92 int32
    switch choice__2.(type) {
    case Left:
        var x61 bool = choice__2.(Left)._0
        var ok__3 bool = x61
        var mtmp64 Result__int32__string = read_left(ok__3)
        var jp95 int32
        switch mtmp64.(type) {
        case Ok:
            var x65 int32 = mtmp64.(Ok)._0
            var try_value__21 int32 = x65
            jp95 = try_value__21
            jp92 = jp95
            var value__6 int32 = jp92
            var t93 Result__int32__string = Ok{
                _0: value__6,
            }
            retv90 = t93
            return retv90
        case Err:
            var x66 string = mtmp64.(Err)._0
            var try_residual__21 string = x66
            var t96 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv90 = t96
            return retv90
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x62 bool = choice__2.(Right)._0
        var ok__4 bool = x62
        var mtmp67 Result__int32__string = read_right(ok__4)
        var jp98 int32
        switch mtmp67.(type) {
        case Ok:
            var x68 int32 = mtmp67.(Ok)._0
            var try_value__25 int32 = x68
            jp98 = try_value__25
            var t99 int32 = jp98 + 1
            jp92 = t99
            var value__6 int32 = jp92
            var t93 Result__int32__string = Ok{
                _0: value__6,
            }
            retv90 = t93
            return retv90
        case Err:
            var x69 string = mtmp67.(Err)._0
            var try_residual__25 string = x69
            var t100 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv90 = t100
            return retv90
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x63 int32 = choice__2.(Keep)._0
        var value__5 int32 = x63
        jp92 = value__5
        var value__6 int32 = jp92
        var t93 Result__int32__string = Ok{
            _0: value__6,
        }
        retv90 = t93
        return retv90
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv102 string
    var jp104 string
    switch res__7.(type) {
    case Ok:
        var x70 int32 = res__7.(Ok)._0
        var value__8 int32 = x70
        var t105 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t106 string = "ok " + t105
        jp104 = t106
    case Err:
        var x71 string = res__7.(Err)._0
        var err__9 string = x71
        var t107 string = "err " + err__9
        jp104 = t107
    default:
        panic("non-exhaustive match")
    }
    retv102 = jp104
    return retv102
}

func main0() struct{} {
    var t109 Choice = Left{
        _0: true,
    }
    var t110 Result__int32__string = choose(t109)
    var t111 string = show(t110)
    println__T_string(t111)
    var t112 Choice = Right{
        _0: true,
    }
    var t113 Result__int32__string = choose(t112)
    var t114 string = show(t113)
    println__T_string(t114)
    var t115 Choice = Keep{
        _0: 5,
    }
    var t116 Result__int32__string = choose(t115)
    var t117 string = show(t116)
    println__T_string(t117)
    var t118 Choice = Left{
        _0: false,
    }
    var t119 Result__int32__string = choose(t118)
    var t120 string = show(t119)
    println__T_string(t120)
    var t121 Choice = Right{
        _0: false,
    }
    var t122 Result__int32__string = choose(t121)
    var t123 string = show(t122)
    println__T_string(t123)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int32_to_string(self__5)
    retv125 = t126
    return retv125
}

func println__T_string(value__1 string) struct{} {
    var t128 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t128)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv131 string
    retv131 = self__37
    return retv131
}

func main() {
    main0()
}
