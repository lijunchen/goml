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
    var retv75 Result__int32__string
    var jp77 Result__int32__string
    if ok__0 {
        var t78 Result__int32__string = Ok{
            _0: 10,
        }
        jp77 = t78
    } else {
        var t79 Result__int32__string = Err{
            _0: "left failed",
        }
        jp77 = t79
    }
    retv75 = jp77
    return retv75
}

func read_right(ok__1 bool) Result__int32__string {
    var retv81 Result__int32__string
    var jp83 Result__int32__string
    if ok__1 {
        var t84 Result__int32__string = Ok{
            _0: 20,
        }
        jp83 = t84
    } else {
        var t85 Result__int32__string = Err{
            _0: "right failed",
        }
        jp83 = t85
    }
    retv81 = jp83
    return retv81
}

func choose(choice__2 Choice) Result__int32__string {
    var retv87 Result__int32__string
    var jp89 int32
    switch choice__2.(type) {
    case Left:
        var x58 bool = choice__2.(Left)._0
        var ok__3 bool = x58
        var mtmp61 Result__int32__string = read_left(ok__3)
        var jp92 int32
        switch mtmp61.(type) {
        case Ok:
            var x62 int32 = mtmp61.(Ok)._0
            var try_value__21 int32 = x62
            jp92 = try_value__21
            jp89 = jp92
            var value__6 int32 = jp89
            var t90 Result__int32__string = Ok{
                _0: value__6,
            }
            retv87 = t90
            return retv87
        case Err:
            var x63 string = mtmp61.(Err)._0
            var try_residual__21 string = x63
            var t93 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv87 = t93
            return retv87
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x59 bool = choice__2.(Right)._0
        var ok__4 bool = x59
        var mtmp64 Result__int32__string = read_right(ok__4)
        var jp95 int32
        switch mtmp64.(type) {
        case Ok:
            var x65 int32 = mtmp64.(Ok)._0
            var try_value__25 int32 = x65
            jp95 = try_value__25
            var t96 int32 = jp95 + 1
            jp89 = t96
            var value__6 int32 = jp89
            var t90 Result__int32__string = Ok{
                _0: value__6,
            }
            retv87 = t90
            return retv87
        case Err:
            var x66 string = mtmp64.(Err)._0
            var try_residual__25 string = x66
            var t97 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv87 = t97
            return retv87
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x60 int32 = choice__2.(Keep)._0
        var value__5 int32 = x60
        jp89 = value__5
        var value__6 int32 = jp89
        var t90 Result__int32__string = Ok{
            _0: value__6,
        }
        retv87 = t90
        return retv87
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv99 string
    var jp101 string
    switch res__7.(type) {
    case Ok:
        var x67 int32 = res__7.(Ok)._0
        var value__8 int32 = x67
        var t102 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t103 string = "ok " + t102
        jp101 = t103
    case Err:
        var x68 string = res__7.(Err)._0
        var err__9 string = x68
        var t104 string = "err " + err__9
        jp101 = t104
    default:
        panic("non-exhaustive match")
    }
    retv99 = jp101
    return retv99
}

func main0() struct{} {
    var t106 Choice = Left{
        _0: true,
    }
    var t107 Result__int32__string = choose(t106)
    var t108 string = show(t107)
    println__T_string(t108)
    var t109 Choice = Right{
        _0: true,
    }
    var t110 Result__int32__string = choose(t109)
    var t111 string = show(t110)
    println__T_string(t111)
    var t112 Choice = Keep{
        _0: 5,
    }
    var t113 Result__int32__string = choose(t112)
    var t114 string = show(t113)
    println__T_string(t114)
    var t115 Choice = Left{
        _0: false,
    }
    var t116 Result__int32__string = choose(t115)
    var t117 string = show(t116)
    println__T_string(t117)
    var t118 Choice = Right{
        _0: false,
    }
    var t119 Result__int32__string = choose(t118)
    var t120 string = show(t119)
    println__T_string(t120)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__2)
    retv122 = t123
    return retv122
}

func println__T_string(value__1 string) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv128 string
    retv128 = self__34
    return retv128
}

func main() {
    main0()
}
