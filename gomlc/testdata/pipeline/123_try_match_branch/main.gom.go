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
    var retv125 Result__int32__string
    var jp127 Result__int32__string
    if ok__0 {
        var t128 Result__int32__string = Ok{
            _0: 10,
        }
        jp127 = t128
    } else {
        var t129 Result__int32__string = Err{
            _0: "left failed",
        }
        jp127 = t129
    }
    retv125 = jp127
    return retv125
}

func read_right(ok__1 bool) Result__int32__string {
    var retv131 Result__int32__string
    var jp133 Result__int32__string
    if ok__1 {
        var t134 Result__int32__string = Ok{
            _0: 20,
        }
        jp133 = t134
    } else {
        var t135 Result__int32__string = Err{
            _0: "right failed",
        }
        jp133 = t135
    }
    retv131 = jp133
    return retv131
}

func choose(choice__2 Choice) Result__int32__string {
    var retv137 Result__int32__string
    var jp139 int32
    switch choice__2.(type) {
    case Left:
        var x108 bool = choice__2.(Left)._0
        var ok__3 bool = x108
        var mtmp111 Result__int32__string = read_left(ok__3)
        var jp142 int32
        switch mtmp111.(type) {
        case Ok:
            var x112 int32 = mtmp111.(Ok)._0
            var try_value__21 int32 = x112
            jp142 = try_value__21
            jp139 = jp142
            var value__6 int32 = jp139
            var t140 Result__int32__string = Ok{
                _0: value__6,
            }
            retv137 = t140
            return retv137
        case Err:
            var x113 string = mtmp111.(Err)._0
            var try_residual__21 string = x113
            var t143 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv137 = t143
            return retv137
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x109 bool = choice__2.(Right)._0
        var ok__4 bool = x109
        var mtmp114 Result__int32__string = read_right(ok__4)
        var jp145 int32
        switch mtmp114.(type) {
        case Ok:
            var x115 int32 = mtmp114.(Ok)._0
            var try_value__25 int32 = x115
            jp145 = try_value__25
            var t146 int32 = jp145 + 1
            jp139 = t146
            var value__6 int32 = jp139
            var t140 Result__int32__string = Ok{
                _0: value__6,
            }
            retv137 = t140
            return retv137
        case Err:
            var x116 string = mtmp114.(Err)._0
            var try_residual__25 string = x116
            var t147 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv137 = t147
            return retv137
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x110 int32 = choice__2.(Keep)._0
        var value__5 int32 = x110
        jp139 = value__5
        var value__6 int32 = jp139
        var t140 Result__int32__string = Ok{
            _0: value__6,
        }
        retv137 = t140
        return retv137
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv149 string
    var jp151 string
    switch res__7.(type) {
    case Ok:
        var x117 int32 = res__7.(Ok)._0
        var value__8 int32 = x117
        var t152 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t153 string = "ok " + t152
        jp151 = t153
    case Err:
        var x118 string = res__7.(Err)._0
        var err__9 string = x118
        var t154 string = "err " + err__9
        jp151 = t154
    default:
        panic("non-exhaustive match")
    }
    retv149 = jp151
    return retv149
}

func main0() struct{} {
    var t156 Choice = Left{
        _0: true,
    }
    var t157 Result__int32__string = choose(t156)
    var t158 string = show(t157)
    println__T_string(t158)
    var t159 Choice = Right{
        _0: true,
    }
    var t160 Result__int32__string = choose(t159)
    var t161 string = show(t160)
    println__T_string(t161)
    var t162 Choice = Keep{
        _0: 5,
    }
    var t163 Result__int32__string = choose(t162)
    var t164 string = show(t163)
    println__T_string(t164)
    var t165 Choice = Left{
        _0: false,
    }
    var t166 Result__int32__string = choose(t165)
    var t167 string = show(t166)
    println__T_string(t167)
    var t168 Choice = Right{
        _0: false,
    }
    var t169 Result__int32__string = choose(t168)
    var t170 string = show(t169)
    println__T_string(t170)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv172 string
    var t173 string = _goml_runtime_core_int32_to_string(self__6)
    retv172 = t173
    return retv172
}

func println__T_string(value__1 string) struct{} {
    var t175 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t175)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv178 string
    retv178 = self__38
    return retv178
}

func main() {
    main0()
}
