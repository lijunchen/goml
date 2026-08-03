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

func choose(choice__2 Choice) Result__int32__string {
    var jp167 int32
    switch choice__2.(type) {
    case Left:
        var x136 bool = choice__2.(Left)._0
        var commute_field255 int32
        var commute_field257 string
        if x136 {
            commute_field255 = 10
            jp167 = commute_field255
            var t168 Result__int32__string = Ok{
                _0: jp167,
            }
            return t168
        } else {
            commute_field257 = "left failed"
            var t171 Result__int32__string = Err{
                _0: commute_field257,
            }
            return t171
        }
    case Right:
        var x137 bool = choice__2.(Right)._0
        var mtmp142 Result__int32__string
        if x137 {
            var inline211 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp142 = inline211
        } else {
            var inline212 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp142 = inline212
        }
        var jp173 int32
        switch mtmp142.(type) {
        case Ok:
            var x143 int32 = mtmp142.(Ok)._0
            jp173 = x143
            var t174 int32 = jp173 + 1
            jp167 = t174
            var t168 Result__int32__string = Ok{
                _0: jp167,
            }
            return t168
        case Err:
            var x144 string = mtmp142.(Err)._0
            var t175 Result__int32__string = Err{
                _0: x144,
            }
            return t175
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x138 int32 = choice__2.(Keep)._0
        jp167 = x138
        var t168 Result__int32__string = Ok{
            _0: jp167,
        }
        return t168
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x145 int32 = res__7.(Ok)._0
        var t180 string
        var inline214 string = _goml_runtime_core_int32_to_string(x145)
        t180 = inline214
        var t181 string = "ok " + t180
        return t181
    case Err:
        var x146 string = res__7.(Err)._0
        var t182 string = "err " + x146
        return t182
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t184 Choice = Left{
        _0: true,
    }
    var t185 Result__int32__string = choose(t184)
    var t186 string = show(t185)
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline252)
    var t187 Choice = Right{
        _0: true,
    }
    var t188 Result__int32__string = choose(t187)
    var t189 string = show(t188)
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline249)
    var t190 Choice = Keep{
        _0: 5,
    }
    var t191 Result__int32__string = choose(t190)
    var t192 string
    switch t191.(type) {
    case Ok:
        var inline241 int32 = t191.(Ok)._0
        var inline243 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline241)
        var inline244 string = "ok " + inline243
        t192 = inline244
    case Err:
        var inline245 string = t191.(Err)._0
        var inline247 string = "err " + inline245
        t192 = inline247
    default:
        panic("non-exhaustive match")
    }
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
    _goml_runtime_core_string_println(inline238)
    var t193 Choice = Left{
        _0: false,
    }
    var t194 Result__int32__string = choose(t193)
    var t195 string
    switch t194.(type) {
    case Ok:
        var inline230 int32 = t194.(Ok)._0
        var inline232 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline230)
        var inline233 string = "ok " + inline232
        t195 = inline233
    case Err:
        var inline234 string = t194.(Err)._0
        var inline236 string = "err " + inline234
        t195 = inline236
    default:
        panic("non-exhaustive match")
    }
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline227)
    var t196 Choice = Right{
        _0: false,
    }
    var t197 Result__int32__string = choose(t196)
    var t198 string
    switch t197.(type) {
    case Ok:
        var inline219 int32 = t197.(Ok)._0
        var inline221 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline219)
        var inline222 string = "ok " + inline221
        t198 = inline222
    case Err:
        var inline223 string = t197.(Err)._0
        var inline225 string = "err " + inline223
        t198 = inline225
    default:
        panic("non-exhaustive match")
    }
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline216)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t201 string = _goml_runtime_core_int32_to_string(self__35)
    return t201
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
