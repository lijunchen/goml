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
    if ok__0 {
        var t175 Result__int32__string = Ok{
            _0: 10,
        }
        return t175
    } else {
        var t176 Result__int32__string = Err{
            _0: "left failed",
        }
        return t176
    }
}

func read_right(ok__1 bool) Result__int32__string {
    if ok__1 {
        var t181 Result__int32__string = Ok{
            _0: 20,
        }
        return t181
    } else {
        var t182 Result__int32__string = Err{
            _0: "right failed",
        }
        return t182
    }
}

func choose(choice__2 Choice) Result__int32__string {
    var jp186 int32
    switch choice__2.(type) {
    case Left:
        var x155 bool = choice__2.(Left)._0
        var mtmp158 Result__int32__string = read_left(x155)
        switch mtmp158.(type) {
        case Ok:
            var x159 int32 = mtmp158.(Ok)._0
            jp186 = x159
            var t187 Result__int32__string = Ok{
                _0: jp186,
            }
            return t187
        case Err:
            var x160 string = mtmp158.(Err)._0
            var t190 Result__int32__string = Err{
                _0: x160,
            }
            return t190
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x156 bool = choice__2.(Right)._0
        var mtmp161 Result__int32__string = read_right(x156)
        var jp192 int32
        switch mtmp161.(type) {
        case Ok:
            var x162 int32 = mtmp161.(Ok)._0
            jp192 = x162
            var t193 int32 = jp192 + 1
            jp186 = t193
            var t187 Result__int32__string = Ok{
                _0: jp186,
            }
            return t187
        case Err:
            var x163 string = mtmp161.(Err)._0
            var t194 Result__int32__string = Err{
                _0: x163,
            }
            return t194
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x157 int32 = choice__2.(Keep)._0
        jp186 = x157
        var t187 Result__int32__string = Ok{
            _0: jp186,
        }
        return t187
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x164 int32 = res__7.(Ok)._0
        var t199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x164)
        var t200 string = "ok " + t199
        return t200
    case Err:
        var x165 string = res__7.(Err)._0
        var t201 string = "err " + x165
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t203 Choice = Left{
        _0: true,
    }
    var t204 Result__int32__string = choose(t203)
    var t205 string = show(t204)
    println__T_string(t205)
    var t206 Choice = Right{
        _0: true,
    }
    var t207 Result__int32__string = choose(t206)
    var t208 string = show(t207)
    println__T_string(t208)
    var t209 Choice = Keep{
        _0: 5,
    }
    var t210 Result__int32__string = choose(t209)
    var t211 string = show(t210)
    println__T_string(t211)
    var t212 Choice = Left{
        _0: false,
    }
    var t213 Result__int32__string = choose(t212)
    var t214 string = show(t213)
    println__T_string(t214)
    var t215 Choice = Right{
        _0: false,
    }
    var t216 Result__int32__string = choose(t215)
    var t217 string = show(t216)
    println__T_string(t217)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t220 string = _goml_runtime_core_int32_to_string(self__6)
    return t220
}

func println__T_string(value__1 string) struct{} {
    var t222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
