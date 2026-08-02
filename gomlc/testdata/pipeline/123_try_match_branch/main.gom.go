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
    var retv172 Result__int32__string
    var jp174 Result__int32__string
    if ok__0 {
        var t175 Result__int32__string = Ok{
            _0: 10,
        }
        jp174 = t175
    } else {
        var t176 Result__int32__string = Err{
            _0: "left failed",
        }
        jp174 = t176
    }
    retv172 = jp174
    return retv172
}

func read_right(ok__1 bool) Result__int32__string {
    var retv178 Result__int32__string
    var jp180 Result__int32__string
    if ok__1 {
        var t181 Result__int32__string = Ok{
            _0: 20,
        }
        jp180 = t181
    } else {
        var t182 Result__int32__string = Err{
            _0: "right failed",
        }
        jp180 = t182
    }
    retv178 = jp180
    return retv178
}

func choose(choice__2 Choice) Result__int32__string {
    var retv184 Result__int32__string
    var jp186 int32
    switch choice__2.(type) {
    case Left:
        var x155 bool = choice__2.(Left)._0
        var ok__3 bool = x155
        var mtmp158 Result__int32__string = read_left(ok__3)
        var jp189 int32
        switch mtmp158.(type) {
        case Ok:
            var x159 int32 = mtmp158.(Ok)._0
            var try_value__21 int32 = x159
            jp189 = try_value__21
            jp186 = jp189
            var value__6 int32 = jp186
            var t187 Result__int32__string = Ok{
                _0: value__6,
            }
            retv184 = t187
            return retv184
        case Err:
            var x160 string = mtmp158.(Err)._0
            var try_residual__21 string = x160
            var t190 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv184 = t190
            return retv184
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x156 bool = choice__2.(Right)._0
        var ok__4 bool = x156
        var mtmp161 Result__int32__string = read_right(ok__4)
        var jp192 int32
        switch mtmp161.(type) {
        case Ok:
            var x162 int32 = mtmp161.(Ok)._0
            var try_value__25 int32 = x162
            jp192 = try_value__25
            var t193 int32 = jp192 + 1
            jp186 = t193
            var value__6 int32 = jp186
            var t187 Result__int32__string = Ok{
                _0: value__6,
            }
            retv184 = t187
            return retv184
        case Err:
            var x163 string = mtmp161.(Err)._0
            var try_residual__25 string = x163
            var t194 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv184 = t194
            return retv184
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x157 int32 = choice__2.(Keep)._0
        var value__5 int32 = x157
        jp186 = value__5
        var value__6 int32 = jp186
        var t187 Result__int32__string = Ok{
            _0: value__6,
        }
        retv184 = t187
        return retv184
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv196 string
    var jp198 string
    switch res__7.(type) {
    case Ok:
        var x164 int32 = res__7.(Ok)._0
        var value__8 int32 = x164
        var t199 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t200 string = "ok " + t199
        jp198 = t200
    case Err:
        var x165 string = res__7.(Err)._0
        var err__9 string = x165
        var t201 string = "err " + err__9
        jp198 = t201
    default:
        panic("non-exhaustive match")
    }
    retv196 = jp198
    return retv196
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
    var retv219 string
    var t220 string = _goml_runtime_core_int32_to_string(self__6)
    retv219 = t220
    return retv219
}

func println__T_string(value__1 string) struct{} {
    var t222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv225 string
    retv225 = self__38
    return retv225
}

func main() {
    main0()
}
