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
    var retv169 Result__int32__string
    var jp171 Result__int32__string
    if ok__0 {
        var t172 Result__int32__string = Ok{
            _0: 10,
        }
        jp171 = t172
    } else {
        var t173 Result__int32__string = Err{
            _0: "left failed",
        }
        jp171 = t173
    }
    retv169 = jp171
    return retv169
}

func read_right(ok__1 bool) Result__int32__string {
    var retv175 Result__int32__string
    var jp177 Result__int32__string
    if ok__1 {
        var t178 Result__int32__string = Ok{
            _0: 20,
        }
        jp177 = t178
    } else {
        var t179 Result__int32__string = Err{
            _0: "right failed",
        }
        jp177 = t179
    }
    retv175 = jp177
    return retv175
}

func choose(choice__2 Choice) Result__int32__string {
    var retv181 Result__int32__string
    var jp183 int32
    switch choice__2.(type) {
    case Left:
        var x152 bool = choice__2.(Left)._0
        var ok__3 bool = x152
        var mtmp155 Result__int32__string = read_left(ok__3)
        var jp186 int32
        switch mtmp155.(type) {
        case Ok:
            var x156 int32 = mtmp155.(Ok)._0
            var try_value__21 int32 = x156
            jp186 = try_value__21
            jp183 = jp186
            var value__6 int32 = jp183
            var t184 Result__int32__string = Ok{
                _0: value__6,
            }
            retv181 = t184
            return retv181
        case Err:
            var x157 string = mtmp155.(Err)._0
            var try_residual__21 string = x157
            var t187 Result__int32__string = Err{
                _0: try_residual__21,
            }
            retv181 = t187
            return retv181
        default:
            panic("non-exhaustive match")
        }
    case Right:
        var x153 bool = choice__2.(Right)._0
        var ok__4 bool = x153
        var mtmp158 Result__int32__string = read_right(ok__4)
        var jp189 int32
        switch mtmp158.(type) {
        case Ok:
            var x159 int32 = mtmp158.(Ok)._0
            var try_value__25 int32 = x159
            jp189 = try_value__25
            var t190 int32 = jp189 + 1
            jp183 = t190
            var value__6 int32 = jp183
            var t184 Result__int32__string = Ok{
                _0: value__6,
            }
            retv181 = t184
            return retv181
        case Err:
            var x160 string = mtmp158.(Err)._0
            var try_residual__25 string = x160
            var t191 Result__int32__string = Err{
                _0: try_residual__25,
            }
            retv181 = t191
            return retv181
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x154 int32 = choice__2.(Keep)._0
        var value__5 int32 = x154
        jp183 = value__5
        var value__6 int32 = jp183
        var t184 Result__int32__string = Ok{
            _0: value__6,
        }
        retv181 = t184
        return retv181
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    var retv193 string
    var jp195 string
    switch res__7.(type) {
    case Ok:
        var x161 int32 = res__7.(Ok)._0
        var value__8 int32 = x161
        var t196 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__8)
        var t197 string = "ok " + t196
        jp195 = t197
    case Err:
        var x162 string = res__7.(Err)._0
        var err__9 string = x162
        var t198 string = "err " + err__9
        jp195 = t198
    default:
        panic("non-exhaustive match")
    }
    retv193 = jp195
    return retv193
}

func main0() struct{} {
    var t200 Choice = Left{
        _0: true,
    }
    var t201 Result__int32__string = choose(t200)
    var t202 string = show(t201)
    println__T_string(t202)
    var t203 Choice = Right{
        _0: true,
    }
    var t204 Result__int32__string = choose(t203)
    var t205 string = show(t204)
    println__T_string(t205)
    var t206 Choice = Keep{
        _0: 5,
    }
    var t207 Result__int32__string = choose(t206)
    var t208 string = show(t207)
    println__T_string(t208)
    var t209 Choice = Left{
        _0: false,
    }
    var t210 Result__int32__string = choose(t209)
    var t211 string = show(t210)
    println__T_string(t211)
    var t212 Choice = Right{
        _0: false,
    }
    var t213 Result__int32__string = choose(t212)
    var t214 string = show(t213)
    println__T_string(t214)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv216 string
    var t217 string = _goml_runtime_core_int32_to_string(self__6)
    retv216 = t217
    return retv216
}

func println__T_string(value__1 string) struct{} {
    var t219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv222 string
    retv222 = self__38
    return retv222
}

func main() {
    main0()
}
