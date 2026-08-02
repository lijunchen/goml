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
    var jp186 int32
    switch choice__2.(type) {
    case Left:
        var x155 bool = choice__2.(Left)._0
        var commute_field274 int32
        var commute_field276 string
        if x155 {
            commute_field274 = 10
            jp186 = commute_field274
            var t187 Result__int32__string = Ok{
                _0: jp186,
            }
            return t187
        } else {
            commute_field276 = "left failed"
            var t190 Result__int32__string = Err{
                _0: commute_field276,
            }
            return t190
        }
    case Right:
        var x156 bool = choice__2.(Right)._0
        var mtmp161 Result__int32__string
        if x156 {
            var inline230 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp161 = inline230
        } else {
            var inline231 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp161 = inline231
        }
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
        var t199 string
        var inline233 string = _goml_runtime_core_int32_to_string(x164)
        t199 = inline233
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
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline271)
    var t206 Choice = Right{
        _0: true,
    }
    var t207 Result__int32__string = choose(t206)
    var t208 string = show(t207)
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline268)
    var t209 Choice = Keep{
        _0: 5,
    }
    var t210 Result__int32__string = choose(t209)
    var t211 string
    switch t210.(type) {
    case Ok:
        var inline260 int32 = t210.(Ok)._0
        var inline262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline260)
        var inline263 string = "ok " + inline262
        t211 = inline263
    case Err:
        var inline264 string = t210.(Err)._0
        var inline266 string = "err " + inline264
        t211 = inline266
    default:
        panic("non-exhaustive match")
    }
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline257)
    var t212 Choice = Left{
        _0: false,
    }
    var t213 Result__int32__string = choose(t212)
    var t214 string
    switch t213.(type) {
    case Ok:
        var inline249 int32 = t213.(Ok)._0
        var inline251 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline249)
        var inline252 string = "ok " + inline251
        t214 = inline252
    case Err:
        var inline253 string = t213.(Err)._0
        var inline255 string = "err " + inline253
        t214 = inline255
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline246)
    var t215 Choice = Right{
        _0: false,
    }
    var t216 Result__int32__string = choose(t215)
    var t217 string
    switch t216.(type) {
    case Ok:
        var inline238 int32 = t216.(Ok)._0
        var inline240 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline238)
        var inline241 string = "ok " + inline240
        t217 = inline241
    case Err:
        var inline242 string = t216.(Err)._0
        var inline244 string = "err " + inline242
        t217 = inline244
    default:
        panic("non-exhaustive match")
    }
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t220 string = _goml_runtime_core_int32_to_string(self__6)
    return t220
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
