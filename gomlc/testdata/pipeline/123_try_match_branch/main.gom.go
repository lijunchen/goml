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
    var jp213 int32
    switch choice__2.(type) {
    case Left:
        var x182 bool = choice__2.(Left)._0
        var commute_field301 int32
        var commute_field303 string
        if x182 {
            commute_field301 = 10
            jp213 = commute_field301
            var t214 Result__int32__string = Ok{
                _0: jp213,
            }
            return t214
        } else {
            commute_field303 = "left failed"
            var t217 Result__int32__string = Err{
                _0: commute_field303,
            }
            return t217
        }
    case Right:
        var x183 bool = choice__2.(Right)._0
        var mtmp188 Result__int32__string
        if x183 {
            var inline257 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp188 = inline257
        } else {
            var inline258 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp188 = inline258
        }
        var jp219 int32
        switch mtmp188.(type) {
        case Ok:
            var x189 int32 = mtmp188.(Ok)._0
            jp219 = x189
            var t220 int32 = jp219 + 1
            jp213 = t220
            var t214 Result__int32__string = Ok{
                _0: jp213,
            }
            return t214
        case Err:
            var x190 string = mtmp188.(Err)._0
            var t221 Result__int32__string = Err{
                _0: x190,
            }
            return t221
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x184 int32 = choice__2.(Keep)._0
        jp213 = x184
        var t214 Result__int32__string = Ok{
            _0: jp213,
        }
        return t214
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x191 int32 = res__7.(Ok)._0
        var t226 string
        var inline260 string = _goml_runtime_core_int32_to_string(x191)
        t226 = inline260
        var t227 string = "ok " + t226
        return t227
    case Err:
        var x192 string = res__7.(Err)._0
        var t228 string = "err " + x192
        return t228
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t230 Choice = Left{
        _0: true,
    }
    var t231 Result__int32__string = choose(t230)
    var t232 string = show(t231)
    var inline298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline298)
    var t233 Choice = Right{
        _0: true,
    }
    var t234 Result__int32__string = choose(t233)
    var t235 string = show(t234)
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline295)
    var t236 Choice = Keep{
        _0: 5,
    }
    var t237 Result__int32__string = choose(t236)
    var t238 string
    switch t237.(type) {
    case Ok:
        var inline287 int32 = t237.(Ok)._0
        var inline289 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline287)
        var inline290 string = "ok " + inline289
        t238 = inline290
    case Err:
        var inline291 string = t237.(Err)._0
        var inline293 string = "err " + inline291
        t238 = inline293
    default:
        panic("non-exhaustive match")
    }
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t238)
    _goml_runtime_core_string_println(inline284)
    var t239 Choice = Left{
        _0: false,
    }
    var t240 Result__int32__string = choose(t239)
    var t241 string
    switch t240.(type) {
    case Ok:
        var inline276 int32 = t240.(Ok)._0
        var inline278 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline276)
        var inline279 string = "ok " + inline278
        t241 = inline279
    case Err:
        var inline280 string = t240.(Err)._0
        var inline282 string = "err " + inline280
        t241 = inline282
    default:
        panic("non-exhaustive match")
    }
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline273)
    var t242 Choice = Right{
        _0: false,
    }
    var t243 Result__int32__string = choose(t242)
    var t244 string
    switch t243.(type) {
    case Ok:
        var inline265 int32 = t243.(Ok)._0
        var inline267 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline265)
        var inline268 string = "ok " + inline267
        t244 = inline268
    case Err:
        var inline269 string = t243.(Err)._0
        var inline271 string = "err " + inline269
        t244 = inline271
    default:
        panic("non-exhaustive match")
    }
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline262)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t247 string = _goml_runtime_core_int32_to_string(self__33)
    return t247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
