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
    var jp218 int32
    switch choice__2.(type) {
    case Left:
        var x187 bool = choice__2.(Left)._0
        var commute_field306 int32
        var commute_field308 string
        if x187 {
            commute_field306 = 10
            jp218 = commute_field306
            var t219 Result__int32__string = Ok{
                _0: jp218,
            }
            return t219
        } else {
            commute_field308 = "left failed"
            var t222 Result__int32__string = Err{
                _0: commute_field308,
            }
            return t222
        }
    case Right:
        var x188 bool = choice__2.(Right)._0
        var mtmp193 Result__int32__string
        if x188 {
            var inline262 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp193 = inline262
        } else {
            var inline263 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp193 = inline263
        }
        var jp224 int32
        switch mtmp193.(type) {
        case Ok:
            var x194 int32 = mtmp193.(Ok)._0
            jp224 = x194
            var t225 int32 = jp224 + 1
            jp218 = t225
            var t219 Result__int32__string = Ok{
                _0: jp218,
            }
            return t219
        case Err:
            var x195 string = mtmp193.(Err)._0
            var t226 Result__int32__string = Err{
                _0: x195,
            }
            return t226
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x189 int32 = choice__2.(Keep)._0
        jp218 = x189
        var t219 Result__int32__string = Ok{
            _0: jp218,
        }
        return t219
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x196 int32 = res__7.(Ok)._0
        var t231 string
        var inline265 string = _goml_runtime_core_int32_to_string(x196)
        t231 = inline265
        var t232 string = "ok " + t231
        return t232
    case Err:
        var x197 string = res__7.(Err)._0
        var t233 string = "err " + x197
        return t233
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t235 Choice = Left{
        _0: true,
    }
    var t236 Result__int32__string = choose(t235)
    var t237 string = show(t236)
    var inline303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline303)
    var t238 Choice = Right{
        _0: true,
    }
    var t239 Result__int32__string = choose(t238)
    var t240 string = show(t239)
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t240)
    _goml_runtime_core_string_println(inline300)
    var t241 Choice = Keep{
        _0: 5,
    }
    var t242 Result__int32__string = choose(t241)
    var t243 string
    switch t242.(type) {
    case Ok:
        var inline292 int32 = t242.(Ok)._0
        var inline294 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline292)
        var inline295 string = "ok " + inline294
        t243 = inline295
    case Err:
        var inline296 string = t242.(Err)._0
        var inline298 string = "err " + inline296
        t243 = inline298
    default:
        panic("non-exhaustive match")
    }
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline289)
    var t244 Choice = Left{
        _0: false,
    }
    var t245 Result__int32__string = choose(t244)
    var t246 string
    switch t245.(type) {
    case Ok:
        var inline281 int32 = t245.(Ok)._0
        var inline283 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline281)
        var inline284 string = "ok " + inline283
        t246 = inline284
    case Err:
        var inline285 string = t245.(Err)._0
        var inline287 string = "err " + inline285
        t246 = inline287
    default:
        panic("non-exhaustive match")
    }
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline278)
    var t247 Choice = Right{
        _0: false,
    }
    var t248 Result__int32__string = choose(t247)
    var t249 string
    switch t248.(type) {
    case Ok:
        var inline270 int32 = t248.(Ok)._0
        var inline272 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline270)
        var inline273 string = "ok " + inline272
        t249 = inline273
    case Err:
        var inline274 string = t248.(Err)._0
        var inline276 string = "err " + inline274
        t249 = inline276
    default:
        panic("non-exhaustive match")
    }
    var inline267 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline267)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t252 string = _goml_runtime_core_int32_to_string(self__33)
    return t252
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
