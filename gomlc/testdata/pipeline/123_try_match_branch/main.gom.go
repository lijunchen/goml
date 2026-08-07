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
    var jp203 int32
    switch choice__2.(type) {
    case Left:
        var x172 bool = choice__2.(Left)._0
        var commute_field291 int32
        var commute_field293 string
        if x172 {
            commute_field291 = 10
            jp203 = commute_field291
            var t204 Result__int32__string = Ok{
                _0: jp203,
            }
            return t204
        } else {
            commute_field293 = "left failed"
            var t207 Result__int32__string = Err{
                _0: commute_field293,
            }
            return t207
        }
    case Right:
        var x173 bool = choice__2.(Right)._0
        var mtmp178 Result__int32__string
        if x173 {
            var inline247 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp178 = inline247
        } else {
            var inline248 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp178 = inline248
        }
        var jp209 int32
        switch mtmp178.(type) {
        case Ok:
            var x179 int32 = mtmp178.(Ok)._0
            jp209 = x179
            var t210 int32 = jp209 + 1
            jp203 = t210
            var t204 Result__int32__string = Ok{
                _0: jp203,
            }
            return t204
        case Err:
            var x180 string = mtmp178.(Err)._0
            var t211 Result__int32__string = Err{
                _0: x180,
            }
            return t211
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x174 int32 = choice__2.(Keep)._0
        jp203 = x174
        var t204 Result__int32__string = Ok{
            _0: jp203,
        }
        return t204
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x181 int32 = res__7.(Ok)._0
        var t216 string
        var inline250 string = _goml_runtime_core_int32_to_string(x181)
        t216 = inline250
        var t217 string = "ok " + t216
        return t217
    case Err:
        var x182 string = res__7.(Err)._0
        var t218 string = "err " + x182
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t220 Choice = Left{
        _0: true,
    }
    var t221 Result__int32__string = choose(t220)
    var t222 string = show(t221)
    var inline288 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline288)
    var t223 Choice = Right{
        _0: true,
    }
    var t224 Result__int32__string = choose(t223)
    var t225 string = show(t224)
    var inline285 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline285)
    var t226 Choice = Keep{
        _0: 5,
    }
    var t227 Result__int32__string = choose(t226)
    var t228 string
    switch t227.(type) {
    case Ok:
        var inline277 int32 = t227.(Ok)._0
        var inline279 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline277)
        var inline280 string = "ok " + inline279
        t228 = inline280
    case Err:
        var inline281 string = t227.(Err)._0
        var inline283 string = "err " + inline281
        t228 = inline283
    default:
        panic("non-exhaustive match")
    }
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline274)
    var t229 Choice = Left{
        _0: false,
    }
    var t230 Result__int32__string = choose(t229)
    var t231 string
    switch t230.(type) {
    case Ok:
        var inline266 int32 = t230.(Ok)._0
        var inline268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline266)
        var inline269 string = "ok " + inline268
        t231 = inline269
    case Err:
        var inline270 string = t230.(Err)._0
        var inline272 string = "err " + inline270
        t231 = inline272
    default:
        panic("non-exhaustive match")
    }
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t231)
    _goml_runtime_core_string_println(inline263)
    var t232 Choice = Right{
        _0: false,
    }
    var t233 Result__int32__string = choose(t232)
    var t234 string
    switch t233.(type) {
    case Ok:
        var inline255 int32 = t233.(Ok)._0
        var inline257 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline255)
        var inline258 string = "ok " + inline257
        t234 = inline258
    case Err:
        var inline259 string = t233.(Err)._0
        var inline261 string = "err " + inline259
        t234 = inline261
    default:
        panic("non-exhaustive match")
    }
    var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline252)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t237 string = _goml_runtime_core_int32_to_string(self__35)
    return t237
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
