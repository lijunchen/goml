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
    var jp208 int32
    switch choice__2.(type) {
    case Left:
        var x177 bool = choice__2.(Left)._0
        var commute_field296 int32
        var commute_field298 string
        if x177 {
            commute_field296 = 10
            jp208 = commute_field296
            var t209 Result__int32__string = Ok{
                _0: jp208,
            }
            return t209
        } else {
            commute_field298 = "left failed"
            var t212 Result__int32__string = Err{
                _0: commute_field298,
            }
            return t212
        }
    case Right:
        var x178 bool = choice__2.(Right)._0
        var mtmp183 Result__int32__string
        if x178 {
            var inline252 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp183 = inline252
        } else {
            var inline253 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp183 = inline253
        }
        var jp214 int32
        switch mtmp183.(type) {
        case Ok:
            var x184 int32 = mtmp183.(Ok)._0
            jp214 = x184
            var t215 int32 = jp214 + 1
            jp208 = t215
            var t209 Result__int32__string = Ok{
                _0: jp208,
            }
            return t209
        case Err:
            var x185 string = mtmp183.(Err)._0
            var t216 Result__int32__string = Err{
                _0: x185,
            }
            return t216
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x179 int32 = choice__2.(Keep)._0
        jp208 = x179
        var t209 Result__int32__string = Ok{
            _0: jp208,
        }
        return t209
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x186 int32 = res__7.(Ok)._0
        var t221 string
        var inline255 string = _goml_runtime_core_int32_to_string(x186)
        t221 = inline255
        var t222 string = "ok " + t221
        return t222
    case Err:
        var x187 string = res__7.(Err)._0
        var t223 string = "err " + x187
        return t223
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t225 Choice = Left{
        _0: true,
    }
    var t226 Result__int32__string = choose(t225)
    var t227 string = show(t226)
    var inline293 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline293)
    var t228 Choice = Right{
        _0: true,
    }
    var t229 Result__int32__string = choose(t228)
    var t230 string = show(t229)
    var inline290 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t230)
    _goml_runtime_core_string_println(inline290)
    var t231 Choice = Keep{
        _0: 5,
    }
    var t232 Result__int32__string = choose(t231)
    var t233 string
    switch t232.(type) {
    case Ok:
        var inline282 int32 = t232.(Ok)._0
        var inline284 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline282)
        var inline285 string = "ok " + inline284
        t233 = inline285
    case Err:
        var inline286 string = t232.(Err)._0
        var inline288 string = "err " + inline286
        t233 = inline288
    default:
        panic("non-exhaustive match")
    }
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t233)
    _goml_runtime_core_string_println(inline279)
    var t234 Choice = Left{
        _0: false,
    }
    var t235 Result__int32__string = choose(t234)
    var t236 string
    switch t235.(type) {
    case Ok:
        var inline271 int32 = t235.(Ok)._0
        var inline273 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline271)
        var inline274 string = "ok " + inline273
        t236 = inline274
    case Err:
        var inline275 string = t235.(Err)._0
        var inline277 string = "err " + inline275
        t236 = inline277
    default:
        panic("non-exhaustive match")
    }
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline268)
    var t237 Choice = Right{
        _0: false,
    }
    var t238 Result__int32__string = choose(t237)
    var t239 string
    switch t238.(type) {
    case Ok:
        var inline260 int32 = t238.(Ok)._0
        var inline262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline260)
        var inline263 string = "ok " + inline262
        t239 = inline263
    case Err:
        var inline264 string = t238.(Err)._0
        var inline266 string = "err " + inline264
        t239 = inline266
    default:
        panic("non-exhaustive match")
    }
    var inline257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline257)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t242 string = _goml_runtime_core_int32_to_string(self__35)
    return t242
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
