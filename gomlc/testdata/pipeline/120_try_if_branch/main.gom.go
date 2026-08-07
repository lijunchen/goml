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

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t184 Result__int32__string = Ok{
            _0: 5,
        }
        return t184
    } else {
        var t185 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t185
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp189 int32
    if flag__1 {
        var commute_field262 int32
        var commute_field264 string
        if fallback__2 {
            commute_field262 = 5
            jp189 = commute_field262
            var t190 int32 = jp189 + 1
            var t191 Result__int32__string = Ok{
                _0: t190,
            }
            return t191
        } else {
            commute_field264 = "bad-branch"
            var t194 Result__int32__string = Err{
                _0: commute_field264,
            }
            return t194
        }
    } else {
        jp189 = 10
        var t190 int32 = jp189 + 1
        var t191 Result__int32__string = Ok{
            _0: t190,
        }
        return t191
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x175 int32 = res__4.(Ok)._0
        var t199 string
        var inline221 string = _goml_runtime_core_int32_to_string(x175)
        t199 = inline221
        var t200 string = "ok=" + t199
        return t200
    case Err:
        var x176 string = res__4.(Err)._0
        var t201 string = "err=" + x176
        return t201
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t203 Result__int32__string = bump(true, true)
    var t204 string = show(t203)
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline259)
    var t205 Result__int32__string = bump(true, false)
    var t206 string
    switch t205.(type) {
    case Ok:
        var inline251 int32 = t205.(Ok)._0
        var inline253 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline251)
        var inline254 string = "ok=" + inline253
        t206 = inline254
    case Err:
        var inline255 string = t205.(Err)._0
        var inline257 string = "err=" + inline255
        t206 = inline257
    default:
        panic("non-exhaustive match")
    }
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline248)
    var t207 Result__int32__string
    var inline234 bool = false
    var inline235 bool = false
    var inline237 int32
    if inline234 {
        var inline241 Result__int32__string = parse(inline235)
        switch inline241.(type) {
        case Ok:
            var inline242 int32 = inline241.(Ok)._0
            inline237 = inline242
            var inline239 int32 = inline237 + 1
            var inline240 Result__int32__string = Ok{
                _0: inline239,
            }
            t207 = inline240
            var t208 string
            switch t207.(type) {
            case Ok:
                var inline226 int32 = t207.(Ok)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "ok=" + inline228
                t208 = inline229
            case Err:
                var inline230 string = t207.(Err)._0
                var inline232 string = "err=" + inline230
                t208 = inline232
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        case Err:
            var inline244 string = inline241.(Err)._0
            var inline246 Result__int32__string = Err{
                _0: inline244,
            }
            t207 = inline246
            var t208 string
            switch t207.(type) {
            case Ok:
                var inline226 int32 = t207.(Ok)._0
                var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
                var inline229 string = "ok=" + inline228
                t208 = inline229
            case Err:
                var inline230 string = t207.(Err)._0
                var inline232 string = "err=" + inline230
                t208 = inline232
            default:
                panic("non-exhaustive match")
            }
            var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
            _goml_runtime_core_string_println(inline223)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline237 = 10
        var inline239 int32 = inline237 + 1
        var inline240 Result__int32__string = Ok{
            _0: inline239,
        }
        t207 = inline240
        var t208 string
        switch t207.(type) {
        case Ok:
            var inline226 int32 = t207.(Ok)._0
            var inline228 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
            var inline229 string = "ok=" + inline228
            t208 = inline229
        case Err:
            var inline230 string = t207.(Err)._0
            var inline232 string = "err=" + inline230
            t208 = inline232
        default:
            panic("non-exhaustive match")
        }
        var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
        _goml_runtime_core_string_println(inline223)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__35)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
