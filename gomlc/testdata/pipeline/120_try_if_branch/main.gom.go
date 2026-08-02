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
        var t167 Result__int32__string = Ok{
            _0: 5,
        }
        return t167
    } else {
        var t168 Result__int32__string = Err{
            _0: "bad-branch",
        }
        return t168
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp172 int32
    if flag__1 {
        var mtmp155 Result__int32__string
        if fallback__2 {
            var inline201 Result__int32__string = Ok{
                _0: 5,
            }
            mtmp155 = inline201
        } else {
            var inline202 Result__int32__string = Err{
                _0: "bad-branch",
            }
            mtmp155 = inline202
        }
        switch mtmp155.(type) {
        case Ok:
            var x156 int32 = mtmp155.(Ok)._0
            jp172 = x156
            var t173 int32 = jp172 + 1
            var t174 Result__int32__string = Ok{
                _0: t173,
            }
            return t174
        case Err:
            var x157 string = mtmp155.(Err)._0
            var t177 Result__int32__string = Err{
                _0: x157,
            }
            return t177
        default:
            panic("non-exhaustive match")
        }
    } else {
        jp172 = 10
        var t173 int32 = jp172 + 1
        var t174 Result__int32__string = Ok{
            _0: t173,
        }
        return t174
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4.(type) {
    case Ok:
        var x158 int32 = res__4.(Ok)._0
        var t182 string
        var inline204 string = _goml_runtime_core_int32_to_string(x158)
        t182 = inline204
        var t183 string = "ok=" + t182
        return t183
    case Err:
        var x159 string = res__4.(Err)._0
        var t184 string = "err=" + x159
        return t184
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t186 Result__int32__string = bump(true, true)
    var t187 string = show(t186)
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline242)
    var t188 Result__int32__string = bump(true, false)
    var t189 string
    switch t188.(type) {
    case Ok:
        var inline234 int32 = t188.(Ok)._0
        var inline236 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline234)
        var inline237 string = "ok=" + inline236
        t189 = inline237
    case Err:
        var inline238 string = t188.(Err)._0
        var inline240 string = "err=" + inline238
        t189 = inline240
    default:
        panic("non-exhaustive match")
    }
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline231)
    var t190 Result__int32__string
    var inline217 bool = false
    var inline218 bool = false
    var inline220 int32
    if inline217 {
        var inline224 Result__int32__string = parse(inline218)
        switch inline224.(type) {
        case Ok:
            var inline225 int32 = inline224.(Ok)._0
            inline220 = inline225
            var inline222 int32 = inline220 + 1
            var inline223 Result__int32__string = Ok{
                _0: inline222,
            }
            t190 = inline223
            var t191 string
            switch t190.(type) {
            case Ok:
                var inline209 int32 = t190.(Ok)._0
                var inline211 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline209)
                var inline212 string = "ok=" + inline211
                t191 = inline212
            case Err:
                var inline213 string = t190.(Err)._0
                var inline215 string = "err=" + inline213
                t191 = inline215
            default:
                panic("non-exhaustive match")
            }
            var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
            _goml_runtime_core_string_println(inline206)
            return struct{}{}
        case Err:
            var inline227 string = inline224.(Err)._0
            var inline229 Result__int32__string = Err{
                _0: inline227,
            }
            t190 = inline229
            var t191 string
            switch t190.(type) {
            case Ok:
                var inline209 int32 = t190.(Ok)._0
                var inline211 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline209)
                var inline212 string = "ok=" + inline211
                t191 = inline212
            case Err:
                var inline213 string = t190.(Err)._0
                var inline215 string = "err=" + inline213
                t191 = inline215
            default:
                panic("non-exhaustive match")
            }
            var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
            _goml_runtime_core_string_println(inline206)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline220 = 10
        var inline222 int32 = inline220 + 1
        var inline223 Result__int32__string = Ok{
            _0: inline222,
        }
        t190 = inline223
        var t191 string
        switch t190.(type) {
        case Ok:
            var inline209 int32 = t190.(Ok)._0
            var inline211 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline209)
            var inline212 string = "ok=" + inline211
            t191 = inline212
        case Err:
            var inline213 string = t190.(Err)._0
            var inline215 string = "err=" + inline213
            t191 = inline215
        default:
            panic("non-exhaustive match")
        }
        var inline206 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
        _goml_runtime_core_string_println(inline206)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t194 string = _goml_runtime_core_int32_to_string(self__6)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
