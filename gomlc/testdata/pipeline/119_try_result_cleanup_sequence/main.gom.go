package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func open_handle(ok__0 bool) Result__Handle__string {
    var retv165 Result__Handle__string
    var jp167 Result__Handle__string
    if ok__0 {
        var t168 Handle = Handle{
            name: "config",
        }
        var t169 Result__Handle__string = Result__Handle__string_Ok{
            _0: t168,
        }
        jp167 = t169
    } else {
        var t170 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp167 = t170
    }
    retv165 = jp167
    return retv165
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv172 Result__unit__string
    var jp174 Result__unit__string
    if ok__2 {
        var t175 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp174 = t175
    } else {
        var t176 string = handle__1.name
        var t177 string = "close failed for " + t176
        var t178 Result__unit__string = Result__unit__string_Err{
            _0: t177,
        }
        jp174 = t178
    }
    retv172 = jp174
    return retv172
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv180 Result__string__string
    var mtmp152 Result__Handle__string = open_handle(open_ok__3)
    var jp182 Handle
    switch mtmp152.(type) {
    case Result__Handle__string_Ok:
        var x153 Handle = mtmp152.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x153
        jp182 = try_value__24
        var handle__5 Handle = jp182
        var name__6 string = handle__5.name
        var mtmp155 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp155.(type) {
        case Result__unit__string_Ok:
            var t184 string = "closed " + name__6
            var t185 Result__string__string = Result__string__string_Ok{
                _0: t184,
            }
            retv180 = t185
            return retv180
        case Result__unit__string_Err:
            var x157 string = mtmp155.(Result__unit__string_Err)._0
            var try_residual__31 string = x157
            var t186 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv180 = t186
            return retv180
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x154 string = mtmp152.(Result__Handle__string_Err)._0
        var try_residual__24 string = x154
        var t187 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv180 = t187
        return retv180
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv189 string
    var jp191 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x159 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x159
        var t192 string = "ok " + value__8
        jp191 = t192
    case Result__string__string_Err:
        var x160 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x160
        var t193 string = "err " + err__9
        jp191 = t193
    default:
        panic("non-exhaustive match")
    }
    retv189 = jp191
    return retv189
}

func main0() struct{} {
    var t195 Result__string__string = use_handle(true, true)
    var t196 string = show(t195)
    println__T_string(t196)
    var t197 Result__string__string = use_handle(false, true)
    var t198 string = show(t197)
    println__T_string(t198)
    var t199 Result__string__string = use_handle(true, false)
    var t200 string = show(t199)
    println__T_string(t200)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t202 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t202)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv205 string
    retv205 = self__38
    return retv205
}

func main() {
    main0()
}
