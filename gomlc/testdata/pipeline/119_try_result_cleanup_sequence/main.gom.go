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
    var retv168 Result__Handle__string
    var jp170 Result__Handle__string
    if ok__0 {
        var t171 Handle = Handle{
            name: "config",
        }
        var t172 Result__Handle__string = Result__Handle__string_Ok{
            _0: t171,
        }
        jp170 = t172
    } else {
        var t173 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp170 = t173
    }
    retv168 = jp170
    return retv168
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv175 Result__unit__string
    var jp177 Result__unit__string
    if ok__2 {
        var t178 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp177 = t178
    } else {
        var t179 string = handle__1.name
        var t180 string = "close failed for " + t179
        var t181 Result__unit__string = Result__unit__string_Err{
            _0: t180,
        }
        jp177 = t181
    }
    retv175 = jp177
    return retv175
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv183 Result__string__string
    var mtmp155 Result__Handle__string = open_handle(open_ok__3)
    var jp185 Handle
    switch mtmp155.(type) {
    case Result__Handle__string_Ok:
        var x156 Handle = mtmp155.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x156
        jp185 = try_value__24
        var handle__5 Handle = jp185
        var name__6 string = handle__5.name
        var mtmp158 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp158.(type) {
        case Result__unit__string_Ok:
            var t187 string = "closed " + name__6
            var t188 Result__string__string = Result__string__string_Ok{
                _0: t187,
            }
            retv183 = t188
            return retv183
        case Result__unit__string_Err:
            var x160 string = mtmp158.(Result__unit__string_Err)._0
            var try_residual__31 string = x160
            var t189 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv183 = t189
            return retv183
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x157 string = mtmp155.(Result__Handle__string_Err)._0
        var try_residual__24 string = x157
        var t190 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv183 = t190
        return retv183
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv192 string
    var jp194 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x162 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x162
        var t195 string = "ok " + value__8
        jp194 = t195
    case Result__string__string_Err:
        var x163 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x163
        var t196 string = "err " + err__9
        jp194 = t196
    default:
        panic("non-exhaustive match")
    }
    retv192 = jp194
    return retv192
}

func main0() struct{} {
    var t198 Result__string__string = use_handle(true, true)
    var t199 string = show(t198)
    println__T_string(t199)
    var t200 Result__string__string = use_handle(false, true)
    var t201 string = show(t200)
    println__T_string(t201)
    var t202 Result__string__string = use_handle(true, false)
    var t203 string = show(t202)
    println__T_string(t203)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t205 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv208 string
    retv208 = self__38
    return retv208
}

func main() {
    main0()
}
