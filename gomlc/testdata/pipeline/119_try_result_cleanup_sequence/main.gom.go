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
    var retv121 Result__Handle__string
    var jp123 Result__Handle__string
    if ok__0 {
        var t124 Handle = Handle{
            name: "config",
        }
        var t125 Result__Handle__string = Result__Handle__string_Ok{
            _0: t124,
        }
        jp123 = t125
    } else {
        var t126 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp123 = t126
    }
    retv121 = jp123
    return retv121
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv128 Result__unit__string
    var jp130 Result__unit__string
    if ok__2 {
        var t131 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp130 = t131
    } else {
        var t132 string = handle__1.name
        var t133 string = "close failed for " + t132
        var t134 Result__unit__string = Result__unit__string_Err{
            _0: t133,
        }
        jp130 = t134
    }
    retv128 = jp130
    return retv128
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv136 Result__string__string
    var mtmp108 Result__Handle__string = open_handle(open_ok__3)
    var jp138 Handle
    switch mtmp108.(type) {
    case Result__Handle__string_Ok:
        var x109 Handle = mtmp108.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x109
        jp138 = try_value__24
        var handle__5 Handle = jp138
        var name__6 string = handle__5.name
        var mtmp111 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp111.(type) {
        case Result__unit__string_Ok:
            var t140 string = "closed " + name__6
            var t141 Result__string__string = Result__string__string_Ok{
                _0: t140,
            }
            retv136 = t141
            return retv136
        case Result__unit__string_Err:
            var x113 string = mtmp111.(Result__unit__string_Err)._0
            var try_residual__31 string = x113
            var t142 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv136 = t142
            return retv136
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x110 string = mtmp108.(Result__Handle__string_Err)._0
        var try_residual__24 string = x110
        var t143 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv136 = t143
        return retv136
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv145 string
    var jp147 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x115 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x115
        var t148 string = "ok " + value__8
        jp147 = t148
    case Result__string__string_Err:
        var x116 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x116
        var t149 string = "err " + err__9
        jp147 = t149
    default:
        panic("non-exhaustive match")
    }
    retv145 = jp147
    return retv145
}

func main0() struct{} {
    var t151 Result__string__string = use_handle(true, true)
    var t152 string = show(t151)
    println__T_string(t152)
    var t153 Result__string__string = use_handle(false, true)
    var t154 string = show(t153)
    println__T_string(t154)
    var t155 Result__string__string = use_handle(true, false)
    var t156 string = show(t155)
    println__T_string(t156)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t158)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv161 string
    retv161 = self__38
    return retv161
}

func main() {
    main0()
}
