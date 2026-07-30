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
    var retv81 Result__Handle__string
    var jp83 Result__Handle__string
    if ok__0 {
        var t84 Handle = Handle{
            name: "config",
        }
        var t85 Result__Handle__string = Result__Handle__string_Ok{
            _0: t84,
        }
        jp83 = t85
    } else {
        var t86 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp83 = t86
    }
    retv81 = jp83
    return retv81
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv88 Result__unit__string
    var jp90 Result__unit__string
    if ok__2 {
        var t91 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp90 = t91
    } else {
        var t92 string = handle__1.name
        var t93 string = "close failed for " + t92
        var t94 Result__unit__string = Result__unit__string_Err{
            _0: t93,
        }
        jp90 = t94
    }
    retv88 = jp90
    return retv88
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv96 Result__string__string
    var mtmp68 Result__Handle__string = open_handle(open_ok__3)
    var jp98 Handle
    switch mtmp68.(type) {
    case Result__Handle__string_Ok:
        var x69 Handle = mtmp68.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x69
        jp98 = try_value__24
        var handle__5 Handle = jp98
        var name__6 string = handle__5.name
        var mtmp71 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp71.(type) {
        case Result__unit__string_Ok:
            var t100 string = "closed " + name__6
            var t101 Result__string__string = Result__string__string_Ok{
                _0: t100,
            }
            retv96 = t101
            return retv96
        case Result__unit__string_Err:
            var x73 string = mtmp71.(Result__unit__string_Err)._0
            var try_residual__31 string = x73
            var t102 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv96 = t102
            return retv96
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x70 string = mtmp68.(Result__Handle__string_Err)._0
        var try_residual__24 string = x70
        var t103 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv96 = t103
        return retv96
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv105 string
    var jp107 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x75 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x75
        var t108 string = "ok " + value__8
        jp107 = t108
    case Result__string__string_Err:
        var x76 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x76
        var t109 string = "err " + err__9
        jp107 = t109
    default:
        panic("non-exhaustive match")
    }
    retv105 = jp107
    return retv105
}

func main0() struct{} {
    var t111 Result__string__string = use_handle(true, true)
    var t112 string = show(t111)
    println__T_string(t112)
    var t113 Result__string__string = use_handle(false, true)
    var t114 string = show(t113)
    println__T_string(t114)
    var t115 Result__string__string = use_handle(true, false)
    var t116 string = show(t115)
    println__T_string(t116)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
