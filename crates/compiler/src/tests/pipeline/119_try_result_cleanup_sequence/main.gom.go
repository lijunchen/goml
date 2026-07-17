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
    var retv74 Result__Handle__string
    var jp76 Result__Handle__string
    if ok__0 {
        var t77 Handle = Handle{
            name: "config",
        }
        var t78 Result__Handle__string = Result__Handle__string_Ok{
            _0: t77,
        }
        jp76 = t78
    } else {
        var t79 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        jp76 = t79
    }
    retv74 = jp76
    return retv74
}

func close_handle(handle__1 Handle, ok__2 bool) Result__unit__string {
    var retv81 Result__unit__string
    var jp83 Result__unit__string
    if ok__2 {
        var t84 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp83 = t84
    } else {
        var t85 string = handle__1.name
        var t86 string = "close failed for " + t85
        var t87 Result__unit__string = Result__unit__string_Err{
            _0: t86,
        }
        jp83 = t87
    }
    retv81 = jp83
    return retv81
}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var retv89 Result__string__string
    var mtmp61 Result__Handle__string = open_handle(open_ok__3)
    var jp91 Handle
    switch mtmp61.(type) {
    case Result__Handle__string_Ok:
        var x62 Handle = mtmp61.(Result__Handle__string_Ok)._0
        var try_value__24 Handle = x62
        jp91 = try_value__24
        var handle__5 Handle = jp91
        var name__6 string = handle__5.name
        var mtmp64 Result__unit__string = close_handle(handle__5, close_ok__4)
        switch mtmp64.(type) {
        case Result__unit__string_Ok:
            var t93 string = "closed " + name__6
            var t94 Result__string__string = Result__string__string_Ok{
                _0: t93,
            }
            retv89 = t94
            return retv89
        case Result__unit__string_Err:
            var x66 string = mtmp64.(Result__unit__string_Err)._0
            var try_residual__31 string = x66
            var t95 Result__string__string = Result__string__string_Err{
                _0: try_residual__31,
            }
            retv89 = t95
            return retv89
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x63 string = mtmp61.(Result__Handle__string_Err)._0
        var try_residual__24 string = x63
        var t96 Result__string__string = Result__string__string_Err{
            _0: try_residual__24,
        }
        retv89 = t96
        return retv89
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__string__string) string {
    var retv98 string
    var jp100 string
    switch res__7.(type) {
    case Result__string__string_Ok:
        var x68 string = res__7.(Result__string__string_Ok)._0
        var value__8 string = x68
        var t101 string = "ok " + value__8
        jp100 = t101
    case Result__string__string_Err:
        var x69 string = res__7.(Result__string__string_Err)._0
        var err__9 string = x69
        var t102 string = "err " + err__9
        jp100 = t102
    default:
        panic("non-exhaustive match")
    }
    retv98 = jp100
    return retv98
}

func main0() struct{} {
    var t104 Result__string__string = use_handle(true, true)
    var t105 string = show(t104)
    println__T_string(t105)
    var t106 Result__string__string = use_handle(false, true)
    var t107 string = show(t106)
    println__T_string(t107)
    var t108 Result__string__string = use_handle(true, false)
    var t109 string = show(t108)
    println__T_string(t109)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t111 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv114 string
    retv114 = self__37
    return retv114
}

func main() {
    main0()
}
