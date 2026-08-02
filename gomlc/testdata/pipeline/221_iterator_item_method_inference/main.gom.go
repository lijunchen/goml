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

type Values struct {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func main0() struct{} {
    Loop_loop165:
    for {
        var for_next156 Option__int32
        for_next156 = None{}
        switch for_next156.(type) {
        case None:
            break Loop_loop165
        case Some:
            var x157 int32 = for_next156.(Some)._0
            var t167 string
            var inline174 string = _goml_runtime_core_int32_to_string(x157)
            t167 = inline174
            _goml_runtime_core_string_println(t167)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main() {
    main0()
}
