package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Point struct {
    x int32
    y int32
}

type Option__int32 interface {
    isOption__int32()
}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

type None struct {}

func (_ None) isOption__int32() {}

func impl_Display_Point_show(self__0 Point) string {
    var ret9 string
    ret9 = "Point"
    return ret9
}

func impl_Display_Option_int32_show(self__1 Option__int32) string {
    var ret10 string
    switch self__1 := self__1.(type) {
    case Some:
        var x0 int32 = self__1._0
        var value__2 int32 = x0
        var t5 string = int32_to_string(value__2)
        var t4 string = "Some(" + t5
        ret10 = t4 + ")"
    case None:
        ret10 = "None"
    }
    return ret10
}

func make_optional(flag__3 bool) Option__int32 {
    var ret11 Option__int32
    if flag__3 {
        ret11 = Some{
            _0: 42,
        }
    } else {
        ret11 = None{}
    }
    return ret11
}

func main0() struct{} {
    var ret12 struct{}
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Option__int32 = make_optional(true)
    var none_number__6 Option__int32 = make_optional(false)
    var t6 string = impl_Display_Point_show(point__4)
    string_println(t6)
    var t7 string = impl_Display_Option_int32_show(some_number__5)
    string_println(t7)
    var t8 string = impl_Display_Option_int32_show(none_number__6)
    string_println(t8)
    ret12 = struct{}{}
    return ret12
}

func main() {
    main0()
}
