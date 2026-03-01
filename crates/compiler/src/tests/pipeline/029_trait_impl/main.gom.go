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

type Maybe__int32 interface {
    isMaybe__int32()
}

type Just struct {
    _0 int32
}

func (_ Just) isMaybe__int32() {}

type Nothing struct {}

func (_ Nothing) isMaybe__int32() {}

func _goml_trait_impl_Display_Point_show(self__0 Point) string {
    var retv5 string
    retv5 = "Point"
    return retv5
}

func _goml_trait_impl_Display_Maybe__int32_show(self__1 Maybe__int32) string {
    var retv7 string
    var jp9 string
    switch self__1.(type) {
    case Just:
        var x0 int32 = self__1.(Just)._0
        var value__2 int32 = x0
        var t10 string = int32_to_string(value__2)
        var t11 string = "Just(" + t10
        var t12 string = t11 + ")"
        jp9 = t12
    case Nothing:
        jp9 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    retv7 = jp9
    return retv7
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var retv14 Maybe__int32
    var jp16 Maybe__int32
    if flag__3 {
        var t17 Maybe__int32 = Just{
            _0: 42,
        }
        jp16 = t17
    } else {
        jp16 = Nothing{}
    }
    retv14 = jp16
    return retv14
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t19 string = _goml_trait_impl_Display_Point_show(point__4)
    string_println(t19)
    var t20 string = _goml_trait_impl_Display_Maybe__int32_show(some_number__5)
    string_println(t20)
    var t21 string = _goml_trait_impl_Display_Maybe__int32_show(none_number__6)
    string_println(t21)
    return struct{}{}
}

func main() {
    main0()
}
