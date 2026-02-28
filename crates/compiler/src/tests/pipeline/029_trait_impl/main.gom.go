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
    return "Point"
}

func _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(self__1 Maybe__int32) string {
    var jp5 string
    switch self__1.(type) {
    case Just:
        var x0 int32 = self__1.(Just)._0
        var value__2 int32 = x0
        var t6 string = int32_to_string(value__2)
        var t7 string = "Just(" + t6
        var t8 string = t7 + ")"
        jp5 = t8
    case Nothing:
        jp5 = "Nothing"
    default:
        panic("non-exhaustive match")
    }
    return jp5
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var jp10 Maybe__int32
    if flag__3 {
        var t11 Maybe__int32 = Just{
            _0: 42,
        }
        jp10 = t11
    } else {
        jp10 = Nothing{}
    }
    return jp10
}

func main0() struct{} {
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t12 string = _goml_trait_impl_Display_Point_show(point__4)
    string_println(t12)
    var t13 string = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(some_number__5)
    string_println(t13)
    var t14 string = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(none_number__6)
    string_println(t14)
    return struct{}{}
}

func main() {
    main0()
}
