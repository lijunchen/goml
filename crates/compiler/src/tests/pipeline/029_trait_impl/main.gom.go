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
    var ret13 string
    ret13 = "Point"
    return ret13
}

func _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(self__1 Maybe__int32) string {
    var ret14 string
    switch self__1 := self__1.(type) {
    case Just:
        var x0 int32 = self__1._0
        var value__2 int32 = x0
        var t9 string = int32_to_string(value__2)
        var t8 string = "Just(" + t9
        ret14 = t8 + ")"
    case Nothing:
        ret14 = "Nothing"
    }
    return ret14
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var ret15 Maybe__int32
    if flag__3 {
        ret15 = Just{
            _0: 42,
        }
    } else {
        ret15 = Nothing{}
    }
    return ret15
}

func main0() struct{} {
    var ret16 struct{}
    var point__4 Point = Point{
        x: 1,
        y: 2,
    }
    var some_number__5 Maybe__int32 = make_maybe(true)
    var none_number__6 Maybe__int32 = make_maybe(false)
    var t10 string = _goml_trait_impl_Display_Point_show(point__4)
    string_println(t10)
    var t11 string = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(some_number__5)
    string_println(t11)
    var t12 string = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(none_number__6)
    string_println(t12)
    ret16 = struct{}{}
    return ret16
}

func main() {
    main0()
}
