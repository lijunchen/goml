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
    var x0 int32
    var value__2 int32
    var t6 string
    var t7 string
    var t8 string
    switch self__1.(type) {
    case Just:
        goto b2
    case Nothing:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp5
    b2:
    x0 = self__1.(Just)._0
    value__2 = x0
    t6 = int32_to_string(value__2)
    t7 = "Just(" + t6
    t8 = t7 + ")"
    jp5 = t8
    goto b1
    b3:
    jp5 = "Nothing"
    goto b1
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var jp10 Maybe__int32
    var t11 Maybe__int32
    if flag__3 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp10
    b2:
    t11 = Just{
        _0: 42,
    }
    jp10 = t11
    goto b1
    b3:
    jp10 = Nothing{}
    goto b1
}

func main0() struct{} {
    var point__4 Point
    var some_number__5 Maybe__int32
    var none_number__6 Maybe__int32
    var t12 string
    var t13 string
    var t14 string
    point__4 = Point{
        x: 1,
        y: 2,
    }
    some_number__5 = make_maybe(true)
    none_number__6 = make_maybe(false)
    t12 = _goml_trait_impl_Display_Point_show(point__4)
    string_println(t12)
    t13 = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(some_number__5)
    string_println(t13)
    t14 = _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(none_number__6)
    string_println(t14)
    return struct{}{}
}

func main() {
    main0()
}
