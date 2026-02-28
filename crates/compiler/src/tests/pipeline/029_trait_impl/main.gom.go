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
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "Point"
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Display_Maybe_x5b_int32_x5d__show(self__1 Maybe__int32) string {
    var jp5 string
    var x0 int32
    var value__2 int32
    var t6 string
    var t7 string
    var t8 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch self__1.(type) {
            case Just:
                pc = 2
            case Nothing:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp5
        case 2:
            x0 = self__1.(Just)._0
            value__2 = x0
            t6 = int32_to_string(value__2)
            t7 = "Just(" + t6
            t8 = t7 + ")"
            jp5 = t8
            pc = 1
        case 3:
            jp5 = "Nothing"
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func make_maybe(flag__3 bool) Maybe__int32 {
    var jp10 Maybe__int32
    var t11 Maybe__int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            if flag__3 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp10
        case 2:
            t11 = Just{
                _0: 42,
            }
            jp10 = t11
            pc = 1
        case 3:
            jp10 = Nothing{}
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var point__4 Point
    var some_number__5 Maybe__int32
    var none_number__6 Maybe__int32
    var t12 string
    var mtmp1 struct{}
    var t13 string
    var mtmp2 struct{}
    var t14 string
    var mtmp3 struct{}
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
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
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
