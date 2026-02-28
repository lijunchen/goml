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

func main0() struct{} {
    var v__0 []int32
    var v__1 []int32
    var v__2 []int32
    var v__3 []int32
    var v__4 []int32
    var s__5 []int32
    var t7 int32
    var _wild0 struct{}
    var t8 int32
    var _wild1 struct{}
    var t9 int32
    var _wild2 struct{}
    var t10 int32
    var _wild3 struct{}
    var t__6 []int32
    var t11 int32
    var _wild4 struct{}
    var t12 int32
    var _wild5 struct{}
    var t13 int32
    var _wild6 struct{}
    _ = _wild0
    _ = _wild1
    _ = _wild2
    _ = _wild3
    _ = _wild4
    _ = _wild5
    _ = _wild6
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            v__0 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
            v__1 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__0, 10)
            v__2 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__1, 20)
            v__3 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__2, 30)
            v__4 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__3, 40)
            s__5 = v__4[1:4]
            t7 = _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(s__5)
            println__T_int32(t7)
            t8 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 0)
            println__T_int32(t8)
            t9 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 1)
            println__T_int32(t9)
            t10 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(s__5, 2)
            println__T_int32(t10)
            t__6 = _goml_inherent_Slice_Slice_x5b_T_x5d__sub__T_int32(s__5, 1, 3)
            t11 = _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(t__6)
            println__T_int32(t11)
            t12 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(t__6, 0)
            println__T_int32(t12)
            t13 = _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(t__6, 1)
            println__T_int32(t13)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var t14 []int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t14 = nil
            return t14
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__66 []int32, elem__67 int32) []int32 {
    var t15 []int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t15 = append(self__66, elem__67)
            return t15
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__len__T_int32(self__73 []int32) int32 {
    var t16 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t16 = int32(len(self__73))
            return t16
        default:
            panic("invalid pc")
        }
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t17 string
    var t18 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t17 = int32_to_string(value__1)
            t18 = string_println(t17)
            return t18
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__get__T_int32(self__71 []int32, index__72 int32) int32 {
    var t19 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t19 = self__71[index__72]
            return t19
        default:
            panic("invalid pc")
        }
    }
}

func _goml_inherent_Slice_Slice_x5b_T_x5d__sub__T_int32(self__74 []int32, start__75 int32, end__76 int32) []int32 {
    var t20 []int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t20 = self__74[start__75:end__76]
            return t20
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
