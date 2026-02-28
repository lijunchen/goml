package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

type S struct {
    value int32
}

type dyn__ToString_vtable struct {
    to_string func(any) string
}

type dyn__ToString struct {
    data any
    vtable *dyn__ToString_vtable
}

func dyn__ToString__wrap__S__to_string(self any) string {
    return _goml_trait_impl_ToString_S_to_string(self.(S))
}

func dyn__ToString__vtable__S() *dyn__ToString_vtable {
    return &dyn__ToString_vtable{
        to_string: dyn__ToString__wrap__S__to_string,
    }
}

func _goml_trait_impl_ToString_S_to_string(self__0 S) string {
    var t11 int32
    var t12 string
    var t13 string
    var t14 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = self__0.value
            t12 = int32_to_string(t11)
            t13 = "S(" + t12
            t14 = t13 + ")"
            return t14
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var _wild0 struct{}
    var _wild1 struct{}
    var _wild2 struct{}
    var _wild3 struct{}
    var t15 string
    var _wild4 struct{}
    var t16 string
    var _wild5 struct{}
    var s__1 S
    var _wild6 struct{}
    var d__2 dyn__ToString
    var _wild7 struct{}
    var r__3 *ref_int32_x
    var _wild8 struct{}
    var _wild9 struct{}
    var _wild10 struct{}
    _ = _wild0
    _ = _wild1
    _ = _wild2
    _ = _wild3
    _ = _wild4
    _ = _wild5
    _ = _wild6
    _ = _wild7
    _ = _wild8
    _ = _wild9
    _ = _wild10
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            println__T_int32(1)
            println__T_bool(true)
            println__T_string("hi")
            println__T_unit(struct{}{})
            t15 = int32_to_string(2)
            println__T_string(t15)
            t16 = int32_to_string(2)
            println__T_string(t16)
            s__1 = S{
                value: 9,
            }
            println__T_S(s__1)
            d__2 = dyn__ToString{
                data: s__1,
                vtable: dyn__ToString__vtable__S(),
            }
            println__T_dynToString(d__2)
            r__3 = ref__Ref_int32(5)
            _goml_println__T_Ref_x5b_int32_x5d_(r__3)
            print__T_string("no-newline")
            println__T_string("!")
            return struct{}{}
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

func println__T_bool(value__1 bool) struct{} {
    var t19 string
    var t20 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t19 = bool_to_string(value__1)
            t20 = string_println(t19)
            return t20
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t21 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t21 = string_println(value__1)
            return t21
        default:
            panic("invalid pc")
        }
    }
}

func println__T_unit(value__1 struct{}) struct{} {
    var t22 string
    var t23 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t22 = unit_to_string(value__1)
            t23 = string_println(t22)
            return t23
        default:
            panic("invalid pc")
        }
    }
}

func println__T_S(value__1 S) struct{} {
    var t24 string
    var t25 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t24 = _goml_trait_impl_ToString_S_to_string(value__1)
            t25 = string_println(t24)
            return t25
        default:
            panic("invalid pc")
        }
    }
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t26 string
    var t27 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t26 = value__1.vtable.to_string(value__1.data)
            t27 = string_println(t26)
            return t27
        default:
            panic("invalid pc")
        }
    }
}

func _goml_println__T_Ref_x5b_int32_x5d_(value__1 *ref_int32_x) struct{} {
    var t28 int32
    var t29 string
    var t30 string
    var t31 string
    var t32 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t28 = ref_get__Ref_int32(value__1)
            t29 = int32_to_string(t28)
            t30 = "ref(" + t29
            t31 = t30 + ")"
            t32 = string_println(t31)
            return t32
        default:
            panic("invalid pc")
        }
    }
}

func print__T_string(value__0 string) struct{} {
    var t33 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t33 = string_print(value__0)
            return t33
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
