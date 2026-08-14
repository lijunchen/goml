package main

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

func main0() int32 {
    var value__0 *ref_int32_x
    var inline211 int32 = 1
    var inline212 *ref_int32_x = ref__Ref_5int32(inline211)
    value__0 = inline212
    var inline208 int32 = 2
    ref_set__Ref_5int32(value__0, inline208)
    var inline206 int32 = ref_get__Ref_5int32(value__0)
    return inline206
}

func main() {
    main0()
}
