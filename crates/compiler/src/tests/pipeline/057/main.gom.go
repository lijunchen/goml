package main

type S struct {}

func _goml_inherent_S_S_method1(self__0 S) struct{} {
    var ret0 struct{}
    ret0 = struct{}{}
    return ret0
}

func main0() struct{} {
    var ret2 struct{}
    var s__2 S = S{}
    ret2 = _goml_inherent_S_S_method1(s__2)
    return ret2
}

func main() {
    main0()
}
