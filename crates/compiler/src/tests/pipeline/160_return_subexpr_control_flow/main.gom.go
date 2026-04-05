package main

type GoError = error

func main0() struct{} {
    return struct{}{}
}

func main() {
    main0()
}
