package main

type Item struct {
	value int
}

type Pair struct {
	left  int
	right int
}

type Pointer struct {
	value int
}

type Number interface {
	Number() int
}

type Count struct {
	value int
}

func (count Count) Number() int {
	return count.value
}

var evaluations int

func source(value any) any {
	evaluations++
	return value
}

func classify(value any) {
	switch selected := source(value).(type) {
	case nil:
		println("nil", selected == nil, evaluations)
	case Item:
		println("item", selected.value, evaluations)
	default:
		println("default", selected != nil, evaluations)
	case Number:
		println("number", selected.Number(), evaluations)
	case Count:
		println("bad count", selected.value, evaluations)
	case Pair, *Pointer:
		_, pair := selected.(Pair)
		pointer, pointerOK := selected.(*Pointer)
		println("multi", pair, pointerOK, selected == nil, pointer == nil, evaluations)
	}
}

func main() {
	var empty any
	classify(empty)
	classify(Item{value: 7})
	classify(Count{value: 8})
	classify(Pair{left: 1, right: 2})
	var pointer *Pointer
	classify(pointer)
	classify("other")

	switch source(Item{value: 9}).(type) {
	case Item:
		println("plain", evaluations)
		break
	default:
		println("bad plain")
	}

	switch before := evaluations; selected := source(Pair{left: 3, right: 4}).(type) {
	case Pair, *Pointer:
		_, pair := selected.(Pair)
		println("init", before, evaluations, pair)
	}

outer:
	switch selected := source(Item{value: 10}).(type) {
	case Item:
		println("labeled", selected.value, evaluations)
		break outer
	default:
		println("bad labeled")
	}
	println("done", evaluations)
	switch selected := source(pointer).(type) {
	case *Pointer:
		println("typed-nil", selected == nil, evaluations)
	}

	functions := []func() int{}
	for index := 1; index <= 2; index++ {
		var value any = Item{value: index}
		switch selected := value.(type) {
		case Item:
			functions = append(functions, func() int { return selected.value })
		}
	}
	println("captures", functions[0](), functions[1]())
}
