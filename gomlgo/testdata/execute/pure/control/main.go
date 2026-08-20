package main

func add(left, right int) int {
	return left + right
}

func main() {
	value := add(1, 2)
	if value == 3 {
		value++
	} else {
		value = 0
	}
	for value < 6 {
		value += 1
	}
	switch value {
	case 6:
		println(value)
	default:
		println(0)
	}
}
