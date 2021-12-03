package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func a() int {
	var (
		parts []string
		horiz int
		depth int
		val   int
	)

	lines := bufio.NewScanner(os.Stdin)
	for lines.Scan() {
		parts = strings.Split(lines.Text(), " ")
		val, _ = strconv.Atoi(parts[1])

		switch parts[0] {
		case "forward":
			horiz += val
		case "up":
			depth -= val
		case "down":
			depth += val
		}
	}

	return horiz * depth
}

func b() int {
	var (
		parts []string
		horiz int
		depth int
		aim   int
		val   int
	)

	lines := bufio.NewScanner(os.Stdin)
	for lines.Scan() {
		parts = strings.Split(lines.Text(), " ")
		val, _ = strconv.Atoi(parts[1])

		switch parts[0] {
		case "forward":
			horiz += val
			depth += aim * val
		case "up":
			aim -= val
		case "down":
			aim += val
		}
	}

	return horiz * depth
}

func main() {
	fmt.Printf("Part A: %d\n", a())
	os.Stdin.Seek(0, 0)
	fmt.Printf("Part B: %d\n", b())
}
