package reflection

import (
	"strconv"
	"strings"

	"github.com/oleiade/gomme"
)

type Book struct {
	Name         string `json:"name"`
	Page_count   int32  `json:"page_count"`
	Rating       int32  `json:"rating"`
	Release_year int32  `json:"release_year"`
}

var numberParser = gomme.Map(gomme.Digit1[string](), func(s string) (int32, error) {
	res, err := strconv.ParseInt(string(s), 10, 32)
	if err != nil {
		return 0, err
	}
	return int32(res), nil
})

var t1 = "\"name\":\""
var t2 = "\"page_count\":"
var t3 = "\"rating\":"
var t4 = "\"release_year\":"

func ParseBook(s string) (Book, string) {
	book := Book{}
	var mask = 0b1111
	s = s[1:]
	for mask != 0 {
		if strings.HasPrefix(s, t1) {
			res := gomme.Many0(gomme.Satisfy[string](func(r rune) bool { return (r != '"') }))(s[len(t1):])
			book.Name = string(res.Output)
			s = res.Remaining[1:] // Skipping closing quote
			mask &^= (1 << 0)
		}
		if strings.HasPrefix(s, t2) {
			res := numberParser(s[len(t2):])
			book.Page_count = res.Output
			s = res.Remaining
			mask &^= (1 << 1)
		}
		if strings.HasPrefix(s, t3) {
			res := numberParser(s[len(t3):])
			book.Rating = res.Output
			s = res.Remaining
			mask &^= (1 << 2)
		}
		if strings.HasPrefix(s, t4) {
			res := numberParser(s[len(t4):])
			book.Release_year = res.Output
			s = res.Remaining
			mask &^= (1 << 3)
		}
		if mask != 0 {
			s = s[1:]
		}
	}
	return book, s[1:]
}

func ParseBooks(s string) ([]Book, string) {

	books := []Book{}
	rest := s[1:]
	for {
		var book Book
		book, rest = ParseBook(rest)
		books = append(books, book)
		if rest[0] != ',' {
			break
		}

		rest = rest[1:]
	}
	return books, rest[1:]
}
