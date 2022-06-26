import munit.FunSuite
import MyList.*

class MyListSuite extends FunSuite {
  test("dropWhile on Nil") {
    val expected = MyNil
    val actual = dropWhile(MyNil, _ => false)
    assertEquals(actual, expected)
  }
  test("dropWhile on Cons") {
    val expected = MyList(3, 4, 5)
    val actual = dropWhile(MyList(1, 2, 3, 4, 5), _ < 3)
    assertEquals(actual, expected)
  }
  test("dropWhile on true") {
    val expected = MyNil
    val actual = dropWhile(MyList(1, 2, 3, 4, 5), _ => true)
    assertEquals(actual, expected)
  }
  test("dropWhile on false") {
    val list = MyList(1, 2, 3, 4, 5)
    val expected = list
    val actual = dropWhile(list, _ => false)
    assertEquals(actual, expected)
  }
  test("takeWhile on Nil") {
    val expected = MyNil
    val actual = takeWhile(MyNil, _ => true)
    assertEquals(actual, expected)
  }
  test("takeWhile on Cons") {
    val expected = MyList(1, 2, 3)
    val actual = takeWhile(MyList(1, 2, 3, 4, 5), _ < 4)
    assertEquals(actual, expected)
  }
  test("takeWhile on true") {
    val list = MyList(1, 2, 3, 4, 5)
    val expected = list
    val actual = takeWhile(list, _ => true)
    assertEquals(actual, expected)
  }
  test("takeWhile on false") {
    val expected = MyNil
    val actual = takeWhile(MyList(1, 2, 3, 4, 5), _ => false)
    assertEquals(actual, expected)
  }
  test("subsequences on Nil") {
    val expected = MyList(MyNil)
    val actual = subsequences(MyNil)
    assertEquals(actual, expected)
  }
  test("subsequences on Cons") {
    val expected = MyList(
      MyList(),
      MyList(1),
      MyList(2),
      MyList(3),
      MyList(1, 2),
      MyList(1, 3),
      MyList(2, 3),
      MyList(1, 2, 3),
    )
    val actual = subsequences(MyList(1, 2, 3))
    assertEquals(actual, expected)
  }
  test("corresponds on Nils") {
    val expected = true
    val actual = corresponds(
      MyNil,
      MyNil,
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on equal Cons") {
    val list = MyList(1, 2, 3, 4, 5)
    val expected = true
    val actual = corresponds(
      list,
      list,
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on different Cons with equal length") {
    val expected = false
    val actual = corresponds(
      MyList(1, 2, 3, 4, 5),
      MyList(1, 2, 3, 4, 7),
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on different Cons with different length (1 > 2)") {
    val expected = false
    val actual = corresponds(
      MyList(1, 2, 3, 4, 5),
      MyList(1, 2, 3, 4),
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on different Cons with different length (1 < 2)") {
    val expected = false
    val actual = corresponds(
      MyList(1, 2, 3, 4),
      MyList(1, 2, 3, 4, 5),
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on Nil with Cons") {
    val expected = false
    val actual = corresponds(
      MyNil,
      MyList(1, 2, 3, 4, 5),
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
  test("corresponds on Cons with Nil") {
    val expected = false
    val actual = corresponds(
      MyList(1, 2, 3, 4, 5),
      MyNil,
      (a, b) => a == b
    )
    assertEquals(actual, expected)
  }
}