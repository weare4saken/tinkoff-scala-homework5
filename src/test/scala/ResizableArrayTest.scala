import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ResizableArrayTest extends AnyFlatSpec with Matchers {
  "A ResizableArray" should "allow appending elements and return a new collection" in {
    val emptyResizableArray = ResizableArray.empty[String]
    val resizableArray1 = emptyResizableArray.appended("Hello")
    val resizableArray2 = resizableArray1.appended("World")

    emptyResizableArray should have size 0
    resizableArray1 should have size 1
    resizableArray2 should have size 2

    resizableArray1(0) shouldBe "Hello"
    resizableArray2(1) shouldBe "World"
  }

  it should "allow elements to be updated within the bounds" in {
    val resizableArray = ResizableArray.from(List(1, 2, 3))
    resizableArray.update(1, 4)

    resizableArray(1) shouldBe 4
  }

  it should "throw an IndexOutOfBoundsException when updating out of bounds" in {
    val resizableArray = ResizableArray.empty[Int]
    an[IndexOutOfBoundsException] should be thrownBy resizableArray.update(0, 1)
  }

  it should "clear the collection and return a new empty collection" in {
    val resizableArray = ResizableArray.from(List(1, 2, 3))
    val clearedResizableArray = resizableArray.clear()

    resizableArray should have size 3
    clearedResizableArray should have size 0
  }

  it should "remove element at specified index and return a new collection" in {
    val resizableArray = ResizableArray.from(List(1, 2, 3, 4, 5))
    val newResizableArray = resizableArray.removeAt(2)

    newResizableArray shouldBe ResizableArray.from(List(1, 2, 4, 5))
  }

  it should "concatenate two arrays and return a new collection" in {
    val resizableArray1 = ResizableArray.from(List(1, 2, 3))
    val resizableArray2 = ResizableArray.from(List(4, 5, 6))
    val concatenatedResizableArray = resizableArray1.concat(resizableArray2)

    concatenatedResizableArray shouldBe ResizableArray.from(List(1, 2, 3, 4, 5, 6))
  }

  it should "filter elements based on predicate and return a new collection" in {
    val resizableArray = ResizableArray.from(List(1, 2, 3, 4, 5))
    val filteredResizableArray = resizableArray.filter(_ % 2 == 0)

    filteredResizableArray shouldBe ResizableArray.from(List(2, 4))
  }

  it should "sort elements and return a new collection" in {
    val resizableArray = ResizableArray.from(List(3, 1, 4, 1, 5, 9, 2, 6))
    val sortedResizableArray = resizableArray.sorted(Ordering[Int])

    sortedResizableArray shouldBe ResizableArray.from(List(1, 1, 2, 3, 4, 5, 6, 9))
  }

  it should "return a subarray based on given indices" in {
    val resizableArray = ResizableArray.from(List(1, 2, 3, 4, 5))
    val subResizableArray = resizableArray.slice(1, 4)

    subResizableArray shouldBe ResizableArray.from(List(2, 3, 4))
  }
}
