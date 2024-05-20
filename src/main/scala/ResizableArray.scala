import scala.collection.{AbstractSeq, SeqFactory, StrictOptimizedSeqOps, mutable}
import scala.collection.mutable.{ArrayBuffer, Builder}

class ResizableArray[A] private(private val array: ArrayBuffer[A])
  extends AbstractSeq[A]
    with mutable.IndexedSeq[A]
    with mutable.IndexedSeqOps[A, mutable.IndexedSeq, ResizableArray[A]]
    with StrictOptimizedSeqOps[A, mutable.IndexedSeq, ResizableArray[A]] {

  override def length: Int = array.length

  override def apply(idx: Int): A = array(idx)

  override def update(idx: Int, elem: A): Unit = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem
  }

  def appended(elem: A): ResizableArray[A] = new ResizableArray(array :+ elem)

  def prepended(elem: A): ResizableArray[A] = new ResizableArray(elem +: array)

  def clear(): ResizableArray[A] = ResizableArray.empty

  def removeAt(idx: Int): ResizableArray[A] = {
    if (idx < 0 || idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    new ResizableArray(array.take(idx) ++ array.drop(idx + 1))
  }

  def concat(other: ResizableArray[A]): ResizableArray[A] = new ResizableArray(array ++ other.array)

  override def filter(predicate: A => Boolean): ResizableArray[A] = new ResizableArray(array.filter(predicate))

  def sorted(ordering: Ordering[A]): ResizableArray[A] = new ResizableArray(array.sorted(ordering))

  override def slice(from: Int, until: Int): ResizableArray[A] = new ResizableArray(array.slice(from, until))

  override def empty: ResizableArray[A] = ResizableArray.empty[A]

  override def iterableFactory: SeqFactory[ResizableArray] = ResizableArray

  override protected def fromSpecific(coll: IterableOnce[A]): ResizableArray[A] = ResizableArray.from(coll)

  override protected def newSpecificBuilder: mutable.Builder[A, ResizableArray[A]] = ResizableArray.newBuilder[A]
}

object ResizableArray extends SeqFactory[ResizableArray] {
  def empty[A]: ResizableArray[A] = new ResizableArray[A](new ArrayBuffer[A]())

  def from[A](it: IterableOnce[A]): ResizableArray[A] = new ResizableArray[A](ArrayBuffer.from(it))

  def newBuilder[A]: mutable.Builder[A, ResizableArray[A]] = ArrayBuffer.newBuilder[A].mapResult(new ResizableArray(_))
}
