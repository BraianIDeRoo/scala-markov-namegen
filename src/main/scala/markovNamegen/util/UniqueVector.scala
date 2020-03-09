package markovNamegen.util

import scala.collection.immutable.StrictOptimizedSeqOps
import scala.collection.{ immutable, mutable, AbstractIterator, IterableFactory, IterableFactoryDefaults, IterableOps }

final class UniqueVector[+A] private (val length: Int, elems: Array[Any])
    extends immutable.Iterable[A]
    with IterableOps[A, UniqueVector, UniqueVector[A]]
    with IterableFactoryDefaults[A, UniqueVector]
    with StrictOptimizedSeqOps[A, UniqueVector, UniqueVector[A]] { self =>

  def this(length: Int) =
    this(length, Array.ofDim(length))

  override def appended[B >: A](elem: B): UniqueVector[B] =
    if (elems.contains(elem)) {
      val newElems = Array.ofDim[Any](length)
      Array.copy(elems, 0, newElems, 0, length)
      new UniqueVector[B](length, newElems)
    } else {
      val newElems = Array.ofDim[Any](length + 1)
      Array.copy(elems, 0, newElems, 0, length)
      newElems(length) = elem
      new UniqueVector[B](length + 1, newElems)
    }

  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private var current           = 0
    override def hasNext: Boolean = current < self.length

    override def next(): A = {
      val elem = self(current)
      current += 1
      elem
    }
  }
  def apply(i: Int): A = elems(i).asInstanceOf[A]

  override val iterableFactory: IterableFactory[UniqueVector] = UniqueVector
}

object UniqueVector extends IterableFactory[UniqueVector] {
  override def from[A](source: IterableOnce[A]): UniqueVector[A] =
    (newBuilder[A] ++= source).result()

  override def empty[A]: UniqueVector[A] = new UniqueVector[A](0)

  override def newBuilder[A]: mutable.Builder[A, UniqueVector[A]] =
    new mutable.ImmutableBuilder[A, UniqueVector[A]](empty) {
      override def addOne(elem: A): this.type = { elems = elems :+ elem; this }
    }
}
