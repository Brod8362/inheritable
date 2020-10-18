package pw.byakuren.inheritable.`trait`

import pw.byakuren.inheritable.collections.{InheritableElement, InheritableSeq}

/**
 * A class that can be inherited by another object, and may inherit objects itself/
 *
 * @tparam T Type contained within this Inheritable
 */
trait Inheritable[T] {

  /**
   * Test if this object inherits the other in any depth.
   *
   * @param o Object to check inheritance with.
   */
  def inherits(o: Inheritable[T]): Boolean = {
    inheritanceTree.contains(o) || inheritanceTree.exists(_.inherits(o))
  }

  /**
   * Test if this object is a direct descendant of the other object.
   *
   * @param o 'Parent' object to test inheritance with.
   */
  def directlyInherits(o: Inheritable[T]): Boolean = {
    inheritanceTree.contains(o)
  }

  /**
   * Get the sequence of items that this object inherits from. If this is a single item, it should return an empty sequence.
   *
   * @return The sequence of items, or an empty sequence.
   */
  def inheritanceTree: Seq[Inheritable[T]]

  /**
   * Flatten out the tree, until all that remains is a Sequence of T.
   *
   * @return The flattened tree containing only a sequence of T.
   */
  def flatTree: Seq[T]
}

object Inheritable {
  def apply[T](o: T): InheritableElement[T] = {
    new InheritableElement[T](o)
  }
  def apply[T](o: Seq[T]): InheritableSeq[T] = {
    new InheritableSeq[T](o.map(Inheritable(_)))
  }
}
