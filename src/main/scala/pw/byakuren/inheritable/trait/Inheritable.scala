package pw.byakuren.inheritable.`trait`

import pw.byakuren.inheritable.collections.InheritableElement

/**
 * A class that can be inherited by another object, and may inherit objects itself/
 *
 * @tparam T
 */
trait Inheritable[T] {

  /**
   * Test if this object inherits the other in any depth.
   *
   * @param o Object to check inheritance with.
   */
  def inherits(o: Inheritable[T]): Boolean = {
    flatTree.contains(o)
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
   * Flatten out the tree by calling inheritanceTree() recursively, until all that remains is a Sequence of T.
   *
   * @return The flattened tree containing only a sequence of T.
   */
  def flatTree: Seq[T]

  /**
   *
   * @return
   */
  def unapply: Option[T]

}

object Inheritable {
  def apply[T](o: T): InheritableElement[T] = {
    new InheritableElement[T](o)
  }
}
