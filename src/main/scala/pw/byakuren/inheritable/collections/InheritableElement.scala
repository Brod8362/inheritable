package pw.byakuren.inheritable.collections

import pw.byakuren.inheritable.`trait`.Inheritable

class InheritableElement[T](obj: T) extends Inheritable[T] {
  /**
   * Get the sequence of items that this object inherits from. If this is a single item, it should return an empty sequence.
   *
   * @return The sequence of items, or an empty sequence.
   */
  override def inheritanceTree: Seq[Inheritable[T]] = {
    Seq()
  }

  /**
   * Flatten out the tree by calling inheritanceTree() recursively, until all that remains is a Sequence of T.
   *
   * @return The flattened tree containing only a sequence of T.
   */
  override def flatTree: Seq[T] = {
    Seq()
  }

  /**
   * 'Unwrap' the inheritable to get the object contained within. In the case of a single object,
   * return Some(). If the object is a Sequence, or this method otherwise does not apply, return None.
   * @return Some() if the object can be unwrapped, None otherwise.
   */
  override def unapply: Option[T] = {
    Some(obj)
  }

  override def toString: String = {
    obj.toString
  }
}
