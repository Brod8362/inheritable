package pw.byakuren.inheritable.collections

import pw.byakuren.inheritable.`trait`.Inheritable
import pw.byakuren.inheritable.exceptions._

class InheritableSeq[T](start: Seq[Inheritable[T]]) extends Inheritable[T] {

  def this() {
    this(Seq())
  }

  private var seq: Seq[Inheritable[T]] = start

  /**
   * Get the sequence of items that this object inherits from. If this is a single item, it should return an empty sequence.
   *
   * @return The sequence of items, or an empty sequence.
   */
  override def inheritanceTree: Seq[Inheritable[T]] = {
    seq
  }

  /**
   * Flatten out the tree by calling inheritanceTree() recursively, until all that remains is a Seq[T].
   *
   * @return The flattened tree containing only a sequence of T.
   */
  override def flatTree: Seq[T] = {
    var f: Seq[T] = Seq()
    for (elem <- inheritanceTree) {
      val e = elem.inheritanceTree
      if (e.isEmpty) {
        elem.unapply match {
          case Some(g) => f++=Seq(g)
          case None =>
            //this case should not happen, it means the element is either a sequence (meaning it should go to the 'else'
            //block just below this one) or that it is incorrectly implemented
            throw new ImpossibleInheritanceException("element failed to unapply")
        }
      }
      else f ++= elem.flatTree
    }
    f
  }

  /**
   * 'Unwrap' the inheritable to get the object contained within. In the case of a single object,
   * return Some(). If the object is a Sequence, or this method otherwise does not apply, return None.
   * @return Some() if the object can be unwrapped, None otherwise.
   */
  override def unapply: Option[T] = {
    None
  }

  /**
   * Append an item to the end of the Sequence.
   * @param e Inheritable Item to append
   */
  def append(e: Inheritable[T]): Unit = {
    seq=seq.appended(e)
  }

  /**
   * Append an item to the end of the Sequence. This will automatically wrap it in an InheritableElement.
   * @param t Item to append
   */
  def append(t: T): Unit = {
    append(Inheritable(t))
  }

  /**
   * Remove an item from the sequence at index i.
   * @param i Index to remove from
   * @return The object removed from index i.
   */
  def remove(i: Int): Inheritable[T] = {
    val r = seq(i)
    seq=seq.slice(0, i)++seq.slice(i+1, seq.size)
    r
  }

  /**
   * Number of items that this sequence directly inherits from.
   * @return number of items that this sequence directly inherits.
   */
  def size: Int = {
    seq.size
  }

  /**
   * Number of items in the full inheritance tree.
   * @return number of items in the flat tree
   */
  def trueSize: Int = {
    flatTree.size
  }

  /**
   * Prepend an element to the Sequence.
   * @param e Element to prepend
   */
  def prepend(e: Inheritable[T]): Unit = {
    seq=seq.prepended(e)
  }

  /**
   * Prepend an element of type [T] to the Sequence, automatically wrapping it in Inheritable()
   * @param t Element to prepend
   */
  def prepend(t: T): Unit = {
    prepend(Inheritable(t))
  }

  override def toString: String = {
    inheritanceTree.mkString("[", ",", "]")
  }
}
