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
   * Flatten out the tree by calling inheritanceTree() recursively, until all that remains is a Sequence of T.
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
            //block just below this one) it that it is incorrectly implemented
            throw new ImpossibleInheritanceException("element failed to unapply")
        }
      }
      else f ++= elem.flatTree
    }
    f
  }

  override def unapply: Option[T] = {
    None
  }

  def append(e: Inheritable[T]): Unit = {
    seq=seq.prepended(e)
  }

  def remove(i: Int): Unit = {
    seq=seq.slice(0, i)++seq.slice(i+1, seq.size)
  }

  def size: Int = {
    seq.size
  }

  def trueSize: Int = {
    flatTree.size
  }

  def prepend(e: Inheritable[T]): Unit = {
    seq=seq.prepended(e)
  }
}
