package pw.byakuren.inheritable.collections

import pw.byakuren.inheritable.`trait`.Inheritable

class InheritableSeq[T](start: Seq[Inheritable[T]]) extends Inheritable[T] {

  def this() {
    this(Seq())
  }

  private var seq: Seq[Inheritable[T]] = start

  /**
   * @inheritdoc
   */
  override def inheritanceTree: Seq[Inheritable[T]] = {
    seq
  }

  /**
   * @inheritdoc
   */
  override def flatTree: Seq[T] = inheritanceTree.flatMap(_.flatTree)

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
