package pw.byakuren.inheritable.collections

import pw.byakuren.inheritable.`trait`.Inheritable

class InheritableElement[T](obj: T) extends Inheritable[T] {

  /**
   * @inheritdoc
   */
  override def inheritanceTree: Seq[Inheritable[T]] = {
    Seq()
  }

  /**
   * @inheritdoc
   */
  override def flatTree: Seq[T] = {
    Seq(obj)
  }

  override def toString: String = {
    obj.toString
  }
}
