import org.scalatest.flatspec.AnyFlatSpec
import pw.byakuren.inheritable.`trait`.Inheritable
import pw.byakuren.inheritable.collections.InheritableSeq

class InheritanceSpec extends AnyFlatSpec {
  "An Inheritable Sequence" should "report size and true size of 0 when empty" in {
    val testseq = new InheritableSeq[Int]
    assert(testseq.size == 0)
    assert(testseq.trueSize == 0)
  }
  it should "add items without an exception" in {
    val testseq = new InheritableSeq[Int]
    testseq.append(Inheritable(5))
  }
  it should "report size and true size of 1 when one standard element is added" in {
    val testseq = new InheritableSeq[Int]
    testseq.append(Inheritable(5))
    assert(testseq.size == 1)
    assert(testseq.trueSize == 1)
  }
  it should "report a size of 1 and a true size of 2 when a sequence containing two elements is added" in {
    val testa = new InheritableSeq[Int]()
    val testb = new InheritableSeq[Int]()
    testb.append(Inheritable(1))
    testb.append(Inheritable(2))
    testa.append(testb)
    assert(testa.size == 1)
    assert(testb.size == 2)
    assert(testa.trueSize == 2)
  }
  it should "report elements in the correct order after being added" in {
    val test = new InheritableSeq[Int](Seq(Inheritable(1), Inheritable(2)))
    assert(test.flatTree == Seq(1, 2))
  }
  it should "remove elements correctly" in {
    val test = new InheritableSeq[Int](Seq(Inheritable(1), Inheritable(2), Inheritable(3)))
    test.remove(1)
    assert(test.flatTree == Seq(1, 3))
    test.remove(1)
    assert(test.flatTree == Seq(1))
    test.remove(0)
    assert(test.flatTree == Seq())
  }
  it should "correctly generate a toString" in {
    val test = new InheritableSeq[Int](Seq(Inheritable(1), Inheritable(2), Inheritable(3)))
    assert(test.toString == "[1,2,3]")
    val test2 = new InheritableSeq[Int](Seq(Inheritable(4)))
    test.append(test2)
    assert(test.toString == "[1,2,3,[4]]")
    test2.append(Inheritable(5))
    assert(test.toString == "[1,2,3,[4,5]]")
    assert(test2.toString == "[4,5]")
  }
  it should "correctly determine if an object inherits it" in {
    val four = Inheritable(4)
    val test = new InheritableSeq[Int](Seq(four, Inheritable(5)))
    val test2 = new InheritableSeq[Int](Seq(Inheritable(1), Inheritable(2), Inheritable(3)))
    test2.append(test)
    assert(test2.directlyInherits(test))
    assert(test2.inherits(test))

    assert(!test.inherits(test2))
    assert(!test.directlyInherits(test2))

    assert(test.directlyInherits(four))
    assert(test.inherits(four))

    assert(!test2.directlyInherits(four))
    assert(test2.inherits(four))
  }
  it should "respond accordingly when components are updated" in {
    val one = Inheritable(1)
    val two = Inheritable(2)
    val three = Inheritable(3)
    val test = new InheritableSeq[Int](Seq(one, two))
    val test2 = new InheritableSeq[Int](Seq(three, Inheritable(4)))
    assert(test.flatTree==Seq(1,2))
    assert(test2.flatTree==Seq(3,4))
    test.append(test2)
    assert(test.flatTree==Seq(1,2,3,4))
    assert(test.inheritanceTree==Seq(one, two, test2))
    test2.remove(1)
    assert(test.inheritanceTree==Seq(one, two, test2))
    assert(test.flatTree==Seq(1,2,3))
    assert(test.inherits(three))
  }
}
