import org.scalatest.flatspec.AnyFlatSpec
import pw.byakuren.inheritable.collections.{InheritableElement, InheritableSeq}

class InheritanceSpec extends AnyFlatSpec{
  "An Inheritable Sequence" should "report size and true size of 0 when empty" in {
    val testseq = new InheritableSeq[Int]
    assert(testseq.size==0)
    assert(testseq.trueSize==0)
  }
  it should "add items without an exception" in {
    val testseq = new InheritableSeq[Int]
    testseq.append(new InheritableElement[Int](5))
  }
  it should "report size and true size of 1 when one standard element is added" in {
    val testseq = new InheritableSeq[Int]
    testseq.append(new InheritableElement[Int](5))
    assert(testseq.size==1)
    assert(testseq.trueSize==1)
  }
  it should "report a size of 1 and a true size of 2 when a sequence containing two elements is added" in {
    val testa = new InheritableSeq[Int]()
    val testb = new InheritableSeq[Int]()
    testb.append(new InheritableElement[Int](1))
    testb.append(new InheritableElement[Int](2))
    testa.append(testb)
    assert(testa.size==1)
    assert(testb.size==2)
    assert(testa.trueSize==2)
  }
  it should "report elements in the correct order after being added" in {
    val test = new InheritableSeq[Int](Seq(new InheritableElement[Int](1), new InheritableElement[Int](2)))
    assert(test.flatTree == Seq(1,2))
  }
}
