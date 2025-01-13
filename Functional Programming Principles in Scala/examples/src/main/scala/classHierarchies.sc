abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
end IntSet

object IntSet:
  def singleton(x: Int): IntSet = NonEmpty(x, Empty, Empty)
end IntSet

// Sets as BinaryTrees
object Empty extends IntSet:
  override def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  override def contains(x: Int): Boolean = false
  override def union(other: IntSet): IntSet = other

class NonEmpty(e: Int, left: IntSet, right: IntSet) extends IntSet:
  override def incl(x: Int): IntSet =
    if x < e then NonEmpty(e, left.incl(x), right)
    else if x > e then NonEmpty(e, left, right.incl(x)) 
    else this

  override def contains(x: Int): Boolean =
    if x == e then true
    else if left.contains(x) then true
    else right.contains(x)

  override def union(other: IntSet): IntSet =
    left.union(right).union(other).incl(e)


//Invocation of companion object
IntSet.singleton(5)






