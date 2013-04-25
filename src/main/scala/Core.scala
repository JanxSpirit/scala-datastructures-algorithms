/*object Core {
  
  def removeDups = {
    val l = LList(1, 4, 7, 4, 9, 9, 4, 3, 1)
    
  }
  
}
*/
class LList[T](head: LNode[T]) {
//  def isEmpty = head.isEmpty
}

object LList {
  def apply(values: Int*) = 
    new LList[Int](values.reverse.foldLeft(NilNode())((a,b) => ListNode(Some(b), Some(a))))
}

case class ListNode[+T](value: Option[T], next: Option[LNode[T]]) extends LNode[T] {
  def +(value: T) = ListNode(Some(value), Some(this))
}

case class NilNode() extends LNode[Nothing] {
  val value = None
  val next = None
}

trait LNode[+T] {
  def value: Option[T]
  def next: Option[LNode[T]]
}

object Core {
  //determine whether a string is a substring of another
  def issubstr(str: String, find: String): Boolean = {
    if (str.size < 1 || find.size < 1) false
    if (find.size == 1) str(0) == find(0)
    else {
      if (find(0) == str(0)) issubstr(str.drop(1), find.drop(1))
      else issubstr(str.drop(1), find)
    }
  }

  def escSpaces(str: String): String =
    if (str.size < 1) str
    else {
      if (str(0) == ' ') "%20" + escSpaces(str.drop(1))
      else str(0) + escSpaces(str.drop(1))
    }
}
