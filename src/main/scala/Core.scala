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

/**
 * String questions
 */

  //determine whether a string is a substring of another
  def isSubstr(str: String, find: String): Boolean = {
    if (str.size < 1 || find.size < 1) false
    else if (find.size == 1) str(0) == find(0)
    else {
      if (find(0) == str(0)) isSubstr(str.drop(1), find.drop(1))
      else isSubstr(str.drop(1), find)
    }
  }

  def escSpaces(str: String): String =
    if (str.size < 1) str
    else {
      if (str(0) == ' ') "%20" + escSpaces(str.drop(1))
      else str(0) + escSpaces(str.drop(1))
    }

  def isAnagram(str1: String, str2: String): Boolean = {
    def charcount(s: String, a: Array[Int] = 
      Array.ofDim[Int](128)): Array[Int] = 
        if (s.size < 1) a
        else {
          a(s(0).toInt) += 1
          charcount(s.drop(1), a)
        }
    charcount(str1).corresponds(charcount(str2))((x, y) => x == y)
  }

  def isRotation(str1: String, str2: String): Boolean = 
    if (str1.size != str2.size) false
    else isSubstr(str1+str1, str2)

}
