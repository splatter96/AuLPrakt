package example

object Hello{
  abstract class MyList(){
    def length: Int
  }
  case class MyNil() extends MyList(){
    override def length: Int = 0
  }
  case class Concat(head: Int, tail: MyList) extends MyList(){
    override def length: Int = 1 + tail.length
  }

  abstract sealed class Btree {
    def contains(v: Int): Boolean
    def insert(v: Int): Btree
    def delete(v: Int): Btree
    def minVal(): Int
  }
  
  case object Empty extends Btree {
    def contains(v: Int) = false
    def insert(v: Int) = {
      Node(v, Empty, Empty)
    }
    def minVal() = 0 //TODO
    def delete(v: Int) = Empty
  }
  
  case class Node(value: Int, l: Btree, r: Btree) extends Btree {
    def contains(v: Int): Boolean = {
      if(v == value)
        true
       else if(v < value) l.contains(v)
       else r.contains(v)
    }

    def insert(v: Int) = {
      if(v < value)
          Node(value, l.insert(v), r)
      else
          Node(value, l, r.insert(v))
    }

    def minVal() = {
      l match {
        case Empty => value
        case _ => l.minVal()
      }
    }

    def delete(v: Int) = {
      if(v == value){
          //we are the node to delete
          (l, r) match {
            case (Empty, Empty) => Empty
            case (Empty, _) => r
            case (_, Empty) => l
            case (_, _) => {
              val minVal = r.minVal()
              Node(minVal, l, r.delete(minVal))
            }
          }
      } else if(v < value){
        Node(value, l.delete(v), r)
      } else{
        Node(value, l, r.delete(v))
      }
    }
  }




  def main(args: Array[String]): Unit = {
    // println(Concat(5, Concat(6, MyNil())).length)
    // println(Concat(5, Concat(6, MyNil())))
    val n1 = Node(1, Node(2, Empty, Empty), Empty)
    val n2 = n1.insert(3).insert(5).insert(4)

    val n3 = Node(5, Node(2, Node(-4, Empty, Empty), Node(3, Empty, Empty)),
             Node(12, Node(9, Empty, Empty), Node(21, Node(19, Empty, Empty), Node(25, Empty, Empty))))

    // println(n1)
    // println(n2)
    println(n3)
    // println(n2.delete(4))
    // println(n2.delete(5))
    // println(n2.delete(3))

    println(n3.delete(-4))
    println(n3.delete(12))

    println(n3.insert(13))
  }
}
