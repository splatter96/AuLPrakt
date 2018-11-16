package example

object Prakt2{

  abstract sealed class Heap {
    def s(): Int
    def isLeftTree(): Boolean
    def put(n: Heap): Heap
    def merge(h: Heap): Heap
    def removeMin(): Heap
  }
  
  case object Empty extends Heap {
    def s() = 0
    def isLeftTree() = true
    def merge(h: Heap) = h
    def put(h: Heap) = h //Node(h.value, Empty, Empty)
    def removeMin() = Empty
  }
  
  case class Node(value: Int, l: Heap, r: Heap) extends Heap {
    def s() = {
        (l.s() + 1).min(r.s() + 1)
    }

    def isLeftTree() = {
        l.s() >= r.s() && l.isLeftTree() && r.isLeftTree()
    }

    def merge(h: Heap): Heap = {
        h match {
            case Empty => Node(value, l, r)
            case Node(hv, hl, hr) => {
                // get heap with smaller root value
                val n = if(value < hv) Node(value, l, r) else Node(hv, hl, hr)
                // get heap with bigger root value
                val m = if(value >= hv) Node(value, l, r) else Node(hv, hl, hr)

                n.r match {
                    case Empty => {
                            if(Node(n.value, n.l, m).isLeftTree()){
                                Node(n.value, n.l, m)
                            }else{
                                // if resulting heap is not a leftTree
                                // swap subtrees
                                Node(n.value, m, n.l)
                            }
                    }
                    case _ => Node(n.value, n.l, n.r.merge(m))
                }
            }
        }
    }

    def put(h: Heap) = {
        Node(value, l, r).merge(h)
    }

    def removeMin() = {
        l.merge(r)
    }

  }

  def main(args: Array[String]): Unit = {
      println("Pratk2")

      println("Node 1")
      val n1 = Node(2, Empty, Empty)
      println(n1.s())
      println(n1.isLeftTree())

      println("Node 2")
      val n2 = Node(2, Node(1, Empty, Empty), Node(1, Empty, Empty))
      println(n2.s())
      println(n2.isLeftTree())

      println("Node 3")
      val n3 = Node(2, Empty, Node(1, Empty, Empty))
      println(n3.s())
      println(n3.isLeftTree())

      println("Merge 1")
      val n4 = Node(3, Empty, Empty)
      println(n1.merge(n4))

      println("Merge 2")
      val n5 = Node(2, Node(4, Empty, Empty), Empty)
      val n6 = Node(5, Node(6, Empty, Empty), Empty)
      println(n5.merge(n6))

      println("Merge 3")
      val n7 = Node(1, Node(3, Node(7, Empty, Empty), Node(8, Empty, Empty)), Node(6, Empty, Empty))
      val n8 = Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty))
      println(n7.merge(n8))

      println("Put 1")
      val n9 = Node(2, Empty, Empty)
      println(n9.put(Node(3, Empty, Empty)))

      println("Put 2")
      val n10 = Node(2, Empty, Empty)
      println(n10.put(Node(1, Empty, Empty)))
  }

}