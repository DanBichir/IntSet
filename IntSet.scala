// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = null // or however empty set is represented
  private var vsize : Int = 0


  /** Convert the set to a string.
    * (The given implementation is not sufficient.)
    * Complexity: O(n) */
  override def toString : String ={
    var position = theSet.next
    if(position == null) return "{}"
    var set = "{"
    if(position != null){
      set += (position.datum).toString
      position = position.next
    }
    while(position != null){
      set += ","
      set += (position.datum).toString
      position = position.next
    }
    set += "}"
    return set
  }

  /** Find the greatest element smaller or equal to e */
  def find(e : Int) : Node ={
    var position = theSet
    while(position.next != null && (position.next).datum <= e)
      position = position.next
    position
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) = {
    if(vsize==0) {theSet.next = Node(e,null); vsize += 1}
    else {
      var position = find(e)
      if(position.datum != e){
        position.next = Node(e,position.next)
        vsize+=1
      }
    }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S
    * Complexity: O(1) */
  def size : Int = vsize

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S)
    * Complexity: O(1) */
  def contains(e: Int) : Boolean = {
    if(vsize==0) return false
    else         return ((find(e)).datum == e)
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S
    * Complexity: O(1) */
  def any : Int = (theSet.next).datum

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S
    * Complexity: O(n) */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
      if(s.vsize != vsize) return false
      else{
        var pos1 = s.theSet
        var pos2 = theSet
        while(pos1 != null){
          if(pos1.datum != pos2.datum)
            return false
          pos1 = pos1.next
          pos2 = pos2.next
        }
        return true
      }
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    if(contains(e)){
      var position = find(e)
      position = position.next
      vsize -= 1
      return true
    }
    else
      return false
  }

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S
    * Complexity: O(n) */
  def subsetOf(that: IntSet) : Boolean = {
    var pos1 = theSet         /* position in this */
    var pos2 = that.theSet    /* position in that */
    while(pos1 != null){
      while(pos1.datum>pos2.datum){
        pos2=pos2.next
        if(pos2 == null) return false
      }
      if(pos1 != pos2)   return false
    }
    return true
  }


  // ----- optional parts below here -----

  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = ???

  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = ???

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = ???

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = ???
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined
    * the main constructor and the add operation.
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}
