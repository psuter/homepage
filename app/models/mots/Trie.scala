package models.mots

import scala.collection.mutable.{Map=>MutableMap}

// Mutable trie, where keys are sequences of A, and values are B.
class Trie[A,B]() {
  private var value : Option[B] = None
  private var successors : MutableMap[A,Trie[A,B]] = MutableMap.empty

  def get(as : Seq[A]) : Option[B] = {
    if(as.isEmpty) {
      value
    } else {
      val s = successors.get(as.head)
      if(s.isEmpty) {
        None
      } else {
        s.get.get(as.tail)
      }
    }
  }

  def insert(as : Seq[A], b : B) {
    if(as.isEmpty) {
      value = Some(b)
    } else {
      val s = successors.get(as.head)
      val n = if(s.isEmpty) {
        val newS = new Trie[A,B]()
        successors(as.head) = newS
        newS
      } else {
        s.get
      }
      n.insert(as.tail, b)
    }
  }
}
