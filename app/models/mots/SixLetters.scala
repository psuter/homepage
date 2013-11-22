package models.mots

object SixLetters {
  import Mots.Entry

  private lazy val slws : Set[Entry] = Mots.setAndMaxLength._1.filter { entry =>
    entry.length == 6
  }

  private def addToMapSet[A,B](m : Map[A,Set[B]], a : A, b : B) : Map[A,Set[B]] = {
    if(!m.isDefinedAt(a)) {
      m + (a -> Set(b))
    } else {
      m + (a -> (m(a) + b))
    }
  }

  private def sixRoots(word : String) : Set[String] = {
    val rs = for(i <- 0 until 6) yield {
      (word.substring(0, i) + word.substring(i+1, 6)).sorted
    }
    rs.toSet
  }

  private lazy val maps = {
    var m : Map[String,Set[String]] = Map.empty

    var representations : Map[String,Set[String]] = Map.empty

    for(entry <- slws) {
      representations = addToMapSet(representations, entry.normalized, entry.word) 
    }

    for(r <- representations.keySet; s <- sixRoots(r)) {
      m = addToMapSet(m, s, r) 
    }

    (m, representations)
  }

  private lazy val rootToWords : Map[String,Set[String]] = maps._1
  private lazy val wordRepresentations : Map[String,Set[String]] = maps._2

  def lookup(optWord : Option[String]) : Either[String,Set[String]] = optWord match {
    case None => Left("Please specify a word.")
    case Some(w) if w.length != 6 => Left("Wrong word length (not 6).")
    case Some(w) =>
      val word = Mots.normalize(w)
      val resSets : Seq[Set[String]] = (for(s <- sixRoots(word)) yield {
        rootToWords.getOrElse(s, Set.empty[String])
      }).toSeq

      val union : Set[String] = resSets.reduceLeft(_ ++ _)
      val inter : Set[String] = resSets.reduceLeft(_ intersect _)

      val resSet = union -- inter
      val resWords = resSet.flatMap(w => wordRepresentations.getOrElse(w, Set.empty))

      Right(resWords)
  }
}
