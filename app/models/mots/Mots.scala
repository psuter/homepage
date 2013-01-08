package models.mots

import java.io.File
import java.io.FileInputStream
import java.util.Scanner

import play.Play

object Mots {
  private def normalize(str : String) : String = str.map(c => c match {
    case 'á' | 'à' | 'â' | 'ä' => 'a'
    case 'é' | 'è' | 'ê' | 'ë' => 'e'
    case 'í' | 'ì' | 'î' | 'ï' => 'i'
    case 'ó' | 'ò' | 'ô' | 'ö' => 'o'
    case 'ú' | 'ù' | 'û' | 'ü' => 'u'
    case 'ý' | 'ỳ' | 'ŷ' | 'ÿ' => 'y'
    case 'ç'                   => 'c'
    case _ => c
  })

  private case class Entry(
    word : String,
    normalized : String,
    palindrome : Boolean,
    frequency : Double,
    hitMap : HitMap
  ) {
    val length   = hitMap.total
    val distinct = (hitMap.max == 1)
  }
  
  private lazy val setAndMaxLength = build

  private lazy val entries : Seq[Entry] = setAndMaxLength._1.toSeq
  private lazy val entriesByWord = entries.sortBy(_.word)
  private lazy val entriesBySize = entries.sortBy(_.length)
  private lazy val entriesByFreq = entries.sortBy(e => -e.frequency)
  private lazy val entriesByAnag = entries.sortBy(_.hitMap.canonical)

  private def maxLength : Int = setAndMaxLength._2

  private def build : (Set[Entry],Int) = {
    val WordFreqSimple = """(\w*)\s(\d*\.\d*)$""".r
    val WordFreq       = """([\wáàâäéèêëíìîïóòôöúùûüýỳŷÿç]*)\s(\d*\.\d*)$""".r

    val list = Play.application.resourceAsStream("resources/liste_mots.txt")

    val scanner = new Scanner(list, "UTF-8")

    var s = Set[Entry]()

    var maxLength = 0
    var maxMin    = -1
    var maxMax    = -1

    def record(w : String, f : Double, needsNorm : Boolean) {
      val n = if(needsNorm) {
        normalize(w)
      } else {
        w
      }
      val isPalindrome = {
        var i = 0
        var possible = true
        val hl = n.length / 2
        val r = n.reverse
        while(possible && i < hl) {
          if(n(i) != r(i)) possible = false
          i += 1
        }
        possible
      }
      val hitMap = HitMap.forWord(w)

      val entry = Entry(w, n, isPalindrome, f, hitMap)

      if (entry.length > maxLength) {
        maxLength = entry.length
      }

      if (entry.hitMap.min > maxMin) {
        maxMin = entry.hitMap.min
      }

      if (entry.hitMap.max > maxMax) {
        maxMax = entry.hitMap.max
      }

      s += entry
    }

    while(scanner.hasNextLine()) {
      val line = scanner.nextLine()

      line match {
        case WordFreqSimple(w, f) =>
          record(w, f.toDouble, false)

        case WordFreq(w, f) =>
          record(w, f.toDouble, true)

        case elze => // println("Skipping " + elze)
      }
    }

    // println("Max length : " + maxLength)
    // println("Max max    : " + maxMax)
    // println("Max min    : " + maxMin)

    scanner.close()

    (s,maxLength)
  }

  // TODO : define Query case class...
  def query(minL : Option[Int], maxL : Option[Int], contains : Option[String], distinct : Boolean, palindrome : Boolean, ordering : Option[String]) : Seq[String] = {
    val hm = contains.map(c => HitMap.forWord(c))
    val emptyHM = contains.isEmpty || hm.get.isEmpty

    if(distinct && !emptyHM && hm.get.max > 1) {
      // println("Early kill")
      return Seq.empty
    }

    // Compute lower-bound for min based on other info, and drop if <= 1 anyway.
    val useMin : Option[Int] = minL flatMap { m =>
      val accountingForContains = hm match {
        case Some(h) => m max h.total
        case None    => m
      }

      if(accountingForContains <= 1) {
        None
      } else {
        Some(accountingForContains)
      }
    }

    // Drop upper-bound if it's maxLength anyway.
    val useMax : Option[Int] = maxL flatMap { m =>
      if(m >= maxLength) {
        None
      } else {
        Some(m)
      }
    }

    // println("m" + useMin)
    // println("M" + useMax)

    if(useMin.isDefined && useMax.isDefined && useMax.get < useMin.get) {
      // println("Early kill 2")
      return Seq.empty
    }

    def matches(e : Entry) : Boolean = {
      if(distinct && !e.distinct) return false
      if(palindrome && !e.palindrome) return false
      if(useMin.isDefined && e.length < useMin.get) return false
      if(useMax.isDefined && e.length > useMax.get) return false
      if(!emptyHM && !e.hitMap.contains(hm.get)) return false
      true
    }

    ordering match {
      case Some("a") => entriesByWord.view.filter(matches).map(_.word).force
      case Some("l") => entriesBySize.view.filter(matches).map(_.word).force
      case Some("f") => entriesByFreq.view.filter(matches).map(_.word).force
      case Some("c") => entriesByAnag.view.filter(matches).map(_.word).force
      case _         => entries.view.filter(matches).map(_.word).force
    }
  }
}
