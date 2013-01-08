package models.mots

final class HitMap private(word : String) {
  private val hits : Array[Byte] = Array.fill(26)(0)

  for(c <- word.toLowerCase; i = HitMap.indexOf(c) if i >= 0) {
    hits(i) = (hits(i) + 1.toByte).asInstanceOf[Byte]
  }

  val isEmpty   : Boolean = hits.forall(_ == 0)
  val total     : Int = hits.sum // also known as length.
  val max       : Int = if(isEmpty) 0 else hits.max
  val min       : Int = if(isEmpty) 0 else hits.filter(_ > 0).min
  val canonical : String = ((0 until 26).toSeq.map { i =>
    ('a' + i).toChar.toString * hits(i)
  }).mkString("")

  def contains(hitMap : HitMap) : Boolean = {
    var i = 0
    while(i < 26) {
      if(hitMap.hits(i) > hits(i)) {
        return false;
      }
      i += 1
    }
    return true;
  }

}

case object HitMap {
  def forWord(word : String) : HitMap = {
    new HitMap(word)
  }

  private def indexOf(char : Char) : Int = char match {
    case 'á' | 'à' | 'â' | 'ä'     => ('a' - 'a').toInt
    case 'é' | 'è' | 'ê' | 'ë'     => ('e' - 'a').toInt 
    case 'í' | 'ì' | 'î' | 'ï'     => ('i' - 'a').toInt
    case 'ó' | 'ò' | 'ô' | 'ö'     => ('o' - 'a').toInt
    case 'ú' | 'ù' | 'û' | 'ü'     => ('u' - 'a').toInt
    case 'ý' | 'ỳ' | 'ŷ' | 'ÿ'     => ('y' - 'a').toInt
    case 'ç'                       => ('c' - 'a').toInt
    case c if c >= 'a' && c <= 'z' => (c - 'a').toInt
    case _ => -1
  }

  
}
